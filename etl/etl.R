# This script is meant to run in a Docker container.
# It extracts bike counter data from the Halifax open data portal, and weather
#  data from the government of Canada.
# It authorizes access to BigQuery and then updates the
#  `bike_counts.daily_counts` and `weather.daily_report` tables.

library(bigrquery)
library(tidyverse)
library(httr)
library(jsonlite)

# Get bike counter data ---------------------------------------------------
# See write-up here: https://tdunn.ca/posts/2022-04-27-predicting-bike-ridership-getting-the-data/#getting-bicycle-count-data

hfx_bike_url <- "https://services2.arcgis.com/11XBiaBYA9Ep0yNJ/arcgis/rest/services/Bicycle_Counts/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"

get_bike_counts <- function(offset) {
  # Need to prevent scientific notation, e.g. "1e+05" instead of "100000"
  offset <- format(offset, scientific = FALSE)

  content_parsed <- GET(paste0(hfx_bike_url, "&resultOffset=", offset)) %>%
    content(as = "parsed")

  map_dfr(
    content_parsed$features,
    ~ as_tibble(.x$attributes)
  )
}

n_records <- GET(paste0(hfx_bike_url, "&returnCountOnly=true")) %>%
  content(as = "text") %>%
  fromJSON() %>%
  as.numeric()

message("Number of bike counter records to collect: ", n_records)
message("Downloading bike counter data")
bike_counts <- map_dfr(
  seq(0, ceiling(n_records / 2000)),
  ~ get_bike_counts(offset = .x * 2000)
)

bike_counts_daily <- bike_counts %>%
  # Timestamps are in Unix time (milliseconds since Jan 1, 1970)
  mutate(count_datetime = as.POSIXct(COUNT_DATETIME / 1000,
                                     tz = "UTC", origin = "1970-01-01")) %>%
  group_by(site_name = SITE_NAME, count_date = as.Date(count_datetime)) %>%
  summarise(n_records = n(), n_bikes = sum(COUNTER_VALUE),
            .groups = "drop") %>%
  filter(
    # Only keep days with a full day of records (24 on Hollis St, 48 others)
    ifelse(site_name == "Hollis St", n_records == 24, n_records == 48)
  ) %>%
  arrange(site_name, count_date) %>%
  group_by(site_name) %>%
  mutate(n_bikes_lag_14 = lag(n_bikes, 14)) %>%
  ungroup() %>%
  arrange(count_date, site_name)

message("Max bike counter date: ", max(bike_counts_daily$count_date))

# Get weather data --------------------------------------------------------
# See write-up here: https://tdunn.ca/posts/2022-04-27-predicting-bike-ridership-getting-the-data/#getting-weather-data

weather_base_url <-
  "https://api.weather.gc.ca/collections/climate-daily/items?f=json&lang=en-CA"
get_daily_climate_report_year <- function(
  station_name, local_year, limit = 10000
) {
  weather_query <- paste(
    c(weather_base_url,
      paste0("limit=", limit),
      paste0("STATION_NAME=", URLencode(station_name)),
      paste0("LOCAL_YEAR=", local_year)),
    collapse = "&"
  )
  content_parsed <- GET(weather_query) %>% content(as = "parsed")

  map_dfr(
    content_parsed$features,
    ~ discard(.x$properties, is.null) %>% as_tibble()
  )
}

bike_count_years <- unique(format(bike_counts_daily$count_date, "%Y"))

message("Downloading weather data")
climate_report_windsor <-
  map2_dfr(
    "HALIFAX WINDSOR PARK", bike_count_years,
    get_daily_climate_report_year
  )
# Especially in 2022, the Windsor St climate station is missing lots of data.
#  To supplement it, a more reliable station is the airport.
#  It is ~30 minutes outside the city, so the weather isn't exactly the same
#  but should give reasonable imputation of missing values
climate_report_airport <-
  map2_dfr(
    "HALIFAX STANFIELD INT'L A", bike_count_years,
    get_daily_climate_report_year
  ) %>%
  # The airport has a few different identifiers, this one seems to have the
  #  most non-missing data
  filter(CLIMATE_IDENTIFIER == "8202251")

climate_report_daily <- climate_report_windsor %>%
  transmute(
    report_date = as.POSIXct(LOCAL_DATE) %>% as.Date(),
    mean_temperature = MEAN_TEMPERATURE,
    total_precipitation = TOTAL_PRECIPITATION,
    snow_on_ground = SNOW_ON_GROUND,
    speed_max_gust = SPEED_MAX_GUST
  ) %>%
  full_join(
    climate_report_airport %>%
      transmute(
        report_date = as.POSIXct(LOCAL_DATE) %>% as.Date(),
        mean_temperature_airport = MEAN_TEMPERATURE,
        total_precipitation_airport = TOTAL_PRECIPITATION,
        snow_on_ground_airport = SNOW_ON_GROUND,
        speed_max_gust_airport = SPEED_MAX_GUST,
      ),
    by = "report_date"
  ) %>%
  arrange(report_date)

message("Max weather date: ", max(climate_report_daily$report_date))

# Upload data -------------------------------------------------------------

# Authorize to view and manage BigQuery projects
bq_auth("/home/rstudio/oauth-client.json",
        email = "hfx-bike-ridership@hfx-bike-ridership.iam.gserviceaccount.com")

# Define the project, dataset and a new table for this project
project <- "hfx-bike-ridership"

daily_counts_table <- bq_table(project, "bike_counts", "daily_counts")
weather_table <- bq_table(project, "weather", "daily_report")

bq_table_upload(daily_counts_table,
                value = bike_counts_daily,
                fields = bike_counts_daily,
                # If table doesn't exist, create it
                create_disposition = "CREATE_IF_NEEDED",
                # If table exists, overwrite it
                write_disposition = "WRITE_TRUNCATE")

bq_table_upload(weather_table,
                value = climate_report_daily,
                fields = climate_report_daily,
                create_disposition = "CREATE_IF_NEEDED",
                write_disposition = "WRITE_TRUNCATE")

message("Finished ETL pipeline")
