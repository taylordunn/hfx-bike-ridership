# This script is meant to run in a Docker container.
# It downloads the prepared bike counter and weather data from BigQuery,
#  trains an XGBoost model, turns it into a `plumber` API,
#  and writes it to a Cloud Storage bucket

library(bigrquery)
library(tidyverse)
library(tidymodels)

# Testing plumber, pins and vetiver ---------------------------------------

library(vetiver)
library(pins)
tmp_plumber <- "model/example-plumber.R"
b <- board_temp(versioned = TRUE)
cars_lm <- lm(mpg ~ ., data = mtcars)
v <- vetiver_model(cars_lm, "cars_linear")
vetiver_pin_write(b, v)
#> Creating new version '20220428T153651Z-88e0b'
#> Writing to pin 'cars_linear'
vetiver_write_plumber(b, "cars_linear", file = tmp_plumber)

## default port
vetiver_write_docker(v, tmp_plumber, tempdir())
#> * Lockfile written to '/tmp/RtmpPkEsgU/renv.lock'.
## port from env variable
vetiver_write_docker(v, tmp_plumber, #tempdir(),
                     port = 'as.numeric(Sys.getenv("PORT"))')

# Read data ---------------------------------------------------

#bq_auth("/home/rstudio/hfx-bike-ridership.json", email = "t.dunn19@gmail.com")
bq_auth("hfx-bike-ridership.json", email = "t.dunn19@gmail.com")

# Define the project, dataset and a new table for this project
project <- "hfx-bike-ridership"

daily_counts_table <- bq_table(project, "bike_counts", "daily_counts")
bike_data <- bq_table_download(daily_counts_table)

weather_table <- bq_table(project, "weather", "daily_report")
weather_data <- bq_table_download(weather_table)


# Pre-process -------------------------------------------------------------

#weather_data <- weather_data %>%
d <- weather_data %>%
  transmute(
    report_date,
    # Where the primary data is missing (from the Windsor Park station),
    #  impute with the value reported at the Halifax airport (if not missing)
    mean_temperature = ifelse(
      is.na(mean_temperature), mean_temperature_airport,
      mean_temperature
    ),
    total_precipitation = ifelse(
      is.na(total_precipitation),
      total_precipitation_airport, total_precipitation
    ),
    snow_on_ground = ifelse(
      is.na(snow_on_ground),
      snow_on_ground_airport, snow_on_ground
    )
  )

bike_weather <- bike_counts_daily %>%
  left_join(weather_report, )
