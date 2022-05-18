library(plumber)
library(dplyr)
library(tidymodels)
library(bigrquery)
library(googleCloudStorageR)

bq_auth(path = "oauth-client.json")
gcs_auth("oauth-client.json")

project <- "hfx-bike-ridership"

daily_counts_table <- bq_table(project, "bike_counts", "daily_counts")
bike_data <- bq_table_download(daily_counts_table)

xgb_fit <- gcs_get_object("xgb-fit.rds", bucket = "hfx-bike-ridership-model",
                          parseFunction = gcs_parse_rds)

#* Predict bike ridership in Halifax, NS
#*
#* @param count_date:str The date in YYYY-MM-DD format.
#* @param site_name:str The location of the bike counter. One of "Dartmouth Harbourfront Greenway", "Hollis St", "South Park St", "Vernon St", "Windsor St".
#* @param n_bikes_lag_14:int The number of bikes measured at the given `site_name` 14 days ago. If not provided, will attempt to impute with the actual value 14 days before `count_date`.
#* @param mean_temperature:numeric The daily mean temperature. If not provided, will impute with the rolling mean.
#* @param total_precipitation:numeric The daily amount of precipitation in mm. If not provided, will impute with zero.
#* @param snow_on_ground:numeric The daily amount of snow on the ground in cm. If not provided, will impute with zero.
#* @param speed_max_gust:numeric The daily maximum wind speed in km/h. If not provided, will impute with the mean in the training set.
#* @get /n_bikes
function(count_date, site_name, n_bikes_lag_14 = NA_integer_,
         mean_temperature = NA_real_, total_precipitation = NA_real_,
         snow_on_ground = NA_real_, speed_max_gust = NA_real_) {

  site_name <- match.arg(
    site_name,
    choices = c("Dartmouth Harbourfront Greenway", "Hollis St",
                "South Park St", "Vernon St", "Windsor St"),
    several.ok = FALSE
  )

  count_date <- as.Date(count_date)
  if (is.na(n_bikes_lag_14)) {
    message("Imputing `n_bikes_lag_14`")
    bike_counts <- bike_data %>% filter(site_name == .env$site_name)

    n_bikes_lag_14 <- bike_counts %>%
      filter(count_date == .env$count_date - 14) %>%
      pull(n_bikes)

    if (length(n_bikes_lag_14) == 0) {
      stop("Could not find a `n_bikes_lag_14` value for site ", site_name,
           " on ", count_date, ". ",
           "Please provide your own `n_bikes_lag_14`, or choose a different `count_date` between ",
           min(bike_counts$count_date) + 14, " and ",
           max(bike_counts$count_date) + 14)
    }
  }

  d <- tibble(
    count_date = count_date, site_name = site_name,
    n_bikes_lag_14 = as.numeric(n_bikes_lag_14),
    mean_temperature = as.numeric(mean_temperature),
    total_precipitation = as.numeric(total_precipitation),
    snow_on_ground = as.numeric(snow_on_ground),
    speed_max_gust = as.numeric(speed_max_gust)
  )

  predict(xgb_fit, d, type = "raw")
}
