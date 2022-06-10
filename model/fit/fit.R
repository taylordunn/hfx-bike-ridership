library(dplyr)
library(readr)
library(tidymodels)
library(bigrquery)
library(googleCloudStorageR)
library(googleCloudRunner)
library(plumber)
source("preprocess.R")

bq_auth(path = "oauth-client.json")
gcs_auth("oauth-client.json")
gcs_upload_set_limit(20000000L) # 20 Mb

# This function will retrieve the latest data from BigQuery, the trained
#  model from GCS, and fit an XGBoost model, which is saved to GCS
pub <- function(message) {
  # Define the project, dataset and a new table for this project
  project <- "hfx-bike-ridership"

  daily_counts_table <- bq_table(project, "bike_counts", "daily_counts")
  bike_data <- bq_table_download(daily_counts_table)
  bike_data_updated <- bq_table_meta(daily_counts_table)$lastModifiedTime %>%
    as.numeric() %>%
    {as.POSIXct(. / 1000, origin = "1970-01-01")}


  weather_table <- bq_table(project, "weather", "daily_report")
  weather_data <- bq_table_download(weather_table)
  weather_data_updated <- bq_table_meta(weather_table)$lastModifiedTime %>%
    as.numeric() %>%
    {as.POSIXct(. / 1000, origin = "1970-01-01")}

  bike_data <- preprocess(bike_data, weather_data)
  xgb_tuned <- gcs_get_object("tune/xgb-model-tuned.rds",
                              bucket = "hfx-bike-ridership-model",
                              parseFunction = gcs_parse_rds)

  message("Writing updating xgb-fit")
  xgb_fit <- list(
    tune_timestamp = xgb_tuned$timestamp,
    timestamp = Sys.time(),
    bike_data_updated = bike_data_updated,
    weather_data_updated = weather_data_updated,
    bike_xgb_fit = fit(xgb_tuned$bike_xgb_fit, bike_data)
  )

  f <- function(input, output) write_rds(input, output)
  metadata <- gcs_upload(xgb_fit, name = "xgb-fit.rds",
                         bucket = "hfx-bike-ridership-model",
                         object_function = f)

  return(TRUE)
}

#' Receive pub/sub message
#' @post /pubsub
#' @param message a pub/sub message
function(message = NULL) {
  message("Received message ", message)
  googleCloudRunner::cr_plumber_pubsub(message, pub)
}
