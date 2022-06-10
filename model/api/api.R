#* @apiTitle Predict bike ridership in Halifax, NS
#* @apiDescription This API serves predictions for the daily number of bicyclists passing particular sites around Halifax, Nova Scotia. For more information, check out the [source code](https://github.com/taylordunn/hfx-bike-ridership) and [this write-up](https://tdunn.ca/posts/2022-05-19-predicting-bike-ridership-deploying-the-model/). You can also read more about [the data here](https://tdunn.ca/posts/2022-04-27-predicting-bike-ridership-getting-the-data/), and [the model development here](https://tdunn.ca/posts/2022-04-29-predicting-bike-ridership-developing-a-model/).
#* @apiContact list(name = "Taylor Dunn", url = "http://www.tdunn.ca", email = "t.dunn19@gmail.com")
#* @apiVersion 1.0

library(plumber)
library(dplyr)
library(tidymodels)
library(bigrquery)
library(googleCloudStorageR)

bq_auth(path = "oauth-client.json",
        email = "hfx-bike-ridership@hfx-bike-ridership.iam.gserviceaccount.com")
gcs_auth("oauth-client.json")

project <- "hfx-bike-ridership"

message("Reading data")
daily_counts_table <- bq_table(project, "bike_counts", "daily_counts")
bike_data <- bq_table_download(daily_counts_table)

message("Loading model")
xgb_fit <- gcs_get_object("xgb-fit.rds", bucket = "hfx-bike-ridership-model",
                          parseFunction = gcs_parse_rds)

site_names <- c("Dartmouth Harbourfront Greenway", "Hollis St",
                "South Park St", "Vernon St", "Windsor St")

#* @param count_date:str The date in YYYY-MM-DD format.
#* @param site_name:[str] The location of the bike counter. One of "Dartmouth Harbourfront Greenway", "Hollis St", "South Park St", "Vernon St", "Windsor St". If not provided, will return predictions for all five sites.
#* @param n_bikes_lag_14:[int] The number of bikes measured at the given `site_name` 14 days ago. If not provided, will attempt to impute with the actual value 14 days before `count_date`.
#* @param mean_temperature:numeric The daily mean temperature. If not provided, will impute with the rolling mean.
#* @param total_precipitation:numeric The daily amount of precipitation in mm. If not provided, will impute with zero.
#* @param snow_on_ground:numeric The daily amount of snow on the ground in cm. If not provided, will impute with zero.
#* @param speed_max_gust:numeric The daily maximum wind speed in km/h. If not provided, will impute with the mean in the training set.
#* @get /n_bikes
function(count_date, site_name = NA_character_, n_bikes_lag_14 = NA_integer_,
         mean_temperature = NA_real_, total_precipitation = NA_real_,
         snow_on_ground = NA_real_, speed_max_gust = NA_real_) {

  # If not provided, use all `site_name`s
  if (any(is.na(site_name))) {
    site_name <- site_names
  } else {
    site_name <- match.arg(
      site_name, choices = site_names, several.ok = TRUE
    )
  }

  count_date <- as.Date(count_date)

  # Check if there is a user-supplied `n_bikes_lag_14` for each `site_name`
  if (any(!is.na(n_bikes_lag_14)) &
      length(site_name) != length(n_bikes_lag_14)) {
    return(list(
      status = 400,
      message = "Must provide a value of `n_bikes_lag_14` for every given `site_name`."
    ))
  } else {
    d <- tibble(site_name = .env$site_name, count_date = .env$count_date,
                count_date_lag_14 = count_date - 14,
                n_bikes_lag_14 = .env$n_bikes_lag_14)

    # Otherwise, impute using the data
    if (sum(is.na(d$n_bikes_lag_14)) > 0) {
      message("Imputing `n_bikes_lag_14`")
      d <- d %>%
        left_join(
          bike_data %>%
            select(site_name, count_date_lag_14 = count_date,
                   n_bikes_lag_14_impute = n_bikes),
          by = c("site_name", "count_date_lag_14")
        ) %>%
        mutate(
          n_bikes_lag_14 = ifelse(is.na(n_bikes_lag_14),
                                  n_bikes_lag_14_impute, n_bikes_lag_14)
        ) %>%
        select(-n_bikes_lag_14_impute)

      if (sum(is.na(d$n_bikes_lag_14)) > 0) {
        return(list(
          status = 400,
          message = paste0(
            "Could not find `n_bikes_lag_14` values on date ", count_date,
            " for these sites ",
            filter(d, is.na(n_bikes_lag_14)) %>% pull(site_name) %>% paste(collapse = ", "),
            ". Please provide your own `n_bikes_lag_14`, or choose a different `count_date`."
          )
        ))
      }
    }
  }

  # Add weather variables
  d <- d %>%
    mutate(
      n_bikes_lag_14 = as.numeric(n_bikes_lag_14),
      mean_temperature = as.numeric(mean_temperature),
      total_precipitation = as.numeric(total_precipitation),
      snow_on_ground = as.numeric(snow_on_ground),
      speed_max_gust = as.numeric(speed_max_gust)
    )

  augment(xgb_fit$bike_xgb_fit, d)
}

#* @get /model_info
#* @response 200 Returns model information: timestamps of when the model was last trained (`timestamp`), the model was last tuned (`tune_timestamp`), the bicycle data was last updated (`bike_data_updated`), the weather data was last updated (`weather_data_updated`).
function() {
  list(
    timestamp = xgb_fit$timestamp,
    tune_timestamp = xgb_fit$tune_timestamp,
    bike_data_updated = xgb_fit$bike_data_updated,
    weather_data_updated = xgb_fit$weather_data_updated
  )
}
