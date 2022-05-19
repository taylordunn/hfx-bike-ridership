# Setup -------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(bigrquery)
library(googleCloudStorageR)
library(tictoc)
source("model/preprocess.R")

n_cores <- parallel::detectCores(logical = FALSE)
library(doParallel)
cl <- makePSOCKcluster(n_cores - 1)
registerDoParallel(cl)
# This extra step makes sure the parallel workers have access to the
#  `tidyr::replace_na()` function during pre-processing
parallel::clusterExport(cl, c("replace_na"))

# If necessary, setup GCP credentials
#googleCloudRunner::cr_setup()
#googleCloudStorageR::gcs_setup()

# Read data ---------------------------------------------------

bq_auth(path = "oauth-client.json")

# Define the project, dataset and a new table for this project
project <- "hfx-bike-ridership"

daily_counts_table <- bq_table(project, "bike_counts", "daily_counts")
bike_data <- bq_table_download(daily_counts_table)

weather_table <- bq_table(project, "weather", "daily_report")
weather_data <- bq_table_download(weather_table)

# Pre-process -------------------------------------------------------------

bike_data <- preprocess(bike_data, weather_data)

# Splitting and resampling ------------------------------------------------

# For the initial time split, data is ordered by date so that the training
#  data consists of the earliest dates across all sites
bike_data <- bike_data %>% arrange(count_date, site_name)
bike_split <- initial_time_split(bike_data, prop = 0.7)

bike_train <- training(bike_split)
bike_test <- testing(bike_split)

# ... but once I'm done splitting the data, I want to order by site followed by
#  date for two reasons:
#  (1) `step_impute_roll()` looks for rows in a window (ordered)
#  (2) the `mase` metric compares predictions to the naive prediction, which
#      uses the previous value
bike_train <- bike_train %>% arrange(count_date, site_name)
bike_test <- bike_test %>% arrange(count_date, site_name)

bike_resamples <-
  sliding_period(bike_train, index = count_date,
                 period = "month", lookback = 13, assess_stop = 1)

# For model versioning, record the splitting and resampling strategy
splits_resamples <- tibble(
  n_data = nrow(bike_data), n_train = nrow(bike_train), n_test = nrow(bike_test),
  min_date_train = min(bike_train$count_date),
  max_date_train = max(bike_train$count_date),
  min_date_test = min(bike_test$count_date),
  max_date_test = max(bike_test$count_date),
  prop = 0.7, resamples = "sliding_period",
  resample_params = "lookback = 13, assess_stop = 1"
)

# Features ------------------------------------------------------------------

# Get Canadian holidays
canada_holidays <-
  timeDate::listHolidays(
    pattern = "^CA|^Christmas|^NewYears|Easter[Sun|Mon]|^GoodFriday|^CaRem"
  )

bike_recipe <-
  recipe(n_bikes ~ count_date + site_name + n_bikes_lag_14 +
           mean_temperature + total_precipitation + speed_max_gust +
           snow_on_ground,
         data = bike_train) %>%
  update_role(count_date, new_role = "date_variable") %>%
  step_date(count_date, features = c("dow", "doy", "year"),
            label = TRUE, ordinal = FALSE) %>%
  step_holiday(count_date, holidays = canada_holidays) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_impute_mean(speed_max_gust) %>%
  step_mutate_at(c(total_precipitation, snow_on_ground),
                 fn = ~ replace_na(., 0)) %>%
  # Use a rolling window to impute temperature
  step_impute_roll(mean_temperature, statistic = mean, window = 31) %>%
  step_zv(all_predictors())


# Model spec and workflow -----------------------------------------------------

xgb_spec <- boost_tree(
  mtry = tune(), trees = tune(), min_n = tune(),
  tree_depth = tune(), learn_rate = tune()
  # Other hyperparameters not tuned:
  #  `loss_reduction`: defaults to 0.0
  #  `sample_size`: defaults to 1.0 (all observations included)
  #  `stop_iter`: defaults to Inf (doesn't stop early)
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

bike_xgb_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(xgb_spec)

bike_train_baked <- prep(bike_recipe) %>% bake(bike_train)

xgb_grid <- grid_latin_hypercube(
  finalize(mtry(), select(bike_train_baked, -n_bikes)),
  trees(), min_n(), tree_depth(), learn_rate(),
  size = 100
)

# Tune --------------------------------------------------------------------

bike_metrics <- metric_set(rmse, mae, rsq, mase)

set.seed(944)
tic()
xgb_tune <- tune_grid(
  bike_xgb_workflow, resamples = bike_resamples,
  grid = xgb_grid, metrics = bike_metrics
)
toc()

# Find the best hyperparameters by MASE
xgb_params <- select_best(xgb_tune, metric = "mase")
# Also get all the metrics from the cross-validated tuning
train_metrics <- xgb_params %>%
  left_join(
    collect_metrics(xgb_tune) %>%
      select(.metric, mean, n, std_err, .config),
    by = ".config"
  )

# Finalize and fit to the full training set
bike_xgb_workflow_final <- finalize_workflow(bike_xgb_workflow, xgb_params)
bike_xgb_fit <- bike_xgb_workflow_final %>% fit(bike_train)

# Predict on the test set and get metrics
test_metrics <- bike_xgb_fit %>%
  augment(bike_test) %>%
  bike_metrics(truth = n_bikes, estimate = .pred)
xgb_metrics <- bind_rows(
  train = train_metrics %>%
    select(metric = .metric, value = mean, n, std_err),
  test = test_metrics %>%
    select(metric = .metric, value = .estimate),
  .id = "data_set"
)

# Save --------------------------------------------------------------------

# Model object
write_rds(bike_xgb_fit, "model/xgb-fit.rds")

gcs_upload_set_limit(20000000L) # 20 Mb
# This works, but also returns an error from GCP about bucket-level access
# See: https://github.com/cloudyr/googleCloudStorageR/issues/121
metadata <- gcs_upload("model/xgb-fit.rds", name = "xgb-fit.rds",
                       bucket = "hfx-bike-ridership-model")#, upload_type = "resumable")
timestamp <- as.POSIXct(metadata$updated,
                        tryFormats = "%Y-%m-%dT%H:%M:%OS", tz = "GMT")

# Model parameters
xgb_params <- xgb_params %>% mutate(timestamp = timestamp) %>% select(-.config)
write_csv(xgb_params, "model/xgb-params.csv",
          append = TRUE, col_names = FALSE)
params_table <- bq_table(project, "model_info", "params")
bq_table_upload(params_table,
                value = xgb_params, fields = xgb_params,
                create_disposition = "CREATE_IF_NEEDED",
                write_disposition = "WRITE_APPEND")

# Model metrics
xgb_metrics <- xgb_metrics %>% mutate(timestamp = timestamp)
write_csv(xgb_metrics, "model/xgb-metrics.csv",
          append = TRUE, col_names = FALSE)
metrics_table <- bq_table(project, "model_info", "metrics")
bq_table_upload(metrics_table,
                value = xgb_metrics, fields = xgb_metrics,
                create_disposition = "CREATE_IF_NEEDED",
                write_disposition = "WRITE_APPEND")

# Splitting and resampling strategy
splits_resamples <- splits_resamples %>% mutate(timestamp = timestamp)
write_csv(splits_resamples, "model/splits-resamples.csv",
          append = TRUE, col_names = FALSE)
splits_resamples_table <- bq_table(project, "model_info", "splits_resamples")
bq_table_upload(splits_resamples_table,
                value = splits_resamples, fields = splits_resamples,
                create_disposition = "CREATE_IF_NEEDED",
                write_disposition = "WRITE_APPEND")

