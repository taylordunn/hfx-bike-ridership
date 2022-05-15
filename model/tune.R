# Setup -------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(bigrquery)
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

# Read data ---------------------------------------------------

bq_auth(path = "service-account.json", email = "t.dunn19@gmail.com")

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

# Workflow ------------------------------------------------------------------

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
  step_impute_roll(mean_temperature, statistic = mean, window = 15) %>%
  step_zv(all_predictors())

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

# Tune --------------------------------------------------------------------

bike_metrics <- metric_set(rmse, mae, rsq, mase)

bike_train_baked <- prep(bike_recipe) %>% bake(bike_train)
xgb_grid <- grid_latin_hypercube(
  finalize(mtry(), select(bike_train_baked, -n_bikes)),
  trees(), min_n(), tree_depth(), learn_rate(),
  size = 100
)

set.seed(944)
tic()
xgb_tune <- tune_grid(
  bike_xgb_workflow, resamples = bike_resamples,
  grid = xgb_grid, metrics = bike_metrics
)
toc()

best_config <- collect_metrics(xgb_tune) %>%
  filter(.metric == "mase") %>%
  slice_min(mean, n = 1)

show_best(xgb_tune, "mase")

bike_xgb_workflow_final <- finalize_workflow(
  bike_xgb_workflow,
  #select_best(xgb_tune, metric = "mase")
  select_best(xgb_tune, metric = "rmse")
)

bike_xgb_fit <- bike_xgb_workflow_final %>%
  fit(bike_train)

bike_xgb_fit %>%
  augment(bike_test) %>%
  bike_metrics(truth = n_bikes, estimate = .pred)
bike_xgb_fit %>%
  augment(filter(bike_train, !is.na(mean_temperature))) %>%
  bike_metrics(truth = n_bikes, estimate = .pred)

# Save --------------------------------------------------------------------

write_rds(bike_xgb_workflow_final, "model/bike_xgb_workflow.rds")
write_rds(bike_xgb_fit, "model/bike_xgb_fit.rds")
write_rds(bike_xgb_train_metrics, "model/bike_xgb_fit.rds")
