
# hfx-bike-ridership

<!-- badges: start -->
<!-- badges: end -->

This is an end-to-end machine learning project to predict daily bike
ridership in Halifax, Nova Scotia, Canada.

The model is deployed on Google Cloud Platform, and available as a [REST
API](https://hfx-bike-ridership-api-74govvz7xq-uc.a.run.app/__docs__/)
and a [Shiny
dashboard](https://hfx-bike-ridership-app-74govvz7xq-uc.a.run.app).

For more information, check out this series of posts on my website:

1.  [Part 1: getting the
    data.](https://tdunn.ca/posts/2022-04-27-predicting-bike-ridership-getting-the-data/)
2.  [Part 2: develping a
    model.](https://tdunn.ca/posts/2022-04-29-predicting-bike-ridership-developing-a-model/)
3.  [Part 3: deploying the
    model.](https://tdunn.ca/posts/2022-05-19-predicting-bike-ridership-deploying-the-model/)

## Model tuning

The ETL pipeline to extract the data, transform it, and load it into
BigQuery, is currently scheduled to run on Sundays at midnight AST (or
when I manually run it). New data in BigQuery triggers Cloud Run to
automatically re-train the model.

On the other hand, I decided to tune the XGBoost model locally instead
of on GCP. Tuning on my PC takes upwards of 10 minutes, even when
running in parallel on a 6 core processor, so this was in the interest
of keeping the project (relatively) free and avoiding any [expensive
surprises](https://reddit.com/r/datascience/comments/tqe3y6/anyone_needs_ec2_instance/).

The code and results of the tuning are in the `model/tune/` folder of
this repository. The model was last tuned at 2022-05-20 15:23:46 UTC.

The data were split 70-30 by time. The training set consisted of 1997
observations, ranging in date from 2019-12-07 to 2021-11-23, and the
testing set consisted of 857 observations, ranging from 2021-11-24 to
2022-05-13. A time-based resampling strategy was used
(`rsample::sliding_period()`) to break the training set up into rolling
periods of 13 months analysis and 1 month assessment.

The final hyperparameters, chosen by lowest mean absolute scaled error
(MASE) were:

| parameter  | value               |
|:-----------|:--------------------|
| mtry       | 22                  |
| trees      | 1620                |
| min_n      | 40                  |
| tree_depth | 13                  |
| learn_rate | 0.00810337130082761 |

And the performance on the training set resamples and the test set:

| data_set | metric |  value |   n | std_err |
|:---------|:-------|-------:|----:|--------:|
| train    | mae    | 42.949 |  10 |   4.484 |
| train    | mase   |  0.429 |  10 |   0.046 |
| train    | rmse   | 61.947 |  10 |   6.342 |
| train    | rsq    |  0.730 |  10 |   0.049 |
| test     | mae    | 26.210 |     |         |
| test     | mase   |  0.586 |     |         |
| test     | rmse   | 39.443 |     |         |
| test     | rsq    |  0.667 |     |         |
