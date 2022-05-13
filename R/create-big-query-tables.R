# This script authorizes access to BigQuery and creates a new table (run once)

library(bigrquery)
library(tidyverse)

# Authorize bigrquery to view and manager BigQuery projects
bq_auth("../docker/hfx-bike-ridership.json", email = "t.dunn19@gmail.com")

# Define the project, dataset and a new table for this project
project <- "hfx-bike-ridership"
dataset <- "bike_counts"
table <- "hourly_counts"

# Create the table on BQ
hourly_counts_ref <- bq_table(project, dataset, table)
bq_table_create(hourly_counts_ref)
# Upload current time to the table
df <- Sys.time() %>% as_tibble()
bq_table_upload(hourly_counts_ref, df, fields = df)
