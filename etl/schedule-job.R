library(googleCloudRunner)

# Use this command to authenticate and setup necessary credentials
cr_setup()
# Project ID: `hfx-bike-ridership`
# Authentication JSON file: `client-id.json`
# Cloud Scheduler build email: chose default email (taken from JSON)

build <- cr_build_make("etl/hfx-bike-ridership-etl.yaml")

cr_schedule(
  # Schedule for every Sunday at 12am
  schedule = "0 0 * * SUN",
  name = "etl",
  httpTarget = cr_schedule_http(build),
  region = "northamerica-northeast1"
)
