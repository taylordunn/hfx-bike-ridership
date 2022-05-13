library(googleCloudRunner)

# Use this command to authenticate and setup necessary credentials
# cr_setup()
# Project ID: `hfx-bike-ridership`
# Authentication JSON file: `client-id.json`
# Cloud Schedular build email: chose default email (taken from JSON)

build <- cr_build_make("R/hfx-bike-ridership.yaml")

cr_schedule(schedule = "0 * * * *", name = "big-query-test",
            httpTarget = cr_schedule_http(build),
            region = "northamerica-northeast1")
            #region = "us-east1")
