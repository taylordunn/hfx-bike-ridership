library(googleCloudRunner)

# Provide the project id and the two JSON files (client and credentials)
 cr_setup()
# Restart R afterwards to load the environment variables that were just added

build <- cr_build("etl/hfx-bike-ridership-etl.yaml")
