library(googleCloudRunner)

# Provide the project id and the two JSON files (client and credentials)
# cr_setup()
# Restart R afterwards to load the environemnt varaibles that were just added
build <- cr_build("R/hfx-bike-ridership.yaml")
