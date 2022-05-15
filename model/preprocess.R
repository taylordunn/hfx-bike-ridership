preprocess <- function(bike_data, weather_data) {
  weather_data <- weather_data %>%
    transmute(
      report_date,
      # Where the primary data is missing (from the Windsor Park station),
      #  impute with the value reported at the Halifax airport (if not missing)
      mean_temperature = ifelse(
        is.na(mean_temperature),
        mean_temperature_airport, mean_temperature
      ),
      total_precipitation = ifelse(
        is.na(total_precipitation),
        total_precipitation_airport, total_precipitation
      ),
      snow_on_ground = ifelse(
        is.na(snow_on_ground),
        snow_on_ground_airport, snow_on_ground
      ),
      speed_max_gust = ifelse(
        is.na(speed_max_gust),
        speed_max_gust_airport, speed_max_gust
      )
    )

  # These are series of dates that I consider anomalies due to a series of zeros
  south_park_2019_11 <- with(bike_data,
    (site_name == "South Park St") & (count_date < "2019-11-23")
  )
  vernon_st_2011_11 <- with(bike_data,
    (site_name == "Vernon St") &
      (count_date >= "2021-11-13" & count_date <= "2021-11-18")
  )

  bike_data <- bike_data %>%
    # Remove anomalous data
    filter(!south_park_2019_11, !vernon_st_2011_11) %>%
    left_join(weather_data, by = c("count_date" = "report_date")) %>%
    # Since data was just removed, need to re-calculate the lag values
    arrange(count_date, site_name) %>%
    group_by(site_name) %>%
    mutate(n_bikes_lag_14 = lag(n_bikes, 14)) %>%
    # Exclude data from the first 14 days which don't have `n_bikes_lag_14`
    ungroup() %>%
    filter(!is.na(n_bikes_lag_14))

  return(bike_data)
}
