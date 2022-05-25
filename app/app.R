library(shiny)
library(shinydashboard)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidymodels)
library(bigrquery)
library(googleCloudStorageR)
library(DT)
library(gt)
library(dunnr)
source("preprocess.R")

extrafont::loadfonts(device = "win", quiet = TRUE)
theme_set(theme_td(base_size = 16))
set_geom_fonts()
set_palette()

project <- "hfx-bike-ridership"
bq_auth(path = "oauth-client.json")
gcs_auth("oauth-client.json")


if (FALSE) {
  daily_counts_table <- bq_table(project, "bike_counts", "daily_counts")
  bike_data <- bq_table_download(daily_counts_table)

  weather_table <- bq_table(project, "weather", "daily_report")
  weather_data <- bq_table_download(weather_table)

  xgb_fit <- gcs_get_object("xgb-fit.rds",
                            bucket = "hfx-bike-ridership-model",
                            parseFunction = gcs_parse_rds)
} else {
}

server <- function(input, output, session) {
  # Import data and model ---------------------------------------------------
  weather_data_raw <- reactive({readr::read_rds("weather-data.rds")})
  bike_data_raw <- reactive({readr::read_rds("bike-data.rds")})
  model <- reactive({readr::read_rds("xgb-fit.rds")})
  model_preproc <- reactive({
    prep(extract_preprocessor(model()$bike_xgb_fit))
  })

  bike_data <- reactive({
    bike_data_raw() %>%
      preprocess(weather_data_raw()) %>%
      filter(count_date >= max(count_date) - 13)
  })
  bike_data_future <- reactive({
    bike_data() %>%
      transmute(
        count_date = count_date + 14, site_name,
        n_bikes_lag_14 = n_bikes,
        mean_temperature = mean(bike_data()$mean_temperature),
        speed_max_gust = round(mean(bike_data()$speed_max_gust, na.rm = TRUE)),
        total_precipitation = 0, snow_on_ground = 0
      )
  })

  # bike_data_prepped <- reactive({
  #   bake(model_preproc(),
  #        bind_rows(bike_data(), bike_data_future()))
  # })
  # bike_data_preds <- reactive({
  #
  # })

  weather_data <- reactive({
    bind_rows(bike_data(), bike_data_future()) %>%
      distinct(count_date, mean_temperature, speed_max_gust,
               total_precipitation, snow_on_ground)
  })

  max_date <- reactive({max(bike_data()$count_date)})
  min_date <- reactive({min(bike_data()$count_date)})
  scale_x <- reactive({
    scale_x_date(NULL,
                 limits = c(min_date() - 1, max_date() + 14),
                 breaks = seq.Date(min_date() - 1, max_date() + 14, "7 days"),
                 date_labels = "%b %d")
  })


  # Model info --------------------------------------------------------------
  output$model_info_1 <- renderText({
    HTML(
      paste(
        "This Shiny app visualizes predictions of the daily number of bicyclists passing various bike counter sites around Halifax, Nova Scotia.",
        "Check out the <a href='https://github.com/taylordunn/hfx-bike-ridership'>source code here</a>, and <a href='https://tdunn.ca/posts/2022-04-27-predicting-bike-ridership-getting-the-data/'>this write-up</a> for more information.",
        "The locations of the sites are overlaid on a map of Halifax below:",
        sep = "<br>"
      )
    )
  })

  output$model_info_2 <- renderText({
    HTML(
      paste(
        "In addition to site, other features of the model are:",
        paste0("<ul>",
               "<li>date features: day of week, day of year, year, and Canadian holidays</li>",
               "<li>the number of bikes counted 14 days ago</li>",
               "<li>weather features: daily mean temperature, total precipitation, maximum gust speed, and snow on the ground",
               "</ul>"),
        "See more information about the features and how missing data are handled <a href='https://tdunn.ca/posts/2022-04-27-predicting-bike-ridership-getting-the-data/'>in this post</a>.",
        "<br>"
      )
    )
  })
  output$model_info_3 <- renderText({
    HTML(
      paste(
        paste0("The data and model are deployed and continuously updated on Google Cloud Platform. Currently this is scheduled for midnight on Sundays (and sometimes manually by myself). Most recently, the model was trained on data up to ",
               "<b>", max_date(), "</b>",
               " as indicated by the vertical dotted line in the plots.")
      )
    )
  })

  output$model_timestamps <- render_gt({
    tribble(
      ~ var, ~ val,
      "Bike data updated", as.Date(model()$bike_data_updated),
      "Weather data updated", as.Date(model()$weather_data_updated),
      "Model trained", as.Date(model()$timestamp)
    ) %>%
      gt() %>%
      tab_options(column_labels.hidden = TRUE)
  })

  # Bike predictions --------------------------------------------------------
  output$n_bikes_plot <- renderPlot({
    d <- bind_rows(bike_data(), bike_data_future())
    augment(model()$bike_xgb_fit, d) %>%
      ggplot(aes(x = count_date)) +
      geom_line(aes(y = .pred), color = "black", size = 1) +
      geom_vline(xintercept = max_date() + 0.5, lty = 2, size = 1) +
      #geom_point(aes(y = n_bikes, color = site_name), size = 3) +
      geom_point(aes(y = n_bikes, fill = site_name),
                 color = "black", shape = 21, size = 4) +
      facet_wrap(~ site_name, ncol = 1) +
      expand_limits(y = 0) +
      scale_x() +
      labs(title = "Number of bikes vs date",
           subtitle = "Coloured points show actual values, black lines are predictions") +
      theme(legend.position = "none") +
      dunnr::add_facet_borders()
  })

  # Weather data ------------------------------------------------------------
  output$temperature_plot <- renderPlot({
    weather_data() %>%
      filter(!is.na(mean_temperature)) %>%
      ggplot(aes(x = count_date, y = mean_temperature)) +
      geom_point() +
      geom_vline(aes(xintercept = max_date()), lty = 2) +
      labs(y = "mean daily temperature (celsius)") +
      scale_x()
  })
  output$wind_plot <- renderPlot({
    weather_data() %>%
      filter(!is.na(speed_max_gust)) %>%
      ggplot(aes(x = count_date, y = speed_max_gust)) +
      geom_point() +
      geom_vline(aes(xintercept = max_date()), lty = 2) +
      labs(y = "max wind gust (km/h)") +
      scale_x()
  })
  output$precipitation_plot <- renderPlot({
    weather_data() %>%
      filter(!is.na(total_precipitation)) %>%
      ggplot(aes(x = count_date, y = total_precipitation)) +
      geom_point() +
      geom_vline(aes(xintercept = max_date()), lty = 2) +
      labs(y = "total precipitation (mm)") +
      scale_x()
  })
  output$snow_plot <- renderPlot({
    weather_data() %>%
      filter(!is.na(snow_on_ground)) %>%
      ggplot(aes(x = count_date, y = snow_on_ground)) +
      geom_point() +
      geom_vline(aes(xintercept = max_date()), lty = 2) +
      labs(y = "snow_on_ground (cm)") +
      scale_x()
  })

  output$weather_table <- renderDataTable(
    weather_data(),
    rownames = FALSE, escape = FALSE,
    colnames = c("Date", "Temperature<br>(celsius)",
                 "Precipitation<br>(mm)", "Snow<br>(cm)", "Max wind<br>(km/h)"),
    editable = list(target = "cell", numeric = c(2, 3, 4, 5))
    #options = list(pageLength = 5, dom = "t", autoWidth = TRUE)
  )
}

ui <- dashboardPage(
  dashboardHeader(title = "Predicting bike ridership in Halifax, NS",
                  titleWidth = 700),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    column(width = 3,
      box(title = "Info", width = 12,
        uiOutput("model_info_1"),
        img(src = "bike-counter-sites.png", align = "center",
            style = "width: 300px; text-align: center;"),
        #gt_output("model_timestamps"),
        uiOutput("model_info_2"),
        uiOutput("model_info_3")
      )
    ),
    column(width = 5,
      box(width = 12,
        plotOutput("n_bikes_plot", height = "800px")
      )
    ),
    column(width = 4,
      box(width = 12, style='overflow-x: scroll;height:900px;overflow-y: scroll;',
      #box(title = "Weather data", width = 12, height = "900px",
        plotOutput("temperature_plot", height = "100px"),
        dataTableOutput("weather_table")
        # plotOutput("wind_plot", height = "100px"),
        # plotOutput("precipitation_plot", height = "100px"),
        # plotOutput("snow_plot", height = "100px")
      )
    ),
  )
)

shinyApp(ui, server)
