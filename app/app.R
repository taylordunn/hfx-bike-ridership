library(shiny)
library(shinydashboard)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(workflows)
library(bigrquery)
library(googleCloudStorageR)
library(DT)
library(gt)
library(dunnr)
source("funcs.R")

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
  # Import and process data ---------------------------------------------------
  data <- reactiveValues()
  min_date <- reactiveVal()
  max_date <- reactiveVal()

  observe({
    # Re-reads data every
    invalidateLater(1e4)

    message("Reading data")

    bike_data_raw <- readr::read_rds("bike-data.rds")
    weather_data_raw <- readr::read_rds("weather-data.rds")

    bike_data <- bike_data_raw %>%
      preprocess_bike_data() %>%
      # Only include the last 14 days
      filter(count_date >= max(count_date) - 13)
    min_date(min(bike_data$count_date))
    max_date(max(bike_data$count_date))
    bike_data_future <- bike_data %>%
      transmute(
        count_date = count_date + 14, site_name, n_bikes_lag_14 = n_bikes
      )

    weather_data <- weather_data_raw %>%
      preprocess_weather_data() %>%
      filter(report_date >= min(bike_data$count_date),
             report_date <= max(bike_data$count_date))
    weather_data_future <- weather_data %>%
      transmute(
        report_date = report_date + 14,
        # Impute temperature and wind speed with the mean
        mean_temperature = round(mean(weather_data$mean_temperature,
                                      na.rm = TRUE), 1),
        speed_max_gust = round(mean(weather_data$speed_max_gust,
                                    na.rm = TRUE)),
        # Impute precipitation and snow with zero
        total_precipitation = 0, snow_on_ground = 0
      )

    data$bike <- bind_rows(bike_data, bike_data_future)
    data$weather <- bind_rows(weather_data, weather_data_future)
  })

  bike_weather_data <- reactive({
    data$bike %>%
      left_join(data$weather, by = c("count_date" = "report_date"))
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

  # Plotting helpers --------------------------------------------------------
  scale_x <- reactive({
    scale_x_date(NULL, limits = c(min_date() - 1, max_date() + 14),
                 breaks = seq.Date(min_date() - 1, max_date() + 14, "7 days"),
                 date_labels = "%b %d")
  })
  vline <- reactive({
    geom_vline(xintercept = max_date() + 0.5, lty = 2, size = 1)
  })

  model <- reactive({readr::read_rds("xgb-fit.rds")})


  # Bike predictions --------------------------------------------------------
  output$n_bikes_plot <- renderPlot({
    workflows:::augment.workflow(model()$bike_xgb_fit,
                                 bike_weather_data()) %>%
      ggplot(aes(x = count_date)) +
      geom_line(aes(y = .pred), color = "black", size = 1) +
      #geom_point(aes(y = n_bikes, color = site_name), size = 3) +
      geom_point(aes(y = n_bikes, fill = site_name),
                 color = "black", shape = 21, size = 4) +
      vline() +
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
    data$weather %>%
      filter(!is.na(mean_temperature)) %>%
      mutate(var = "Mean daily temperature (celsius)") %>%
      ggplot(aes(x = report_date, y = mean_temperature)) +
      geom_point(fill = td_colors$nice$light_coral, shape = 21, size = 4) +
      vline() +
      facet_wrap(~ var) +
      labs(y = NULL,
           title = "Weather vs date",
           subtitle = "Use the table below to edit values for prediction") +
      scale_x()
  })
  output$precipitation_plot <- renderPlot({
    data$weather %>%
      filter(!is.na(total_precipitation)) %>%
      mutate(var = "Total daily precipitation (mm)") %>%
      ggplot(aes(x = report_date, y = total_precipitation)) +
      geom_col(fill = td_colors$nice$spanish_blue) +
      vline() +
      facet_wrap(~ var) +
      expand_limits(y = 5) +
      scale_y_continuous(NULL, expand = expansion(mult = c(0, 0.05))) +
      scale_x()
  })

  output$snow_plot <- renderPlot({
    data$weather %>%
      filter(!is.na(snow_on_ground)) %>%
      mutate(var = "Snow on ground (cm)") %>%
      ggplot(aes(x = report_date, y = snow_on_ground)) +
      geom_col(fill = td_colors$nice$charcoal) +
      vline() +
      facet_wrap(~ var) +
      expand_limits(y = 5) +
      scale_y_continuous(NULL, expand = expansion(mult = c(0, 0.05))) +
      scale_x()
  })
  output$wind_plot <- renderPlot({
    data$weather %>%
      filter(!is.na(speed_max_gust)) %>%
      mutate(var = "Maximum wind gust (km/h)") %>%
      ggplot(aes(x = report_date, y = speed_max_gust)) +
      geom_point(fill = td_colors$nice$emerald, shape = 21, size = 4) +
      vline() +
      facet_wrap(~ var) +
      labs(y = NULL) +
      scale_x()
  })

  output$weather_plot <- renderPlot({

  })

  output$weather_table <- renderDataTable(
    datatable(
      data$weather,
      rownames = FALSE, escape = FALSE,
      # colnames = c("Date", "Temperature<br>(celsius)",
      #              "Precipitation<br>(mm)", "Snow<br>(cm)", "Max wind<br>(km/h)"),
      colnames = c("Date", "Temp.", "Precip.", "Snow", "Wind"),
      editable = list(target = "cell", numeric = c(2, 3, 4, 5)),
      options = list(pageLength = 7, dom = "tp"),
      caption = "Double click a cell to edit values. Plots and predictions will update automatically."
    ) %>%
      DT::formatStyle(names(data$weather), lineHeight = "80%")
  )

  observeEvent(input$weather_table_cell_edit, {
    row <- input$weather_table_cell_edit$row
    col <- input$weather_table_cell_edit$col
    data$weather[row, col + 1] <- input$weather_table_cell_edit$value
  })
}

ui <- dashboardPage(
  dashboardHeader(title = "Predicting bike ridership in Halifax, NS",
                  titleWidth = 700),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    column(width = 3,
      box(
        title = "Info", width = 12,
        style = "overflow-x: scroll;height:900px;",
        uiOutput("model_info_1"),
        img(src = "bike-counter-sites.png",
            style = "width: 300px; display: block; margin-left: auto; margin-right: auto;"),
        #gt_output("model_timestamps"),
        uiOutput("model_info_2"),
        uiOutput("model_info_3")
      )
    ),
    column(width = 5,
      box(
        width = 12,
        style = "overflow-x: scroll;height:900px;",
        plotOutput("n_bikes_plot", height = "800px")
      )
    ),
    column(
      width = 4,
      box(
        width = 12,
        style = "overflow-x: scroll;height:900px;",
        plotOutput("temperature_plot", height = "150px"),
        plotOutput("precipitation_plot", height = "150px"),
        plotOutput("snow_plot", height = "150px"),
        plotOutput("wind_plot", height = "150px"),
        dataTableOutput("weather_table")
      )
    )
  )
)

shinyApp(ui, server)
