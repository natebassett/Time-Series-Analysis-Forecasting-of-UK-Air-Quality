library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(vroom)
library(bslib)
library(DT)

setwd("C:\\Users\\natha\\OneDrive\\Desktop\\MAT514\\Assessment 2\\514")

DATA_FILE <- "AURN_2015_2023.csv"
START_YEAR <- 2020

pollutant_cols <- c(
  "co", "nox", "no2", "no", "o3", "so2",
  "pm10", "pm2.5",
  "v10", "v2.5", "nv10", "nv2.5",
  "ws", "wd", "air_temp"
)

# 2. LOAD DATA EFFICIENTLY

df <- vroom(
  DATA_FILE,
  show_col_types = FALSE,
  altrep = TRUE
)

names(df) <- tolower(names(df))

if ("...1" %in% names(df)) {
  df <- df %>% select(-...1)
}

# Keep only the columns needed by the app
available_cols <- intersect(c("date", "site", pollutant_cols), names(df))

df <- df %>%
  select(all_of(available_cols)) %>%
  mutate(
    date = ymd_hms(date, quiet = TRUE)
  ) %>%
  filter(
    !is.na(date),
    year(date) >= START_YEAR
  )

# Convert to long format only after filtering
df_long <- df %>%
  pivot_longer(
    cols = all_of(intersect(pollutant_cols, names(df))),
    names_to = "pollutant",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    site = as.factor(site),
    pollutant = as.factor(pollutant)
  )

available_sites <- sort(unique(as.character(df_long$site)))
available_pollutants <- sort(unique(as.character(df_long$pollutant)))

# 3. HELPER FUNCTIONS

get_frequency <- function(aggregation) {
  switch(
    aggregation,
    "Daily" = 7,
    "Weekly" = 52,
    "Monthly" = 12,
    "Yearly" = 1,
    12
  )
}

make_period <- function(date, aggregation) {
  case_when(
    aggregation == "Daily" ~ as.Date(date),
    aggregation == "Weekly" ~ as.Date(floor_date(date, "week")),
    aggregation == "Monthly" ~ as.Date(floor_date(date, "month")),
    aggregation == "Yearly" ~ as.Date(paste0(year(date), "-01-01")),
    TRUE ~ as.Date(date)
  )
}

build_forecast <- function(ts_data, model_type, horizon) {
  switch(
    model_type,
    "ARIMA" = forecast(auto.arima(ts_data), h = horizon),
    "ETS" = forecast(ets(ts_data), h = horizon),
    "Naive" = naive(ts_data, h = horizon),
    "Seasonal Naive" = snaive(ts_data, h = horizon)
  )
}

# 4. UI
ui <- page_sidebar(
  title = "UK Air Quality Forecast Dashboard",
  
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  sidebar = sidebar(
    width = 340,
    
    h5("Controls"),
    
    selectInput(
      "site",
      "Monitoring site:",
      choices = available_sites,
      selected = available_sites[1]
    ),
    
    checkboxInput(
      "enable_compare",
      "Enable site comparison",
      value = FALSE
    ),
    
    uiOutput("second_site_ui"),
    
    selectInput(
      "pollutant",
      "Pollutant / weather variable:",
      choices = available_pollutants,
      selected = "pm10"
    ),
    
    selectInput(
      "aggregation",
      "Time scale:",
      choices = c("Weekly", "Monthly", "Yearly", "Daily"),
      selected = "Monthly"
    ),
    
    selectInput(
      "model_type",
      "Forecasting model:",
      choices = c("ARIMA", "ETS", "Naive", "Seasonal Naive"),
      selected = "ARIMA"
    ),
    
    sliderInput(
      "forecast_horizon",
      "Forecast periods:",
      min = 3,
      max = 36,
      value = 12,
      step = 1
    ),
    
    checkboxInput(
      "trend_line",
      "Show trend line",
      value = TRUE
    ),
    
    hr(),
    
    helpText(
      "Data is filtered from 2020 onwards to improve app speed and stability."
    )
  ),
  
  layout_columns(
    col_widths = c(4, 4, 4),
    
    value_box(
      title = "Selected Site",
      value = textOutput("selected_site_box"),
      theme = "primary"
    ),
    
    value_box(
      title = "Selected Variable",
      value = textOutput("selected_pollutant_box"),
      theme = "info"
    ),
    
    value_box(
      title = "Model",
      value = textOutput("selected_model_box"),
      theme = "success"
    )
  ),
  
  card(
    card_header("Problem Overview"),
    p(
      "This Shiny app investigates whether UK air-quality and weather variables can be forecast using historical monitoring data."
    ),
    p(
      "The app applies time-series forecasting models from the R forecast package, including ARIMA, ETS, naive and seasonal naive methods."
    )
  ),
  
  navset_card_tab(
    nav_panel(
      "Trend Explorer",
      plotOutput("time_series_plot", height = "430px")
    ),
    
    nav_panel(
      "Forecast",
      plotOutput("forecast_plot", height = "430px"),
      br(),
      htmlOutput("model_explanation")
    ),
    
    nav_panel(
      "Decomposition",
      plotOutput("decomposition_plot", height = "500px")
    ),
    
    nav_panel(
      "Accuracy",
      DTOutput("accuracy_table"),
      br(),
      p(
        "Accuracy is calculated using an 80/20 train-test split. Lower RMSE, MAE and MAPE values indicate better forecasting performance."
      )
    ),
    
    nav_panel(
      "Summary Statistics",
      DTOutput("summary_table")
    )
  )
)

# 5. SERVER

server <- function(input, output, session) {
  
  output$second_site_ui <- renderUI({
    if (isTRUE(input$enable_compare)) {
      selectInput(
        "site2",
        "Second monitoring site:",
        choices = available_sites,
        selected = available_sites[min(2, length(available_sites))]
      )
    }
  })
  
  output$selected_site_box <- renderText({
    input$site
  })
  
  output$selected_pollutant_box <- renderText({
    input$pollutant
  })
  
  output$selected_model_box <- renderText({
    input$model_type
  })
  
  filtered_data <- reactive({
    req(input$site, input$pollutant)
    
    selected_sites <- input$site
    
    if (isTRUE(input$enable_compare)) {
      req(input$site2)
      selected_sites <- c(input$site, input$site2)
    }
    
    df_long %>%
      filter(
        site %in% selected_sites,
        pollutant == input$pollutant
      )
  })
  
  aggregated_data <- reactive({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    data %>%
      mutate(
        period = make_period(date, input$aggregation)
      ) %>%
      group_by(site, period) %>%
      summarise(
        avg_value = mean(value, na.rm = TRUE),
        observations = n(),
        .groups = "drop"
      ) %>%
      arrange(site, period)
  })
  
  selected_site_series <- reactive({
    data <- aggregated_data() %>%
      filter(site == input$site) %>%
      arrange(period)
    
    req(nrow(data) > 0)
    
    data
  })
  
  output$time_series_plot <- renderPlot({
    data <- aggregated_data()
    
    validate(
      need(nrow(data) > 1, "Not enough data to display.")
    )
    
    p <- ggplot(
      data,
      aes(
        x = period,
        y = avg_value,
        colour = site,
        group = site
      )
    ) +
      geom_line(linewidth = 0.9) +
      theme_minimal(base_size = 13) +
      labs(
        title = paste("Observed", input$pollutant, "trend"),
        subtitle = paste("Aggregation:", input$aggregation),
        x = "Time",
        y = paste("Average", input$pollutant),
        colour = "Site"
      ) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
    
    if (isTRUE(input$trend_line)) {
      p <- p +
        geom_smooth(
          method = "lm",
          se = TRUE,
          linewidth = 0.8,
          alpha = 0.15
        )
    }
    
    p
  })
  
  forecast_result <- reactive({
    data <- selected_site_series()
    
    validate(
      need(nrow(data) > 10, "Not enough data for forecasting.")
    )
    
    freq <- get_frequency(input$aggregation)
    
    ts_data <- ts(
      data$avg_value,
      frequency = freq
    )
    
    build_forecast(
      ts_data = ts_data,
      model_type = input$model_type,
      horizon = input$forecast_horizon
    )
  })
  
  output$forecast_plot <- renderPlot({
    fc <- forecast_result()
    
    autoplot(fc) +
      theme_minimal(base_size = 13) +
      labs(
        title = paste(input$model_type, "forecast for", input$pollutant),
        subtitle = paste(
          input$site,
          "| Forecast horizon:",
          input$forecast_horizon,
          input$aggregation,
          "periods"
        ),
        x = "Time index",
        y = paste("Average", input$pollutant)
      ) +
      theme(
        plot.title = element_text(face = "bold")
      )
  })
  
  output$model_explanation <- renderUI({
    explanation <- switch(
      input$model_type,
      
      "ARIMA" = paste(
        "<b>ARIMA</b> models use previous observations, differencing and past forecast errors",
        "to predict future values. It is useful when the data has autocorrelation or repeating temporal structure."
      ),
      
      "ETS" = paste(
        "<b>ETS</b> stands for Error, Trend and Seasonality.",
        "It is useful when a time series contains smooth trend or seasonal behaviour."
      ),
      
      "Naive" = paste(
        "<b>Naive forecasting</b> assumes the next value will be equal to the most recent observed value.",
        "It is useful as a simple baseline model."
      ),
      
      "Seasonal Naive" = paste(
        "<b>Seasonal naive forecasting</b> assumes future values repeat the value from the same season in the previous cycle.",
        "It is useful when strong repeating seasonal patterns exist."
      )
    )
    
    HTML(paste0(
      "<div style='padding:12px; background:#f8f9fa; border-left:5px solid #2c7fb8;'>",
      explanation,
      "</div>"
    ))
  })
  
  output$decomposition_plot <- renderPlot({
    data <- selected_site_series()
    
    freq <- get_frequency(input$aggregation)
    
    validate(
      need(freq > 1, "Decomposition is not useful for yearly data."),
      need(nrow(data) >= freq * 2, "Not enough data for decomposition.")
    )
    
    ts_data <- ts(
      data$avg_value,
      frequency = freq
    )
    
    autoplot(stl(ts_data, s.window = "periodic")) +
      theme_minimal(base_size = 13) +
      labs(
        title = paste("Time Series Decomposition:", input$pollutant),
        subtitle = "Observed data separated into seasonal, trend and remainder components"
      ) +
      theme(
        plot.title = element_text(face = "bold")
      )
  })
  
  output$accuracy_table <- renderDT({
    data <- selected_site_series()
    
    validate(
      need(nrow(data) > 20, "Not enough data for accuracy testing.")
    )
    
    freq <- get_frequency(input$aggregation)
    
    ts_data <- ts(
      data$avg_value,
      frequency = freq
    )
    
    train_size <- floor(length(ts_data) * 0.8)
    
    train <- ts_data[1:train_size]
    test <- ts_data[(train_size + 1):length(ts_data)]
    
    train_ts <- ts(train, frequency = freq)
    
    fc <- build_forecast(
      ts_data = train_ts,
      model_type = input$model_type,
      horizon = length(test)
    )
    
    acc <- accuracy(fc, test)
    
    acc_df <- as.data.frame(acc) %>%
      tibble::rownames_to_column("Data") %>%
      select(Data, ME, RMSE, MAE, MAPE)
    
    datatable(
      round_df(acc_df),
      options = list(
        pageLength = 5,
        dom = "t"
      ),
      rownames = FALSE
    )
  })
  
  output$summary_table <- renderDT({
    data <- aggregated_data()
    
    summary_df <- data %>%
      group_by(site) %>%
      summarise(
        Mean = mean(avg_value, na.rm = TRUE),
        SD = sd(avg_value, na.rm = TRUE),
        Minimum = min(avg_value, na.rm = TRUE),
        Maximum = max(avg_value, na.rm = TRUE),
        Observations = n(),
        .groups = "drop"
      )
    
    datatable(
      round_df(summary_df),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
}

round_df <- function(data, digits = 3) {
  data %>%
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, digits)
      )
    )
}

shinyApp(ui = ui, server = server)