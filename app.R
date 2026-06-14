library(shiny)
library(bslib)
library(plotly)
library(ggplot2)
library(forecast)
library(tseries)

# Source modules
source("R/data_loader.R")
source("R/models.R")
source("R/plots.R")

# Preload default dataset to ensure instant availability
download_default_data()

# Premium CSS for custom glassmorphism and modern UI elements
custom_css <- "
  body {
    background: radial-gradient(circle at top right, #1e293b, #0f172a);
    font-family: 'Inter', sans-serif;
  }
  .card {
    border-radius: 12px;
    background: rgba(30, 41, 59, 0.7) !important;
    backdrop-filter: blur(10px);
    border: 1px solid rgba(255, 255, 255, 0.05);
    box-shadow: 0 4px 30px rgba(0, 0, 0, 0.2);
  }
  .card-header {
    background: rgba(15, 23, 42, 0.8) !important;
    border-bottom: 1px solid rgba(255, 255, 255, 0.05) !important;
    font-weight: bold;
    color: #e2e8f0;
  }
  .well {
    background: rgba(15, 23, 42, 0.6) !important;
    border: 1px solid rgba(255, 255, 255, 0.05) !important;
    border-radius: 12px;
  }
  .nav-pills .nav-link.active {
    background-color: #3b82f6 !important;
    box-shadow: 0 4px 14px rgba(59, 130, 246, 0.4);
  }
  .nav-link {
    color: #94a3b8 !important;
    font-weight: 500;
  }
  .nav-link:hover {
    color: #f1f5f9 !important;
  }
  .value-box {
    border-radius: 12px;
    background: linear-gradient(135deg, rgba(59, 130, 246, 0.1), rgba(37, 99, 235, 0.2));
    border: 1px solid rgba(59, 130, 246, 0.2);
    padding: 15px;
    text-align: center;
    color: #ffffff;
  }
  .value-box-number {
    font-size: 24px;
    font-weight: 800;
    color: #3b82f6;
  }
  .value-box-title {
    font-size: 12px;
    color: #94a3b8;
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }
  .btn-primary {
    background-color: #3b82f6 !important;
    border-color: #2563eb !important;
    font-weight: 600;
    border-radius: 8px;
    transition: all 0.2s ease-in-out;
  }
  .btn-primary:hover {
    background-color: #2563eb !important;
    box-shadow: 0 4px 14px rgba(59, 130, 246, 0.4);
    transform: translateY(-1px);
  }
  .tab-content, .tab-pane {
    overflow: visible !important;
  }
  .card-body {
    overflow: visible !important;
  }
"

# Shiny UI definition
ui <- page_sidebar(
  title = span(icon("chart-line"), " Chronos: Time Series Analysis & Forecasting", style = "font-weight: 800; color: #f8fafc;"),
  fillable = FALSE,
  theme = bs_theme(
    version = 5,
    bg = "#0f172a",
    fg = "#e2e8f0",
    primary = "#3b82f6",
    secondary = "#64748b",
    base_font = font_google("Inter")
  ),
  header = tags$head(
    tags$style(HTML(custom_css)),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap")
  ),
  
  sidebar = sidebar(
    title = "Controls & Parameters",
    width = 320,
    bg = "rgba(15, 23, 42, 0.95)",
    
    radioButtons("data_source", "Data Source",
                 choices = c("Default Dataset (CA Female Births)" = "default",
                             "Upload Custom CSV File" = "custom")),
    
    conditionalPanel(
      condition = "input.data_source == 'custom'",
      fileInput("csv_file", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
    ),
    
    sliderInput("train_ratio", "Training Set Ratio",
                min = 0.4, max = 0.9, value = 0.7, step = 0.05),
    
    numericInput("frequency", "Seasonal Frequency",
                 value = 7, min = 1, step = 1),
    
    numericInput("horizon", "Forecast Horizon (Steps)",
                 value = 30, min = 1, step = 1),
    
    hr(style = "border-color: rgba(255,255,255,0.1);"),
    
    actionButton("run_btn", "⚡ Run Forecasting", class = "btn-primary w-100")
  ),
  
  # Dashboard Layout
  # Row 1: Value Boxes
  layout_column_wrap(
    width = 1/3,
    div(class = "value-box",
        div(class = "value-box-title", "Total Observations"),
        uiOutput("val_obs")
    ),
    div(class = "value-box",
        div(class = "value-box-title", "Series Average"),
        uiOutput("val_avg")
    ),
    div(class = "value-box",
        div(class = "value-box-title", "ADF Stationarity P-Value"),
        uiOutput("val_adf")
    )
  ),
  
  # Row 2: Tabs with visualizations and parameters
  navset_card_pill(
    title = "Analysis Tabs",
    
    nav_panel(
      title = "Data Overview",
      icon = icon("table"),
      card(
        card_header("Historical Time Series Plot"),
        plotlyOutput("ts_plot", height = "400px")
      ),
      card(
        card_header("Data Summary & Stationarity"),
        layout_column_wrap(
          width = 1/2,
          div(
            h5("Descriptive Statistics", style = "color: #3b82f6; font-weight: bold;"),
            tableOutput("summary_table")
          ),
          div(
            h5("Augmented Dickey-Fuller (ADF) Test", style = "color: #3b82f6; font-weight: bold;"),
            verbatimTextOutput("adf_output"),
            uiOutput("adf_interpretation")
          )
        )
      )
    ),
    
    nav_panel(
      title = "Exploratory Analysis",
      icon = icon("compass"),
      card(
        card_header("Seasonal and Trend Decomposition (STL)"),
        plotlyOutput("decomp_plot", height = "550px")
      ),
      card(
        card_header("Autocorrelation Diagnostics"),
        layout_column_wrap(
          width = 1/2,
          plotlyOutput("acf_plot", height = "350px"),
          plotlyOutput("pacf_plot", height = "350px")
        )
      )
    ),
    
    nav_panel(
      title = "Model Benchmarking",
      icon = icon("gauge-high"),
      card(
        card_header("Model Selection"),
        selectInput("selected_model", "Choose Forecasting Model for Visual Comparison",
                    choices = c("Seasonal Naive (SNAIVE)" = "snaive",
                                "Exponential Smoothing (ETS)" = "ets",
                                "Holt-Winters" = "hw",
                                "Auto-ARIMA" = "arima"))
      ),
      card(
        card_header("Forecast vs Actuals (Validation Holdout)"),
        plotlyOutput("forecast_plot", height = "400px")
      ),
      card(
        card_header("Test Holdout Set Metrics Comparison"),
        tableOutput("accuracy_table")
      )
    ),
    
    nav_panel(
      title = "Residual Diagnostics",
      icon = icon("flask"),
      card(
        card_header("Residual Diagnosis Plots"),
        layout_column_wrap(
          width = 1/2,
          plotlyOutput("res_line_plot", height = "350px"),
          plotlyOutput("res_acf_plot", height = "350px")
        )
      ),
      card(
        card_header("Independence & Residual Normality"),
        layout_column_wrap(
          width = 1/2,
          plotlyOutput("res_hist_plot", height = "350px"),
          div(
            h5("Ljung-Box Test of Residuals", style = "color: #3b82f6; font-weight: bold;"),
            verbatimTextOutput("ljung_box_output"),
            uiOutput("ljung_box_interpretation")
          )
        )
      )
    ),
    
    nav_panel(
      title = "Export Forecasts",
      icon = icon("download"),
      card(
        card_header("Export & Predictions Preview"),
        p("Download the final forecast output generated by the Auto-ARIMA model fitted on all historical data."),
        downloadButton("download_fc", "💾 Download Forecast CSV", class = "btn-success"),
        hr(style = "border-color: rgba(255,255,255,0.1);"),
        tableOutput("forecast_preview")
      )
    )
  )
)

# Shiny Server logic
server <- function(input, output, session) {
  
  # Reactive value container
  state <- reactiveValues(
    raw_df = NULL,
    y = NULL,
    train = NULL,
    test = NULL,
    h = 30,
    fc_snaive = NULL,
    fc_ets = NULL,
    fc_hw = NULL,
    fc_arima = NULL,
    accuracy_df = NULL
  )
  
  # Reactive function to process file input and parameters
  process_pipeline <- function() {
    # 1. Load Data
    if (input$data_source == "default") {
      file_path <- "data/daily-total-female-births.csv"
      if (!file.exists(file_path)) {
        download_default_data(file_path)
      }
    } else {
      req(input$csv_file)
      file_path <- input$csv_file$datapath
    }
    
    # Load and clean
    tryCatch({
      df <- load_and_clean_data(file_path)
      y <- create_ts_object(df, frequency = input$frequency)
      
      # Train-test split
      split <- split_train_test(y, train_ratio = input$train_ratio)
      
      # Fit validation models
      fc_snaive <- fit_snaive_model(split$train, h = split$h)
      fc_ets <- fit_ets_model(split$train, h = split$h)
      fc_hw <- fit_holtwinters_model(split$train, h = split$h, seasonal = "additive")
      fc_arima <- fit_arima_model(split$train, h = split$h)
      
      # Accuracy comparison table
      acc_snaive <- evaluate_model_accuracy(fc_snaive, split$test)
      acc_ets <- evaluate_model_accuracy(fc_ets, split$test)
      acc_hw <- evaluate_model_accuracy(fc_hw, split$test)
      acc_arima <- evaluate_model_accuracy(fc_arima, split$test)
      
      accuracy_df <- data.frame(
        Model = c("SNAIVE", "ETS", "Holt-Winters", "Auto-ARIMA"),
        RMSE = c(acc_snaive["Test set", "RMSE"], acc_ets["Test set", "RMSE"], acc_hw["Test set", "RMSE"], acc_arima["Test set", "RMSE"]),
        MAE = c(acc_snaive["Test set", "MAE"], acc_ets["Test set", "MAE"], acc_hw["Test set", "MAE"], acc_arima["Test set", "MAE"]),
        MAPE = c(acc_snaive["Test set", "MAPE"], acc_ets["Test set", "MAPE"], acc_hw["Test set", "MAPE"], acc_arima["Test set", "MAPE"]),
        MASE = c(acc_snaive["Test set", "MASE"], acc_ets["Test set", "MASE"], acc_hw["Test set", "MASE"], acc_arima["Test set", "MASE"])
      )
      
      # Save state
      state$raw_df <- df
      state$y <- y
      state$train <- split$train
      state$test <- split$test
      state$h <- split$h
      state$fc_snaive <- fc_snaive
      state$fc_ets <- fc_ets
      state$fc_hw <- fc_hw
      state$fc_arima <- fc_arima
      state$accuracy_df <- accuracy_df
      
    }, error = function(e) {
      showNotification(paste("Pipeline Error:", e$message), type = "error", duration = NULL)
    })
  }
  
  # Trigger processing on startup
  observe({
    process_pipeline()
  })
  
  # Trigger processing on button press
  observeEvent(input$run_btn, {
    process_pipeline()
  })
  
  # --- Value Boxes ---
  output$val_obs <- renderUI({
    req(state$y)
    div(class = "value-box-number", length(state$y))
  })
  
  output$val_avg <- renderUI({
    req(state$y)
    div(class = "value-box-number", round(mean(state$y), 2))
  })
  
  output$val_adf <- renderUI({
    req(state$y)
    res <- adf.test(state$y)
    div(class = "value-box-number", round(res$p.value, 4))
  })
  
  # --- Tab 1: Data Overview ---
  output$ts_plot <- renderPlotly({
    req(state$y)
    p <- plot_time_series(state$y, "Time Series History")
    ggplotly(p, tooltip = "text") %>% layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$summary_table <- renderTable({
    req(state$y)
    stats <- as.matrix(summary(state$y))
    df_stats <- data.frame(Metric = rownames(stats), Value = as.vector(stats))
    df_stats
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$adf_output <- renderPrint({
    req(state$y)
    adf.test(state$y)
  })
  
  output$adf_interpretation <- renderUI({
    req(state$y)
    res <- adf.test(state$y)
    if (res$p.value < 0.05) {
      p("✅ The series is stationary (p-value < 0.05). Differencing may not be required for ARIMA fitting.", style = "color: #10b981; font-weight: 500;")
    } else {
      p("⚠️ The series is non-stationary (p-value >= 0.05). Integrated differencing (d > 0) is recommended.", style = "color: #f59e0b; font-weight: 500;")
    }
  })
  
  # --- Tab 2: Decomposition & Autocorrelations ---
  output$decomp_plot <- renderPlotly({
    req(state$y)
    p <- plot_decomposition(state$y)
    ggplotly(p, tooltip = "text") %>% layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$acf_plot <- renderPlotly({
    req(state$y)
    acf_list <- plot_acf_pacf(state$y)
    ggplotly(acf_list$acf) %>% layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$pacf_plot <- renderPlotly({
    req(state$y)
    acf_list <- plot_acf_pacf(state$y)
    ggplotly(acf_list$pacf) %>% layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  # --- Tab 3: Model Training ---
  current_forecast_obj <- reactive({
    req(input$selected_model)
    switch(input$selected_model,
           "snaive" = state$fc_snaive,
           "ets" = state$fc_ets,
           "hw" = state$fc_hw,
           "arima" = state$fc_arima)
  })
  
  output$forecast_plot <- renderPlotly({
    fc <- current_forecast_obj()
    req(fc)
    p <- plot_forecast_vs_actual(fc, state$test, toupper(input$selected_model))
    ggplotly(p, tooltip = "text") %>% layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$accuracy_table <- renderTable({
    req(state$accuracy_df)
    state$accuracy_df
  }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 3)
  
  # --- Tab 4: Residual Diagnostics ---
  output$res_line_plot <- renderPlotly({
    fc <- current_forecast_obj()
    req(fc)
    res_list <- plot_residuals_diagnostics(fc)
    ggplotly(res_list$line) %>% layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$res_acf_plot <- renderPlotly({
    fc <- current_forecast_obj()
    req(fc)
    res_list <- plot_residuals_diagnostics(fc)
    ggplotly(res_list$acf) %>% layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$res_hist_plot <- renderPlotly({
    fc <- current_forecast_obj()
    req(fc)
    res_list <- plot_residuals_diagnostics(fc)
    ggplotly(res_list$hist) %>% layout(plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$ljung_box_output <- renderPrint({
    fc <- current_forecast_obj()
    req(fc)
    # Get the raw model object if possible
    model <- if (!is.null(fc$model)) fc$model else residuals(fc)
    Box.test(residuals(fc), lag = input$frequency * 2, type = "Ljung-Box")
  })
  
  output$ljung_box_interpretation <- renderUI({
    fc <- current_forecast_obj()
    req(fc)
    res <- Box.test(residuals(fc), lag = input$frequency * 2, type = "Ljung-Box")
    if (res$p.value > 0.05) {
      p("✅ No significant residual autocorrelation (p-value > 0.05). Residuals mimic white noise.", style = "color: #10b981; font-weight: 500;")
    } else {
      p("⚠️ Significant residual autocorrelation present (p-value <= 0.05). Consider adjusting model lags or specifications.", style = "color: #f59e0b; font-weight: 500;")
    }
  })
  
  # --- Tab 5: Export Forecasts ---
  final_forecast <- reactive({
    req(state$y)
    # Fit ARIMA model to all data for production forecasting
    fit <- auto.arima(state$y)
    fc <- forecast(fit, h = input$horizon)
    fc
  })
  
  forecast_data_frame <- reactive({
    fc <- final_forecast()
    req(fc)
    
    # Calculate time stamps
    time_seq <- seq(from = length(state$y) + 1, length.out = input$horizon)
    
    data.frame(
      Time_Step = time_seq,
      Point_Forecast = round(as.numeric(fc$mean), 2),
      Lo_80 = round(as.numeric(fc$lower[, 1]), 2),
      Hi_80 = round(as.numeric(fc$upper[, 1]), 2),
      Lo_95 = round(as.numeric(fc$lower[, 2]), 2),
      Hi_95 = round(as.numeric(fc$upper[, 2]), 2)
    )
  })
  
  output$forecast_preview <- renderTable({
    req(forecast_data_frame())
    head(forecast_data_frame(), 15)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$download_fc <- downloadHandler(
    filename = function() {
      paste("forecasts-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(forecast_data_frame(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
