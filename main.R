#!/usr/bin/env Rscript

# Command-line interface for the Time Series Forecasting Framework
# Sourcing packages and local modules
source("R/data_loader.R")
source("R/models.R")
source("R/plots.R")

library(rmarkdown)

# Simple command-line argument parser
args <- commandArgs(trailingOnly = TRUE)

print_help <- function() {
  cat("
Time Series Forecasting CLI Tool
Usage:
  Rscript main.R [options]

Options:
  --data=PATH         Path to the CSV file (default: data/daily-total-female-births.csv)
  --ratio=NUM         Train-test split ratio (default: 0.6)
  --freq=NUM          Frequency of the seasonal cycle (default: 7)
  --horizon=NUM       Forecast horizon for final prediction (default: 154)
  --report=PATH       Path to save the generated HTML report (default: report.html)
  --help, -h          Print this help message

Examples:
  Rscript main.R
  Rscript main.R --ratio=0.8 --freq=12 --horizon=30 --report=output_report.html
\n")
}

# Default values
data_path <- "data/daily-total-female-births.csv"
train_ratio <- 0.6
frequency <- 7
forecast_horizon <- 154
report_output <- "report.html"

# Parse arguments
for (arg in args) {
  if (arg == "--help" || arg == "-h") {
    print_help()
    quit(save = "no", status = 0)
  } else if (grepl("^--data=", arg)) {
    data_path <- sub("^--data=", "", arg)
  } else if (grepl("^--ratio=", arg)) {
    train_ratio <- as.numeric(sub("^--ratio=", "", arg))
  } else if (grepl("^--freq=", arg)) {
    frequency <- as.numeric(sub("^--freq=", "", arg))
  } else if (grepl("^--horizon=", arg)) {
    forecast_horizon <- as.numeric(sub("^--horizon=", "", arg))
  } else if (grepl("^--report=", arg)) {
    report_output <- sub("^--report=", "", arg)
  } else {
    warning("Unknown argument: ", arg)
    print_help()
    quit(save = "no", status = 1)
  }
}

cat("=========================================\n")
cat("Starting Time Series Forecasting Pipeline\n")
cat("=========================================\n")
cat("Data Path:       ", data_path, "\n")
cat("Split Ratio:     ", train_ratio, "\n")
cat("Frequency:       ", frequency, "\n")
cat("Forecast Horizon:", forecast_horizon, "\n")
cat("Output Report:   ", report_output, "\n")
cat("-----------------------------------------\n")

# 1. Download & Load Data
if (data_path == "data/daily-total-female-births.csv") {
  download_default_data(data_path)
}

if (!file.exists(data_path)) {
  stop("Error: Data file '", data_path, "' does not exist.")
}

cat("Loading and cleaning data...\n")
df <- load_and_clean_data(data_path)
y <- create_ts_object(df, frequency = frequency)

# 2. Train-Test Split
cat("Splitting data into training and testing...\n")
split <- split_train_test(y, train_ratio = train_ratio)
train <- split$train
test <- split$test
h <- split$h

cat("Training length:", length(train), "\n")
cat("Test length:    ", length(test), "\n\n")

# 3. Model Training & Evaluation
cat("Fitting SNAIVE, ETS, Holt-Winters, and Auto-ARIMA models...\n")
fc_snaive <- fit_snaive_model(train, h = h)
fc_ets <- fit_ets_model(train, h = h)
fc_hw <- fit_holtwinters_model(train, h = h, seasonal = "additive")
fc_arima <- fit_arima_model(train, h = h)

acc_snaive <- evaluate_model_accuracy(fc_snaive, test)
acc_ets <- evaluate_model_accuracy(fc_ets, test)
acc_hw <- evaluate_model_accuracy(fc_hw, test)
acc_arima <- evaluate_model_accuracy(fc_arima, test)

# 4. Summary Output
cat("\nModel Accuracy Comparison (Test Set):\n")
cat(sprintf("%-15s | %-8s | %-8s | %-8s | %-8s\n", "Model", "RMSE", "MAE", "MAPE", "MASE"))
cat(paste(rep("-", 55), collapse = ""), "\n")

print_metric <- function(name, acc) {
  cat(sprintf("%-15s | %-8.3f | %-8.3f | %-8.3f | %-8.3f\n", 
              name, 
              acc["Test set", "RMSE"], 
              acc["Test set", "MAE"], 
              acc["Test set", "MAPE"], 
              acc["Test set", "MASE"]))
}

print_metric("SNAIVE", acc_snaive)
print_metric("ETS", acc_ets)
print_metric("Holt-Winters", acc_hw)
print_metric("Auto-ARIMA", acc_arima)
cat(paste(rep("-", 55), collapse = ""), "\n\n")

# 5. Report Compilation
cat("Generating RMarkdown HTML report...\n")
tryCatch({
  render(
    input = "report.Rmd",
    output_file = report_output,
    params = list(
      data_path = data_path,
      train_ratio = train_ratio,
      frequency = frequency,
      forecast_horizon = forecast_horizon
    ),
    quiet = TRUE
  )
  cat("Report successfully compiled and saved to:", report_output, "\n")
}, error = function(e) {
  cat("Error generating report: ", e$message, "\n")
})

cat("=========================================\n")
cat("Pipeline execution completed successfully!\n")
cat("=========================================\n")
