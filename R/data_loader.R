#' Download Default Time Series Data
#'
#' Downloads the daily female births dataset from GitHub if it is not already present.
#'
#' @param dest_path character. Path where the dataset should be saved.
#' @return logical. TRUE if file exists or is successfully downloaded, FALSE otherwise.
download_default_data <- function(dest_path = "data/daily-total-female-births.csv") {
  if (file.exists(dest_path)) {
    message("Default dataset already exists at: ", dest_path)
    return(TRUE)
  }
  
  # Create directory if it doesn't exist
  dir_name <- dirname(dest_path)
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, recursive = TRUE)
  }
  
  url <- "https://raw.githubusercontent.com/jbrownlee/Datasets/master/daily-total-female-births.csv"
  message("Downloading dataset from: ", url)
  
  tryCatch({
    download.file(url, destfile = dest_path, method = "auto", quiet = TRUE)
    if (file.exists(dest_path)) {
      message("Successfully downloaded dataset to: ", dest_path)
      return(TRUE)
    }
  }, error = function(e) {
    warning("Failed to download dataset: ", e$message)
  })
  
  return(FALSE)
}

#' Load and Clean Time Series Data
#'
#' Reads a CSV file containing date and value columns, and performs quality checks.
#'
#' @param file_path character. Path to the CSV file.
#' @return data.frame. Cleaned data frame with 'Date' and 'Value' columns.
load_and_clean_data <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File not found at: ", file_path)
  }
  
  # Read CSV
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Validation checks
  if (ncol(df) < 2) {
    stop("Input data must have at least 2 columns (Date and Value).")
  }
  
  # Standardize column names
  colnames(df)[1:2] <- c("Date", "Value")
  
  # Clean columns
  df$Value <- as.numeric(df$Value)
  
  # Check for NAs
  if (any(is.na(df$Value))) {
    warning("Missing or non-numeric values found. Removing NA rows.")
    df <- df[!is.na(df$Value), ]
  }
  
  # Check for date parsing
  df$Date <- as.Date(df$Date)
  if (any(is.na(df$Date))) {
    warning("Some dates could not be parsed. Checking row index representation.")
  }
  
  return(df)
}

#' Create R Time Series Object
#'
#' Converts a data frame column to a ts class object.
#'
#' @param df data.frame. Cleaned data frame from load_and_clean_data.
#' @param frequency numeric. Frequency of the time series (e.g. 7 for weekly seasonality).
#' @return ts. Time series object.
create_ts_object <- function(df, frequency = 7) {
  if (!"Value" %in% colnames(df)) {
    stop("Data frame must contain a 'Value' column.")
  }
  
  # Create ts object starting from 1 (or specific start date index if desired)
  # For daily-total-female-births, daily data with weekly seasonality (frequency = 7)
  # starting at 1959-01-01
  ts_obj <- ts(df$Value, frequency = frequency)
  return(ts_obj)
}
