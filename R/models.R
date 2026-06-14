library(forecast)

#' Split Time Series Into Train and Test Sets
#'
#' @param y ts. Time series object.
#' @param train_ratio numeric. Proportion of data to use for training (default: 0.6).
#' @return list. Contains 'train' and 'test' ts objects, and forecast horizon 'h'.
split_train_test <- function(y, train_ratio = 0.6) {
  if (!is.ts(y)) {
    stop("Input must be a 'ts' object.")
  }
  n <- length(y)
  n_train <- round(n * train_ratio)
  h <- n - n_train
  
  # Ensure the time series retains its frequency structure
  freq <- frequency(y)
  start_y <- start(y)
  
  train_data <- ts(y[1:n_train], frequency = freq, start = start_y)
  
  # Calculate start for test data
  test_start_time <- start_y[1] + (n_train / freq)
  test_data <- ts(y[(n_train + 1):n], frequency = freq, start = test_start_time)
  
  return(list(
    train = train_data,
    test = test_data,
    h = h
  ))
}

#' Fit Seasonal Naive Model
#'
#' @param train ts. Training time series.
#' @param h integer. Forecast horizon.
#' @return forecast. Forecast object from snaive.
fit_snaive_model <- function(train, h) {
  tryCatch({
    snaive(train, h = h)
  }, error = function(e) {
    stop("Error fitting Seasonal Naive model: ", e$message)
  })
}

#' Fit ETS (Exponential Smoothing) Model
#'
#' @param train ts. Training time series.
#' @param h integer. Forecast horizon.
#' @return forecast. Forecast object from ets.
fit_ets_model <- function(train, h) {
  tryCatch({
    model <- ets(train)
    forecast(model, h = h)
  }, error = function(e) {
    stop("Error fitting ETS model: ", e$message)
  })
}

#' Fit Holt-Winters Model
#'
#' @param train ts. Training time series.
#' @param h integer. Forecast horizon.
#' @param seasonal character. Type of seasonal component ("additive" or "multiplicative").
#' @return forecast. Forecast object from HoltWinters.
fit_holtwinters_model <- function(train, h, seasonal = "additive") {
  tryCatch({
    # HoltWinters requires at least 2 periods of data and can fail on multiplicative with zero/negative values
    model <- HoltWinters(train, seasonal = seasonal)
    forecast(model, h = h)
  }, error = function(e) {
    # Fallback to additive if multiplicative fails, or propagate error
    if (seasonal == "multiplicative") {
      warning("Multiplicative Holt-Winters failed. Retrying with additive.")
      model <- HoltWinters(train, seasonal = "additive")
      return(forecast(model, h = h))
    }
    stop("Error fitting Holt-Winters model: ", e$message)
  })
}

#' Fit Auto ARIMA Model
#'
#' @param train ts. Training time series.
#' @param h integer. Forecast horizon.
#' @return forecast. Forecast object from auto.arima.
fit_arima_model <- function(train, h) {
  tryCatch({
    model <- auto.arima(train)
    forecast(model, h = h)
  }, error = function(e) {
    stop("Error fitting Auto-ARIMA model: ", e$message)
  })
}

#' Get Accuracy Metrics
#'
#' Evaluates forecast performance on test data.
#'
#' @param fc forecast. Forecast object.
#' @param test ts. Test time series data.
#' @return matrix. Accuracy metrics.
evaluate_model_accuracy <- function(fc, test) {
  # returns accuracy metrics for training and test data
  acc <- accuracy(fc, test)
  return(acc)
}
