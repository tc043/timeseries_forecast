library(testthat)
library(forecast)

test_that("split_train_test splits correct proportion", {
  y <- ts(rnorm(100), frequency = 7, start = 1)
  split <- split_train_test(y, train_ratio = 0.8)
  
  expect_equal(length(split$train), 80)
  expect_equal(length(split$test), 20)
  expect_equal(split$h, 20)
  expect_true(is.ts(split$train))
  expect_true(is.ts(split$test))
  expect_equal(frequency(split$train), 7)
  expect_equal(frequency(split$test), 7)
})

test_that("forecasting models fit and forecast correctly", {
  # Mock data
  y <- ts(rnorm(50) + seq(1, 50) * 0.1, frequency = 7, start = 1)
  split <- split_train_test(y, train_ratio = 0.8)
  h <- split$h
  
  # SNAIVE
  fc_snaive <- fit_snaive_model(split$train, h = h)
  expect_s3_class(fc_snaive, "forecast")
  expect_equal(length(fc_snaive$mean), h)
  
  # ETS
  fc_ets <- fit_ets_model(split$train, h = h)
  expect_s3_class(fc_ets, "forecast")
  expect_equal(length(fc_ets$mean), h)
  
  # Holt-Winters (additive)
  fc_hw <- fit_holtwinters_model(split$train, h = h, seasonal = "additive")
  expect_s3_class(fc_hw, "forecast")
  expect_equal(length(fc_hw$mean), h)
  
  # ARIMA
  fc_arima <- fit_arima_model(split$train, h = h)
  expect_s3_class(fc_arima, "forecast")
  expect_equal(length(fc_arima$mean), h)
})

test_that("evaluate_model_accuracy computes statistics", {
  y <- ts(rnorm(50), frequency = 7)
  split <- split_train_test(y, train_ratio = 0.8)
  fc <- fit_snaive_model(split$train, h = split$h)
  
  acc <- evaluate_model_accuracy(fc, split$test)
  expect_true(is.matrix(acc))
  expect_true("RMSE" %in% colnames(acc))
})
