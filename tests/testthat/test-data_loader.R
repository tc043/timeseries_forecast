library(testthat)

test_that("download_default_data works correctly", {
  # We can check that the download function creates a file and returns TRUE/FALSE properly
  temp_file <- tempfile(fileext = ".csv")
  res <- download_default_data(dest_path = temp_file)
  expect_true(res)
  expect_true(file.exists(temp_file))
  unlink(temp_file)
})

test_that("load_and_clean_data parses data correctly", {
  # Write a small temporary CSV
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    Date = c("1959-01-01", "1959-01-02", "1959-01-03"),
    Births = c("35", "32", "30")
  ), file = temp_file, row.names = FALSE)
  
  df <- load_and_clean_data(temp_file)
  
  expect_equal(colnames(df)[1:2], c("Date", "Value"))
  expect_equal(nrow(df), 3)
  expect_s3_class(df$Date, "Date")
  expect_type(df$Value, "double")
  
  # Error cases
  expect_error(load_and_clean_data("nonexistent_file.csv"))
  
  unlink(temp_file)
})

test_that("create_ts_object creates correct ts", {
  df <- data.frame(
    Date = as.Date(c("1959-01-01", "1959-01-02", "1959-01-03")),
    Value = c(35, 32, 30)
  )
  y <- create_ts_object(df, frequency = 7)
  
  expect_true(is.ts(y))
  expect_equal(frequency(y), 7)
  expect_equal(length(y), 3)
})
