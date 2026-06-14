library(ggplot2)
library(forecast)

# Define a clean, premium color palette for charts
THEME_COLORS <- list(
  primary = "#2c3e50",
  secondary = "#18bc9c",
  accent = "#e74c3c",
  light_bg = "#ecf0f1",
  grid_line = "#e2e8f0",
  confidence_95 = "rgba(44, 62, 80, 0.15)",
  confidence_80 = "rgba(44, 62, 80, 0.3)"
)

#' Modern ggplot2 Theme for the Project
#'
#' @return ggplot theme.
theme_premium <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 14, color = THEME_COLORS$primary, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 11, color = "#7f8c8d", margin = margin(b = 15)),
      axis.title = element_text(face = "bold", size = 10, color = THEME_COLORS$primary),
      axis.text = element_text(size = 9, color = "#555555"),
      panel.grid.major = element_line(color = THEME_COLORS$grid_line, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      plot.margin = margin(15, 15, 15, 15),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 9)
    )
}

#' Plot Time Series Data
#'
#' @param y ts. Time series object.
#' @param title character. Plot title.
#' @return ggplot object.
plot_time_series <- function(y, title = "Time Series Data") {
  time_idx <- time(y)
  df <- data.frame(
    Time = as.numeric(time_idx),
    Value = as.numeric(y)
  )
  
  ggplot(df, aes(x = Time, y = Value)) +
    geom_line(color = THEME_COLORS$primary, linewidth = 0.8) +
    geom_point(color = THEME_COLORS$secondary, size = 1.2, alpha = 0.8) +
    labs(
      title = title,
      x = "Time Period / Frequency Cycles",
      y = "Value"
    ) +
    theme_premium()
}

#' Plot ACF and PACF Side-by-Side
#'
#' @param y ts. Time series object.
#' @return ggplot object (grid).
plot_acf_pacf <- function(y) {
  # Calculate ACF
  acf_res <- acf(y, plot = FALSE)
  df_acf <- data.frame(Lag = acf_res$lag[-1, 1, 1], ACF = acf_res$acf[-1, 1, 1])
  
  # Calculate PACF
  pacf_res <- pacf(y, plot = FALSE)
  df_pacf <- data.frame(Lag = pacf_res$lag[, 1, 1], PACF = pacf_res$acf[, 1, 1])
  
  # Significant boundary
  ci <- 1.96 / sqrt(length(y))
  
  p1 <- ggplot(df_acf, aes(x = Lag, y = ACF)) +
    geom_segment(aes(xend = Lag, yend = 0), color = THEME_COLORS$primary, linewidth = 0.8) +
    geom_point(color = THEME_COLORS$secondary, size = 1.5) +
    geom_hline(yintercept = c(-ci, ci), color = THEME_COLORS$accent, linetype = "dashed", alpha = 0.7) +
    geom_hline(yintercept = 0, color = "grey50") +
    labs(title = "Autocorrelation (ACF)", x = "Lag", y = "ACF") +
    theme_premium()
  
  p2 <- ggplot(df_pacf, aes(x = Lag, y = PACF)) +
    geom_segment(aes(xend = Lag, yend = 0), color = THEME_COLORS$primary, linewidth = 0.8) +
    geom_point(color = THEME_COLORS$secondary, size = 1.5) +
    geom_hline(yintercept = c(-ci, ci), color = THEME_COLORS$accent, linetype = "dashed", alpha = 0.7) +
    geom_hline(yintercept = 0, color = "grey50") +
    labs(title = "Partial Autocorrelation (PACF)", x = "Lag", y = "PACF") +
    theme_premium()
  
  return(list(acf = p1, pacf = p2))
}

#' Plot Time Series Decomposition
#'
#' @param y ts. Time series object.
#' @return ggplot object.
plot_decomposition <- function(y) {
  decomp <- stl(y, s.window = "periodic")
  df <- data.frame(
    Time = as.numeric(time(y)),
    Observed = as.numeric(decomp$time.series[, "trend"] + decomp$time.series[, "seasonal"] + decomp$time.series[, "remainder"]),
    Trend = as.numeric(decomp$time.series[, "trend"]),
    Seasonal = as.numeric(decomp$time.series[, "seasonal"]),
    Remainder = as.numeric(decomp$time.series[, "remainder"])
  )
  
  df_long <- tidyr::pivot_longer(df, cols = -Time, names_to = "Component", values_to = "Value")
  df_long$Component <- factor(df_long$Component, levels = c("Observed", "Trend", "Seasonal", "Remainder"))
  
  ggplot(df_long, aes(x = Time, y = Value)) +
    geom_line(color = THEME_COLORS$primary, linewidth = 0.8) +
    facet_grid(Component ~ ., scales = "free_y") +
    labs(
      title = "STL Decomposition",
      x = "Time Period / Cycles",
      y = "Value"
    ) +
    theme_premium() +
    theme(strip.text.y = element_text(angle = 0, face = "bold", size = 10))
}

#' Plot Forecast vs Actuals
#'
#' Plots the forecast predictions, confidence intervals, and actual (test) data.
#'
#' @param fc forecast. Forecast object from models.
#' @param test ts. Test time series.
#' @param model_name character. Name of the forecasting model.
#' @return ggplot object.
plot_forecast_vs_actual <- function(fc, test = NULL, model_name = "Model") {
  # Convert training data to data frame
  train_time <- time(fc$x)
  df_train <- data.frame(
    Time = as.numeric(train_time),
    Value = as.numeric(fc$x),
    Type = "Training"
  )
  
  # Forecast data
  fc_time <- time(fc$mean)
  df_fc <- data.frame(
    Time = as.numeric(fc_time),
    Value = as.numeric(fc$mean),
    Type = "Forecast",
    Lo80 = as.numeric(fc$lower[, 1]),
    Hi80 = as.numeric(fc$upper[, 1]),
    Lo95 = as.numeric(fc$lower[, 2]),
    Hi95 = as.numeric(fc$upper[, 2])
  )
  
  # Append columns for confidence intervals to train
  df_train$Lo80 <- NA
  df_train$Hi80 <- NA
  df_train$Lo95 <- NA
  df_train$Hi95 <- NA
  
  # Combine data
  df_all <- rbind(df_train, df_fc)
  
  # Add test data if available
  if (!is.null(test)) {
    test_time <- time(test)
    df_test <- data.frame(
      Time = as.numeric(test_time),
      Value = as.numeric(test),
      Type = "Actual (Test)",
      Lo80 = NA, Hi80 = NA, Lo95 = NA, Hi95 = NA
    )
    df_all <- rbind(df_all, df_test)
  }
  
  # Plot
  p <- ggplot(df_all, aes(x = Time, y = Value, color = Type)) +
    # Confidence ribbon 95%
    geom_ribbon(data = subset(df_all, Type == "Forecast"),
                aes(ymin = Lo95, ymax = Hi95), fill = THEME_COLORS$primary, alpha = 0.1, color = NA) +
    # Confidence ribbon 80%
    geom_ribbon(data = subset(df_all, Type == "Forecast"),
                aes(ymin = Lo80, ymax = Hi80), fill = THEME_COLORS$primary, alpha = 0.2, color = NA) +
    # Lines
    geom_line(aes(linetype = Type), linewidth = 0.8) +
    # Custom colors and line types
    scale_color_manual(values = c(
      "Training" = THEME_COLORS$primary,
      "Forecast" = THEME_COLORS$accent,
      "Actual (Test)" = THEME_COLORS$secondary
    )) +
    scale_linetype_manual(values = c(
      "Training" = "solid",
      "Forecast" = "dashed",
      "Actual (Test)" = "solid"
    )) +
    labs(
      title = paste("Forecast evaluation -", model_name),
      subtitle = "Visual comparison of historical training, actual test data, and forecasts with 80% & 95% confidence intervals",
      x = "Time Period / Cycles",
      y = "Value"
    ) +
    theme_premium()
  
  return(p)
}

#' Plot Residual Analysis
#'
#' Generates plots to diagnose residuals: line plot, ACF, and histogram (normal curve).
#'
#' @param fc forecast. Forecast object.
#' @return list of ggplot objects.
plot_residuals_diagnostics <- function(fc) {
  res <- residuals(fc)
  res_numeric <- as.numeric(res)
  # Remove NAs which might appear in initial residuals
  res_numeric <- res_numeric[!is.na(res_numeric)]
  
  time_idx <- seq_along(res_numeric)
  df_res <- data.frame(Index = time_idx, Residual = res_numeric)
  
  # Residual line plot
  p1 <- ggplot(df_res, aes(x = Index, y = Residual)) +
    geom_line(color = THEME_COLORS$primary, linewidth = 0.6) +
    geom_hline(yintercept = 0, color = THEME_COLORS$accent, linetype = "dashed") +
    labs(title = "Residuals over Time", x = "Index", y = "Residual") +
    theme_premium()
  
  # ACF of residuals
  acf_res <- acf(res_numeric, plot = FALSE)
  df_acf <- data.frame(Lag = acf_res$lag[-1, 1, 1], ACF = acf_res$acf[-1, 1, 1])
  ci <- 1.96 / sqrt(length(res_numeric))
  p2 <- ggplot(df_acf, aes(x = Lag, y = ACF)) +
    geom_segment(aes(xend = Lag, yend = 0), color = THEME_COLORS$primary, linewidth = 0.8) +
    geom_hline(yintercept = c(-ci, ci), color = THEME_COLORS$accent, linetype = "dashed", alpha = 0.7) +
    geom_hline(yintercept = 0, color = "grey50") +
    labs(title = "ACF of Residuals", x = "Lag", y = "ACF") +
    theme_premium()
  
  # Histogram of residuals
  p3 <- ggplot(df_res, aes(x = Residual)) +
    geom_histogram(aes(y = after_stat(density)), bins = 20, fill = THEME_COLORS$secondary, color = "white", alpha = 0.7) +
    stat_function(fun = dnorm, args = list(mean = mean(res_numeric), sd = sd(res_numeric)),
                  color = THEME_COLORS$primary, linewidth = 1) +
    labs(title = "Distribution of Residuals", x = "Residual", y = "Density") +
    theme_premium()
  
  return(list(line = p1, acf = p2, hist = p3))
}
