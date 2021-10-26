log_returns_tbl <- readRDS("00_data/cci30.rds")

# * Visualize ----
log_returns_tbl %>%
  plot_time_series(
    .date_var = date_col
    , .value  = value
    , .smooth = FALSE
  )

# * ACF / PACF -----
# - Date Features & Fourier Series 
log_returns_tbl %>%
  plot_acf_diagnostics(.date_var = date_col, .value = value)

log_returns_tbl %>%
  plot_acf_diagnostics(.date_var = date_col, .value = value, .lags = 28)

# 3.0 SEASONALITY ----
# - Detecting Time-Based Features

log_returns_tbl %>%
  plot_seasonal_diagnostics(.date_var = date_col, .value = value)

# 4.0 ANOMALIES ----
# - Detecting Events & Possible Data Issues

log_returns_tbl %>%
  plot_anomaly_diagnostics(
    .date_var = date_col
    , .value  = value
    , .alpha  = 0.01
    , .max_anomalies = 0.01
  )

log_returns_tbl %>%
  tk_anomaly_diagnostics(
    .date_var = date_col
    , .value  = value
  )

# 5.0 SEASONAL DECOMPOSITION ----
# - Detecting Trend and Seasonal Cycles

log_returns_tbl %>%
  plot_stl_diagnostics(
    .date_var = date_col
    , .value  = value
  )


# 6.0 TIME SERIES REGRESSION PLOT ----
# - Finding features

log_returns_tbl %>%
  plot_time_series_regression(
    .date_var  = date_col
    , .formula = value ~ as.numeric(date_col)
    + week(date_col)
    + month(date_col, label = TRUE)
    , .show_summary = TRUE
  )

