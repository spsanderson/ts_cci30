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

?plot_anomaly_diagnostics

subscribers_day_tbl %>%
  plot_anomaly_diagnostics(
    .date_var = optin_time
    , .value = optins
    , .alpha = 0.01
    , .max_anomalies = 0.01
  )

subscribers_day_tbl %>%
  tk_anomaly_diagnostics(
    .date_var = optin_time
    , .value = optins
  )

# Grouped Anomalies
google_analytics_long_hour_tbl %>%
  group_by(name) %>%
  plot_anomaly_diagnostics(
    .date_var = date
    , .value = value
  )