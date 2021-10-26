# GOAL: Forecast Daily Log Returns - Next 12-WEEKS

# OBJECTIVES ----
# - Dive into a time-series analysis project
# - Experience Frameworks: modeltime
# - Experience 2 Algorithms:
#   1. Prophet
#   2. LM w/ Engineered Features

# LIBRARIES ----
source("00_scripts/lib_load.R")
source("00_scripts/data_load.R")

# DATA -----
cci_index_tbl <- read_csv("00_data/cci30_OHLCV.csv") %>%
  clean_names()


# 1.0 EDA & DATA PREP ----
# * DAILY SUBSCRIBERS INCREASES
cci_index_tbl %>%
  glimpse()


# * Daily Log Returns ----
time_param <- "weekly"
log_returns_tbl <- cci_index_tbl %>%
  tq_transmute(
    select = close
    , periodReturn
    , period = time_param
    , type = "log"
    , col_rename = "value"
  ) %>%
  purrr::set_names("date_col","value")

# * Summary diagnostics ----
log_returns_tbl %>%
  tk_summary_diagnostics(.date_var = date_col)

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




# * Train/Test ----

splits <- log_returns_tbl %>%
  time_series_split(
    date_var = date_col
    , assess = "12 weeks"
    , cumulative = TRUE
  )

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(
    .date_var = date_col
    , .value  = value
    , .title  = paste0(
      "CCI30 ", stringr::str_to_title(time_param), " Log Returns"
    )
  )

# Features ----------------------------------------------------------------

recipe_base <- recipe(value ~ ., data = training(splits)) %>%
  step_timeseries_signature(date_col)

recipe_final <- recipe_base %>%
  step_rm(
    contains("iso")
    , contains("second")
    , contains("minute")
    , contains("hour")
    , contains("am.pm")
    , contains("xts")
  ) %>%
  step_normalize(contains("index.num"), date_col_year) %>%
  step_dummy(contains("lbl"), one_hot = TRUE) %>%
  step_fourier(date_col, period = 365/12, K = 2) %>%
  step_holiday_signature(date_col) %>%
  step_YeoJohnson(value)

# MODELING ----
# * AUTO ARIMA ----
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(value ~ date_col, data = training(splits))

# * Boosted Auto ARIMA ------------------------------------------------------

model_spec_arima_boosted <- arima_boost(
  min_n = 2
  , learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost")

wflw_fit_arima_boosted <- workflow() %>%
  add_recipe(recipe = recipe_final) %>%
  add_model(model_spec_arima_boosted) %>%
  fit(training(splits))


# * ETS ---------------------------------------------------------------------

model_spec_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") 

wflw_fit_ets <- workflow() %>%
  add_recipe(recipe = recipe_final) %>%
  add_model(model_spec_ets) %>%
  fit(training(splits))

# * Prophet -----------------------------------------------------------------

model_spec_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet")

wflw_fit_prophet <- workflow() %>%
  add_recipe(recipe = recipe_final) %>%
  add_model(model_spec_prophet) %>%
  fit(training(splits))

model_spec_prophet_boost <- prophet_boost(learn_rate = 0.1) %>% 
  set_engine("prophet_xgboost") 

wflw_fit_prophet_boost <- workflow() %>%
  add_recipe(recipe = recipe_final) %>%
  add_model(model_spec_prophet_boost) %>%
  fit(training(splits))

# * TSLM --------------------------------------------------------------------

model_spec_lm <- linear_reg() %>%
  set_engine("lm")

wflw_fit_lm <- workflow() %>%
  add_recipe(recipe = recipe_final) %>%
  add_model(model_spec_lm) %>%
  fit(training(splits))


# * TBATS ------------------------------------------------------------

model_fit_tbats <- seasonal_reg() %>%
  set_engine("tbats")%>%
  fit(value ~ date_col, data = training(splits))

# * MARS --------------------------------------------------------------------

model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth")

wflw_fit_mars <- workflow() %>%
  add_recipe(recipe = recipe_final) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))

# * NNETAR ------------------------------------------------------------------

model_fit_nnetar <- nnetar_reg() %>%
  set_engine("nnetar") %>%
  fit(value ~ date_col, data = training(splits))

# Model Table -------------------------------------------------------------

models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  wflw_fit_arima_boosted,
  wflw_fit_ets,
  wflw_fit_prophet,
  wflw_fit_prophet_boost,
  wflw_fit_lm, 
  model_fit_tbats,
  wflw_fit_mars,
  model_fit_nnetar
)

# * Model Ensemble Table ----------------------------------------------------
resample_tscv <- training(splits) %>%
  time_series_cv(
    date_var      = date_col
    , assess      = "30 days"
    , initial     = "120 days"
    , skip        = "30 days"
    , slice_limit = 1
  )

submodel_predictions <- models_tbl %>%
  modeltime_fit_resamples(
    resamples = resample_tscv
    , control = control_resamples(verbose = TRUE)
  )

ensemble_fit <- submodel_predictions %>%
  ensemble_model_spec(
    model_spec = linear_reg(
      penalty  = tune()
      , mixture = tune()
    ) %>%
      set_engine("glmnet")
    , kfold    = 5
    , grid     = 6
    , control  = control_grid(verbose = TRUE)
  )

ensemble_mean_fit <- models_tbl %>%
  ensemble_average(type = "mean")

# Model Table -------------------------------------------------------------

models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  wflw_fit_arima_boosted,
  wflw_fit_ets,
  wflw_fit_prophet,
  wflw_fit_prophet_boost,
  wflw_fit_lm, 
  model_fit_tbats,
  wflw_fit_mars,
  model_fit_nnetar,
  #ensemble_fit,
  ensemble_mean_fit
)

models_tbl

# Calibrate Model Testing -------------------------------------------------

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))
calibration_tbl

# Testing Accuracy --------------------------------------------------------

calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = log_returns_tbl
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25,
    .interactive = TRUE,
    .conf_interval_show = FALSE
  )

calibration_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse) %>%
  table_modeltime_accuracy(resizable = TRUE, bordered = TRUE)

# Refit to all Data -------------------------------------------------------

refit_tbl <- calibration_tbl %>%
  #modeltime_refit(data = log_returns_tbl)
  modeltime_refit(
    data        = log_returns_tbl
    , resamples = resample_tscv
    #, control   = control_resamples(verbose = TRUE)
  )

top_two_models <- refit_tbl %>% 
  modeltime_accuracy() %>% 
  arrange(mae) %>% 
  head(2)

refit_tbl %>%
  filter(.model_id %in% top_two_models$.model_id) %>%
  modeltime_forecast(h = "12 weeks", actual_data = log_returns_tbl) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25
    , .interactive = FALSE
    , .conf_interval_show = FALSE
    , .title = "12 Week CCI30 Log Return Forecast"
  )
