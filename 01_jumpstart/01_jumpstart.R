# GOAL: Forecast Daily Log Returns - Next 12-WEEKS

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
    , mutate_fun = periodReturn
    , period = time_param
    , type = "log"
    , col_rename = "value"
  ) %>%
  set_names("date_col", "value")

write_rds(
  x    = log_returns_tbl,
  file = "00_data/cci30.rds"
)


# * Summary diagnostics ----
log_returns_tbl %>%
  tk_summary_diagnostics(.date_var = date_col)


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

n_cores <- parallel::detectCores() - 1

# Features ----------------------------------------------------------------

recipe_base <- recipe(value ~ ., data = training(splits)) %>%
  step_mutate(wk = lubridate::week(date_col)) %>%
  step_harmonic(wk, frequency = 365/52, cycle_size = 1) %>%
  step_rm(wk) %>%
  step_hai_fourier(value, scale_type = "sincos", period = 365/52, order = 1) %>%
  step_lag(value, lag = 1) %>%
  step_impute_knn(contains("lag_"))

recipe_date <- recipe_base %>%
  step_timeseries_signature(date_col) %>%
  step_rm(matches("(iso$)|(xts$)|(hour)|(min)|(sec)|(am.pm)")) %>%
  step_normalize(contains("index.num"), contains("date_col_year"))

recipe_fourier <- recipe_date %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_fourier(date_col, period = 365/52, K = 1) %>%
  step_YeoJohnson(value, limits = c(0,1))

recipe_fourier_final <- recipe_fourier %>%
  step_nzv(all_predictors())

recipe_pca <- recipe_base %>%
  step_timeseries_signature(date_col) %>%
  step_rm(matches("(iso$)|(xts$)|(hour)|(min)|(sec)|(am.pm)")) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_normalize(value) %>%
  step_fourier(date_col, period = 365/52, K = 1) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_pca(all_numeric_predictors(), threshold = .95)

recipe_num_only <- recipe_pca %>%
  step_rm(-value, -all_numeric_predictors())

# Models ------------------------------------------------------------------

# Auto ARIMA --------------------------------------------------------------

model_spec_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima")

# Boosted Auto ARIMA ------------------------------------------------------

model_spec_arima_boosted <- arima_boost(
  min_n = 2
  , learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost")

# ETS ---------------------------------------------------------------------

model_spec_ets <- exp_smoothing(
  seasonal_period = "auto",
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto"
) %>%
  set_engine(engine = "ets") 

model_spec_croston <- exp_smoothing(
  seasonal_period = "auto",
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto"
) %>%
  set_engine(engine = "croston")

model_spec_theta <- exp_smoothing(
  seasonal_period = "auto",
  error = "auto",
  trend = "auto",
  season = "auto",
  damping = "auto"
) %>%
  set_engine(engine = "theta")


# STLM ETS ----------------------------------------------------------------

model_spec_stlm_ets <- seasonal_reg(
  seasonal_period_1 = "auto",
  seasonal_period_2 = "auto",
  seasonal_period_3 = "auto"
) %>%
  set_engine("stlm_ets")

model_spec_stlm_tbats <- seasonal_reg(
  seasonal_period_1 = "auto",
  seasonal_period_2 = "auto",
  seasonal_period_3 = "auto"
) %>%
  set_engine("tbats")

model_spec_stlm_arima <- seasonal_reg(
  seasonal_period_1 = "auto",
  seasonal_period_2 = "auto",
  seasonal_period_3 = "auto"
) %>%
  set_engine("stlm_arima")

# NNETAR ------------------------------------------------------------------

model_spec_nnetar <- nnetar_reg(
  mode              = "regression"
  , seasonal_period = "auto"
) %>%
  set_engine("nnetar")


# Prophet -----------------------------------------------------------------

model_spec_prophet <- prophet_reg(
  seasonality_yearly = "auto",
  seasonality_weekly = "auto",
  seasonality_daily = "auto"
) %>%
  set_engine(engine = "prophet")

model_spec_prophet_boost <- prophet_boost(
  learn_rate = 0.1
  , trees = 10
  , seasonality_yearly = FALSE
  , seasonality_weekly = FALSE
  , seasonality_daily  = FALSE
) %>% 
  set_engine("prophet_xgboost") 

# TSLM --------------------------------------------------------------------

model_spec_lm <- linear_reg() %>%
  set_engine("lm")

model_spec_glm <- linear_reg(
  penalty = 1,
  mixture = 0.5
) %>%
  set_engine("glmnet")

model_spec_stan <- linear_reg() %>%
  set_engine("stan")

model_spec_spark <- linear_reg(
  penalty = 1,
  mixture = 0.5
) %>% 
  set_engine("spark")

model_spec_keras <- linear_reg(
  penalty = 1,
  mixture = 0.5
) %>%
  set_engine("keras")

# MARS --------------------------------------------------------------------

model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth")

# XGBoost -----------------------------------------------------------------

model_spec_xgboost <- boost_tree(
  mode  = "regression",
  mtry  = 10,
  trees = 100,
  min_n = 5,
  tree_depth = 3,
  learn_rate = 0.3,
  loss_reduction = 0.01
) %>%
  set_engine("xgboost")

# Workflowsets ------------------------------------------------------------

wfsets <- workflow_set(
  preproc = list(
    base          = recipe_base,
    date          = recipe_date,
    fourier       = recipe_fourier,
    fourier_final = recipe_fourier_final,
    pca           = recipe_pca,
    num_only_pca  = recipe_num_only
  ),
  models = list(
    model_spec_arima_no_boost,
    model_spec_arima_boosted,
    model_spec_ets,
    model_spec_lm,
    model_spec_glm,
    # model_spec_stan,
    # model_spec_spark,
    # model_spec_keras,
    model_spec_mars,
    model_spec_nnetar,
    model_spec_prophet,
    model_spec_prophet_boost,
    model_spec_stlm_arima,
    model_spec_stlm_ets,
    model_spec_stlm_tbats,
    model_spec_xgboost
  ),
  cross = TRUE
)

parallel_start(n_cores)
wf_fits <- wfsets %>% 
  modeltime_fit_workflowset(
    data = training(splits)
    , control = control_fit_workflowset(
      allow_par = TRUE
      , verbose = TRUE
    )
  )
parallel_stop()

wf_fits <- wf_fits %>%
  filter(.model != "NULL")

# Model Table -------------------------------------------------------------

models_tbl <- wf_fits

# Model Ensemble Table ----------------------------------------------------

fit_mean_ensemble <- models_tbl %>%
  ensemble_average(type = "mean")

fit_median_ensemble <- models_tbl %>%
  ensemble_average(type = "median")

# Model Table -------------------------------------------------------------

models_tbl <- models_tbl %>%
  add_modeltime_model(fit_mean_ensemble) %>%
  add_modeltime_model(fit_median_ensemble)

models_tbl

# Calibrate Model Testing -------------------------------------------------

parallel_start(n_cores)

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits), quiet = FALSE)

parallel_stop()

calibration_tbl

# Testing Accuracy --------------------------------------------------------

parallel_start(n_cores)

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = log_returns_tbl
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width   = 25,
    .interactive        = TRUE,
    .conf_interval_show = FALSE
  )

parallel_stop()

calibration_tbl %>%
  modeltime_accuracy() %>%
  drop_na() %>%
  arrange(desc(rsq)) %>%
  table_modeltime_accuracy(.interactive = FALSE)

output <- healthyR.ts::ts_model_auto_tune(
  .modeltime_model_id = 2,
  .calibration_tbl    = calibration_tbl,
  .splits_obj         = splits,
  .drop_training_na   = TRUE,
  .date_col           = date_col,
  .value_col          = value,
  .tscv_assess        = "26 weeks",
  .tscv_skip          = "4 weeks",
  .num_cores          = n_cores
)

new_model <- output$model_info$tuned_tscv_wflw_spec
ori_model <- output$model_info$plucked_model

calibrate_and_plot(
  new_model,
  ori_model,
  .splits_obj = splits,
  .data = log_returns_tbl,
  .type = "testing",
  .interactive = TRUE
)$plot 

calibration_tuned_tbl <- modeltime_table(
  new_model,
  ori_model
) %>%
  update_model_description(1, "TUNED - ARIMA(2,0,0) W Non-Zero Mean W/Drift") %>%
  update_model_description(2, "ORIGINAL - ARIMA(2,0,0) W Non-Zero Mean W/Drift") %>%
  modeltime_refit(data = training(splits)) %>%
  modeltime_calibrate(new_data = testing(splits))

parallel_start(n_cores)
refit_ensemble_models <- calibration_tbl %>%
  filter(
    .model_desc %>%
      str_to_lower() %>%
      str_detect("ensemble")
  ) %>%
  modeltime_refit(
    data = log_returns_tbl
    , control = control_refit(
      verbose   = TRUE
      , allow_par = TRUE
    )
  ) %>%
  modeltime_accuracy()
parellel_stop()

# Refit to all Data -------------------------------------------------------

parallel_start(n_cores)
refit_tbl <- calibration_tuned_tbl %>%
  modeltime_refit(
    data = log_returns_tbl
    , control = control_refit(
      verbose   = TRUE
      , allow_par = TRUE
    )
  )
parallel_stop()

model_choices <- rbind(refit_tbl, refit_ensemble_models)

 model_choices %>%
  modeltime_forecast(h = "12 weeks", actual_data = log_returns_tbl) %>%
  plot_modeltime_forecast(
    .legend_max_width     = 25
    , .interactive        = TRUE
    , .conf_interval_show = FALSE
    , .title = "CCI30 Log Returns Forecast 12 Weeks Out"
  )
