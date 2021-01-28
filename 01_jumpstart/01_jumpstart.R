# GOAL: Forecast Daily Log Returns - Next 12-WEEKS

# OBJECTIVES ----
# - Dive into a time-series analysis project
# - Experience Frameworks: modeltime
# - Experience 2 Algorithms:
#   1. Prophet
#   2. LM w/ Engineered Features

# LIBRARIES ----

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  
  # TS ML
  "tidymodels",
  "modeltime",
  "tidyquant",
  
  # EDA
  "DataExplorer",
  
  # Core TS
  "tidyverse",
  "timetk",
  "lubridate",
  
  # Clean Up
  "janitor"
)

# DATA -----
cci_index_tbl <- read_csv("00_data/cci30_OHLCV.csv") %>%
  clean_names()


# 1.0 EDA & DATA PREP ----
# * DAILY SUBSCRIBERS INCREASES
cci_index_tbl %>%
  glimpse()


# * Daily Log Returns ----
time_param <- "daily"
log_rets_daily_tbl <- cci_index_tbl %>%
  tq_transmute(
    select = close
    , periodReturn
    , period = time_param
    , type = "log"
    , col_rename = "value"
  ) %>%
  purrr::set_names("date_col","value")

# * Summary diagnostics ----
log_rets_daily_tbl %>%
  tk_summary_diagnostics(.date_var = date_col)

# * Visualize ----
log_rets_daily_tbl %>%
  plot_time_series(
    .date_var = date_col
    , .value  = value
    , .smooth = FALSE
  )

# * Train/Test ----

splits <- log_rets_daily_tbl %>%
  time_series_split(
    .date_var = date_col
    , assess = "12 week"
    , cumulative = TRUE
  )

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(
    .date_var = date_col
    , .value  = value
  )

# 3.0 PROPHET FORECASTING ----

# * Prophet Model using Modeltime/Parsnip ----
model_prophet_fit <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(value ~ date_col, data = training(splits))

# * Modeltime Process ----
model_tbl <- modeltime_table(
  model_prophet_fit
)

# * Calibration ----
calibration_tbl <- model_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

# * Visualize Forecast on Test Data ----
calibration_tbl %>%
  modeltime_forecast(actual_data = log_rets_daily_tbl) %>%
  plot_modeltime_forecast()

# * Get Accuracy Metrics ----
calibration_tbl %>%
  modeltime_accuracy()

# 4.0 FORECASTING WITH FEATURE ENGINEERING ----

# * Identifying Possible Features ----

log_rets_daily_tbl %>%
  plot_seasonal_diagnostics(
    .date_var = date_col
    , .value  = value
  )

# * Recipes Spec ----
training(splits)

recipe_spec <- recipe(date_col ~ ., data = training(splits)) %>%
  
  # TS Signature
  step_timeseries_signature(date_col) %>%
  
  step_rm(ends_with(".iso")) %>%
  step_rm(ends_with("xts")) %>%
  step_rm(contains("hour"), contains("minute"), contains("second"), contains("am.pm")) %>%
  
  step_normalize(ends_with("index.num"), ends_with("_year")) %>%
  step_dummy(all_nominal())

recipe_spec %>%
  prep() %>%
  juice() %>%
  glimpse()


# * ML Specs ----

model_spec <- linear_reg() %>%
  set_engine("lm")

workflow_fit_lm <- workflow() %>%
  add_model(model_spec) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

# * Modeltime Process ----
calibration_tbl <- modeltime_table(
  model_prophet_fit
  , workflow_fit_lm
) %>%
  modeltime_calibrate(testing(splits))

calibration_tbl %>%
  modeltime_accuracy()

calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits)
    , actual_data = evaluation_tbl
  ) %>%
  plot_modeltime_forecast()