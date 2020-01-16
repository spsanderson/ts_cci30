# Load Libs ####
# Time Series analysis of CCI30 crypto index
install.load::install_load(
  "tidyquant"
  ,"timetk"
  , "tibbletime"
  , "tsibble"
  , "sweep"
  , "anomalize"
  , "caret"
  , "forecast"
  , "funModeling"
  , "xts"
  , "fpp"
  , "lubridate"
  , "tidyverse"
  , "urca"
  , "prophet"
  , "fable"
  , "feasts"
  , "RemixAutoML"
)

# Daily OHLCV ####
url <- "https://cci30.com/ajax/getIndexHistory.php"
destfile <- "cci30_OHLCV.csv"
download.file(url, destfile = destfile)
df <- read.csv("cci30_OHLCV.csv")
class(df)

# Get month end of file - last day of previous month
# Format Date ####
df$Date <- lubridate::ymd(df$Date)
df <- df %>%
 mutate(month_start = floor_date(Date, unit = "month") - period(1, units = "day"))

df.tibble <- as_tsibble(df, index = Date) %>%
  filter(Date <= max(month_start)) %>%
  select(Date, Open, High, Low, Close, Volume)

class(df.tibble)
head(df.tibble, 1)
tail(df.tibble, 1)

min.date  <- min(df.tibble$Date)
min.year  <- year(min.date)
min.month <- month(min.date)
max.date  <- max(df.tibble$Date)
max.year  <- year(max.date)
max.month <- month(max.date)

# Coerce df to tibble ####
df.tibble <- as_tibble(df.tibble)
featurePlot(
  x = df.tibble[,c("Open","High","Low","Volume")]
  , y = df.tibble$Close
  , plot = "pairs"
  , auto.key = list(columns = 4)
  , na.action(na.omit)
)

# Add varaibles ####
df.tibble <- df.tibble %>% 
  tq_mutate(
    select = Close
    , mutate_fun = dailyReturn
    , col_rename = "Daily_Return"
  ) %>%
  tq_mutate(
    select = Close
    , mutate_fun = periodReturn
    , period = "daily"
    , type = "log"
    , col_rename = "Daily_Log_Return"
  )

head(df.tibble, 5)
profiling_num(df.tibble$Daily_Log_Return)

# Time Parameter ----
time_param <- "weekly"

# Make a log returns of close object
df.ts <- df.tibble %>%
  tq_transmute(
    select = Close
    , periodReturn
    , period = time_param
    , type = "log"
    , col_rename = str_c(str_to_title(time_param),"Log_Returns", sep = "_")
  )
head(df.ts, 5)

# Get some Params ####
# get max and min discharges
max.log.return <- max(df.ts[,ncol(df.ts)])
min.log.return <- min(df.ts[,ncol(df.ts)])
start.date <- min(df.ts$Date)
end.date   <- max(df.ts$Date)
return_col <- df.ts[,ncol(df.ts)] %>% pull()

training.region <- round(nrow(df.ts) * 0.7, 0)
test.region     <- nrow(df.ts) - training.region

plot.ts(return_col)

# Plot intial Data ----
df.ts %>%
  ggplot(
    aes(
      x = Date
      , y = return_col
    )
  ) +
  geom_point(
    alpha = 0.5
    , color = palette_light()[[1]]
  ) +
  geom_line(
    alpha = 0.5
  ) +
  geom_smooth(
    se = F
    , method = 'loess'
    , color = 'red'
    , span = case_when(
      time_param == "monthly" ~ 1/4
      , time_param == "weekly" ~ 1/12
    )  #1/12
  ) +
  labs(
    title = str_c(str_to_title(time_param), " Returns: Log Scale")
    , subtitle = "Source: https://cci30.com/ajax/getIndexHistory.php"
    , caption = paste0(
      "Based on daily closing prices from: "
      , start.date
      , " through "
      , end.date
    )
    , y = ""
    , x = ""
  ) +
  theme_tq()

ggAcf(return_col) + 
  theme_tq() + 
  labs(title = str_c(
    "ACF of"
    , str_to_title(time_param)
    , "Log Returns"
    , sep = " "
    )
  )

ggPacf(return_col) +
  theme_tq() +
  labs(
    title = str_c(
      "PACF of"
      , str_to_title(time_param)
      , "Log Returns"
      , sep = " "
    )
  )

ggtsdisplay(return_col)
gghistogram(return_col)
ggseasonplot(
  tk_ts(
    return_col
    , frequency = case_when(
      time_param == "monthly" ~ 12
      , time_param == "weekly" ~ 52
    )
    , start = c(min.year, min.month)
    , end = c(max.year, max.month)
    )
  ) +
  labs(
    title = str_c(
      "Seasonal plot:"
      , str_to_title(time_param)
      , "Log Returns"
      , sep = " "
    )
  ) +
  theme_tq() +
  theme(
    axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
  ) 

# Anomalize df.ts ----
value_column <- ncol(df.ts)
df.ts %>%
  tibbletime::as_tbl_time(index = Date) %>%
  time_decompose(value_column, method = "twitter") %>%
  anomalize(remainder, method = "gesd") %>%
  time_recompose() %>%
  plot_anomalies(
    ncol = 3
    , alpha_dots = 0.25
  ) +
  xlab("") +
  ylab(str_c(str_to_title(time_param), "Log Returns", sep = " ")) +
  labs(
    title = str_c(
      "Anomaly Detection for CCI30"
      , str_to_title(time_param)
      , "Log Returns"
      , sep = " "
    )
    , subtitle = "Twitter and GESD Methods"
  )

df.ts %>%
  tibbletime::as_tbl_time(index = Date) %>%
  time_decompose(value_column, method = "twitter") %>%
  anomalize(remainder, method = "gesd") %>%
  plot_anomaly_decomposition() +
  xlab("") +
  ylab(str_c(str_to_title(time_param), "Log Returns", sep = " ")) +
  labs(
    title = str_c(
      "Anomaly Detection for CCI30"
      , str_to_title(time_param)
      , "Log Returns"
      , sep = " "
    )
    , subtitle = "Twitter and GESD Methods"
  )

# Make tsibble ---
# Add observed cleaned to df.ts in both a tibble and tsibble
df_tbl <- df.ts
class(df_tbl)

df_tbl <- df_tbl %>%
  tibbletime::as_tbl_time(index = Date) %>%
  time_decompose(value_column, method = "twitter") %>%
  anomalize(remainder, method = "gesd") %>%
  clean_anomalies() %>%
  time_recompose() %>%
  select(Date, observed, anomaly, observed_cleaned)

# Plot observed vs observed_cleaned
df_tbl %>%
  ggplot(
    mapping = aes(
      x = Date
      , y = observed
    )
  ) +
  geom_point() +
  geom_line() +
  geom_point(
    mapping = aes(
      y = observed_cleaned
    )
    , color = "red"
  ) +
  geom_line(
    mapping = aes(
      y = observed_cleaned
    )
    , color = "red"
  ) +
  theme_tq() +
  labs(
    title = "Observed Log Returns vs. Observed Cleaned"
    , subtitle = "Anomalies cleaned with anomalize::clean_anomalies()"
    , y = str_c(str_to_title(time_param), "Log Return", sep = " ")
    , x = ""
  )

# Convert to tsibble ----
ifelse(
  time_param == "weekly"
  , df_tsbl <- df_tbl %>%
    mutate(date_column = yearweek(Date)) %>%
    select(date_column, observed, observed_cleaned) %>%
    as_tsibble(index = date_column) 
  , df_tsbl <- df_tbl %>%
    mutate(date_column = yearmonth(Date)) %>%
    select(date_column, observed, observed_cleaned) %>%
    as_tsibble(index = date_column)
)

df_tsbl %>% 
  STL(observed_cleaned ~ season(window = Inf)) %>% 
  autoplot() +
  theme_tq()

df_tsbl %>%
  STL(observed ~ season(window = Inf)) %>%
  autoplot() +
  theme_tq()

df_tsbl %>%
  gg_tsdisplay(observed)

df_tsbl %>%
  gg_tsdisplay(observed_cleaned)

# Modeling ----
models_observed <- df_tsbl %>%
  model(
    ets = ETS(observed)
    , arima = ARIMA(observed)
    , nnetar = NNETAR(observed, n_nodes = 10)
  )

models_observed_cleaned <- df_tsbl %>%
  model(
    ets = ETS(observed_cleaned)
    , arima = ARIMA(observed_cleaned)
    , nnetar = NNETAR(observed_cleaned, n_nodes = 10)
  )

models_ob_acc <- accuracy(models_observed) %>%
  arrange(MAE) %>%
  mutate(model = .model %>% as_factor()) %>%
  mutate(model_numeric = .model %>% as_factor() %>% as.numeric())

models_obc_acc <- accuracy(models_observed_cleaned) %>%
  arrange(MAE) %>%
  mutate(model = .model %>% as_factor()) %>%
  mutate(model_numeric = .model %>% as_factor() %>% as.numeric())

models_ob_tidy <- augment(models_observed) %>%
  mutate(key = "actual") %>%
  mutate(data_type = "observed") %>%
  set_names(
    "Model"
    , "Date_Column"
    , "Value"
    , "Fitted"
    , "Residuals"
    , "key"
    , "data_type"
  ) %>%
  as_tibble()

models_obc_tidy <- augment(models_observed_cleaned) %>%
  mutate(key = "actual") %>%
  mutate(data_type = "observed_cleaned") %>%
  set_names(
    "Model"
    , "Date_Column"
    , "Value"
    , "Fitted"
    , "Residuals"
    , "key"
    , "data_type"
  ) %>%
  as_tibble()

model_desc_ob <- models_observed %>%
  as_tibble() %>%
  gather() %>%
  mutate(model_desc = print(value)) %>%
  select(key, model_desc) %>%
  set_names("model", "model_desc")

model_desc_obc <- models_observed_cleaned %>%
  as_tibble() %>%
  gather() %>%
  mutate(model_desc = print(value)) %>%
  select(key, model_desc) %>%
  set_names("model", "model_desc")

model_ob_desc <- model_desc_ob$model_desc
model_obc_desc <- model_desc_obc$model_desc

# Plot Residuals ----
a <- models_ob_tidy %>%
  inner_join(models_ob_acc, by = c("Model" = ".model")) %>%
  inner_join(model_desc_ob, by = c("Model" = "model")) %>%
  select(Model, Date_Column, model, model_desc, Residuals, key) %>%
  mutate(data_type = "observed")
b <- models_obc_tidy %>%
  inner_join(models_obc_acc, by = c("Model" = ".model")) %>%
  inner_join(model_desc_obc, by = c("Model" = "model")) %>%
  select(Model, Date_Column, model, model_desc, Residuals, key) %>%
  mutate(data_type = "observed_cleaned")
c <- union_all(a, b)

c %>%
  ggplot(
    mapping = aes(
      x = Date_Column
      , y = Residuals
      , group = data_type
      , color = data_type
    )
  ) + 
  geom_hline(yintercept = 0) +
  geom_line() +
  facet_wrap(~ model, ncol = 1, scales = "free_y") +
  theme_tq() + 
  labs(
    x = ""
    , title = "Model Residuals of Observed vs. Observed Cleaned by Model"
    , subtitle = "Anomalies cleaned with anomalize::clean_anomalies()"
    , color = ""
  )

c %>%
  ggplot(
    mapping = aes(
      x = Date_Column
      , y = Residuals
      , group = model
      , color = model
    )
  ) +
  geom_hline(yintercept = 0) +
  geom_line() +
  facet_wrap(~ data_type, ncol = 1, scales = "free_y") +
  theme_tq() + 
  labs(
    x = ""
    , title = "Model Residuals by Observed vs. Observed Cleaned"
    , subtitle = "Anomalies cleaned with anomalize::clean_anomalies()"
    , color = "Model:"
  )

c %>%
  ggplot(
    mapping = aes(
      x = Residuals
      , group = data_type
      , color = data_type
      , fill = data_type
    )
  ) +
  geom_density(alpha = 0.314) +
  facet_wrap(~ model, ncol = 1, scales = "free_y") +
  theme_tq() + 
  labs(
    x = ""
    , title = "Model Residuals by Observed vs. Observed Cleaned"
    , subtitle = "Anomalies cleaned with anomalize::clean_anomalies()"
    , fill = ""
    , color = ""
  )

c %>%
  ggplot(
    mapping = aes(
      x = Residuals
      , group = model
      , color = model
      , fill = model
    )
  ) +
  geom_density(alpha = 0.314) +
  facet_wrap(~ data_type, ncol = 1, scales = "free_y") +
  theme_tq() + 
  labs(
    x = ""
    , title = "Model Residuals by Observed vs. Observed Cleaned"
    , subtitle = "Anomalies cleaned with anomalize::clean_anomalies()"
    , fill = ""
    , color = ""
  )

# Forecast ----
models_ob_fcast <- models_observed %>%
  forecast(h = ifelse(time_param == "weekly", 52, 12)) %>%
  as_tibble() %>%
  select(.model, date_column, observed) %>%
  mutate(key = "forecast") %>%
  mutate(data_type = "observed") %>%
  set_names("Model","Date_Column", "Value", "key", "data_type")

models_obc_fcast <- models_observed_cleaned %>%
  forecast(h = ifelse(time_param == "weekly", 52, 12)) %>%
  as_tibble() %>%
  select(.model, date_column, observed_cleaned) %>%
  mutate(key = "forecast") %>%
  mutate(data_type = "observed_cleaned") %>%
  set_names("Model","Date_Column", "Value", "key", "data_type")

models_fcast <- union_all(models_ob_fcast, models_obc_fcast)

winning_ob_model_lbl <- models_ob_acc %>%
  filter(model_numeric == 1) %>%
  left_join(model_desc_ob, by = c(".model" = "model")) %>%
  select(model_desc)

winning_obc_model_lbl <- models_obc_acc %>%
  filter(model_numeric == 1) %>%
  left_join(model_desc_obc, by = c(".model" = "model")) %>%
  select(model_desc)

tidy_model_tbl <- union_all(models_ob_tidy, models_obc_tidy)

tidy_model_tbl %>%
  ggplot(
    mapping = aes(
      x = Date_Column
      , y = Value
      , color = Model
    )
  ) +
  geom_point(
    alpha = 0.5
    , color = palette_light()[[1]]
  ) +
  geom_line(
    alpha = 0.5
    , size = 1
  ) +
  facet_wrap(~ data_type, scales = "free_y", ncol = 1) +
  geom_point(
    data = models_fcast
    , mapping = aes(
      x = Date_Column
      , y = Value
      , group = Model
      , color = Model
    )
  ) +
  geom_line(
    data = models_fcast
    , mapping = aes(
      x = Date_Column
      , y = Value
      , group = Model
      , color = Model
    )
  ) +
  theme_tq() +
  scale_color_tq() +
  labs(
    title = "CCI30 Forecast of Observed vs Cleaned Observations"
    , subtitle = "Observed Cleaned data obtained with anomalize::clean_anomalies()"
    , caption = str_c("Time Parameter is set to:", str_to_title(time_param), sep = " ")
    , x = ""
    , y = str_c(str_to_title(time_param), "Log Returns", sep = " ")
    , color = "Models:"
  )

# AutoTS() ----
AutoTS(
  data = df_tbl
  , TargetName = "observed"
  , DateName = "Date"
  , FCPeriods = ifelse(time_param == "weekly", 52, 12)
  , TimeUnit = ifelse(time_param == "weekly","week","month")
  , HoldOutPeriods = round(nrow(df.ts.tbl) * 0.7, 0)
)

AutoTS(
  data = df_tbl
  , TargetName = "observed_cleaned"
  , DateName = "Date"
  , FCPeriods = ifelse(time_param == "weekly", 52, 12)
  , TimeUnit = ifelse(time_param == "weekly","week","month")
  , HoldOutPeriods = round(nrow(df.ts.tbl) * 0.7, 0)
)

AutoXGBoostCARMA(
  data = tidy_model_tbl
  , TargetColumnName = "Value"
  , DateColumnName = "Date_Column"
  , GroupVariables = "data_type"
  , FC_Periods = ifelse(time_param == "weekly", 52, 12)
  , TimeUnit = ifelse(time_param == "weekly", "week", "month")
)
