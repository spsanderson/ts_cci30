# Lib Load ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  "prophet",
  "tidyverse",
  "Matrix",
  "lubridate",
  "RcppRoll",
  "tidyquant",
  "tsibble",
  "patchwork",
  "RemixAutoML",
  "fitdistrplus"
)

# Data ----
url <- "https://cci30.com/ajax/getIndexHistory.php"
destfile <- "data/cci30_OHLCV.csv"
download.file(url, destfile = destfile)
df <- read.csv("data/cci30_OHLCV.csv")
class(df)

# Adjust date
df$Date <- ymd(df$Date)

# Time Params ----
time_param <- "weekly"
m_freq <- case_when(
  time_param == "daily" ~ "day",
  time_param == "weekly" ~ "week",
  time_param == "monthly" ~ "month"
)
m_periods <- case_when(
  time_param == "daily" ~ 30,
  time_param == "weekly" ~ 52,
  time_param == "monthly" ~ 12
)

# Filter Data ----
df <- df %>%
  mutate(
    time_start = case_when(
      m_freq == "month" ~ EOMONTH(Date),
      m_freq == "week" ~ floor_date(Date, unit = m_freq),
      TRUE ~ Date
    )
  )

df.tibble <- as_tsibble(df, index = Date) %>%
  filter(Date <= max(time_start)) %>%
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

# Coerce df to tibble ----
df.tibble <- as_tibble(df.tibble)

# Add varaibles ----
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

return_col <- df.ts[,ncol(df.ts)] %>% pull()

# Return Dist ----
descdist(data = return_col)

# Prohpet ----
# Get into prophet format ds/y
ds <- df.ts$Date
y <- return_col
df_prophet <- data.frame(ds, y)

# Model Params ----
m_mcmc_samples <- 1000
m_uncertainty_samples <- 500

# Models ----
m1 <- prophet(
  df_prophet, 
  growth = "linear", 
  yearly.seasonality = "auto",
  weekly.seasonality = "auto",
  daily.seasonality = "auto",
  interval.width = 0.95
  )

m2 <- prophet(
  df_prophet,
  growth = "linear",
  mcmc.samples = m_mcmc_samples,
  yearly.seasonality = "auto",
  weekly.seasonality = "auto",
  daily.seasonality = "auto",
  uncertainty.samples = m_uncertainty_samples,
  interval.width = 0.95
)

m3 <- prophet(
  df_prophet,
  growth = "linear",
  mcmc.samples = m_mcmc_samples * 5, # 5000 samples
  yearly.seasonality = "auto",
  weekly.seasonality = "auto",
  daily.seasonality = "auto",
  uncertainty.samples = m_uncertainty_samples,
  interval.width = 0.95
)

# Prediction Horizon ----
# sets the prediction frequency to m_freq and covers the next n periods defined by m_periods
future1 <- make_future_dataframe(m1, periods = m_periods, freq = m_freq) 
future2 <- make_future_dataframe(m2, periods = m_periods, freq = m_freq)
future3 <- make_future_dataframe(m2, periods = m_periods, freq = m_freq)
# makes sure we have enough predictions
range(future1$ds)

# Forecasts ----
# M1 ----
# make future predictions for a determined amount of weeks
forecast1 <- predict(m1, future1) # use future time frame to predict m
plot(m1, forecast1) + 
  # initial plot with change points identified
  add_changepoints_to_plot(m1) +
  theme_tq() +
  labs(
    y = str_c(str_to_title(time_param), "Log Returns", sep = " "),
    x = "",
    title = "Prophet Model M1 No MCMC Sampling",
    subtitle = "Red lines are Change Points"
  )
# note approximately 9 per year
tail(forecast1[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]) # for validation, delete later. Note vertical lines for inflection points
prophet_plot_components(m1, forecast1) # component analysis

m1_yhat <- forecast1 %>%
  as_tibble() %>%
  select(ds, yhat) %>%
  mutate(ds = as_date(ds))

m1_residuals <- df_prophet %>%
  as_tibble() %>%
  inner_join(m1_yhat, by = c("ds"="ds")) %>%
  mutate(.resid = yhat - y)

m1_resid_plt <- m1_residuals %>%
  ggplot(
    mapping = aes(
      x = ds,
      y = .resid
    )
  ) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  theme_tq()+
  labs(
    y = str_c(str_to_title(time_param), "Log Returns", sep = " "),
    x = "",
    title = "Prophet Model M1 No MCMC Sampling",
    subtitle = "Model Residuals"
  )

m1_resid_hist <- m1_residuals %>%
  ggplot(
    mapping = aes(
      x = .resid
      )
  ) +
  geom_histogram(bins = 30L, color = "black") +
  theme_tq() +
  labs(
    y = str_c(str_to_title(time_param), "Log Returns", sep = " "),
    x = "",
    title = "Prophet Model M1 No MCMC Sampling",
    subtitle = "Model Residuals"
  )

dyplot.prophet(m1, forecast1) # interactive plot

# M2 ----
# make future predictions for a determined amount of weeks
forecast2 <- predict(m2, future2) # use future time frame to predict m
plot(m2, forecast2) + 
  # initial plot with change points identified
  add_changepoints_to_plot(m2) +
  theme_tq() +
  labs(
    y = str_c(str_to_title(time_param), "Log Returns", sep = " "),
    x = "",
    title = "Prophet Model M2 MCMC Sampling",
    subtitle = str_c("Red lines are Change Points - MCMC Samples: ", m_mcmc_samples)
  )

# note approximately 9 per year
tail(forecast2[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]) # for validation, delete later. Note vertical lines for inflection points
prophet_plot_components(m2, forecast2) # component analysis

m2_yhat <- forecast2 %>%
  as_tibble() %>%
  select(ds, yhat) %>%
  mutate(ds = as_date(ds))

m2_residuals <- df_prophet %>%
  as_tibble() %>%
  inner_join(m2_yhat, by = c("ds"="ds")) %>%
  mutate(.resid = yhat - y)

m2_resid_plt <- m2_residuals %>%
  ggplot(
    mapping = aes(
      x = ds,
      y = .resid
    )
  ) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  theme_tq()+
  labs(
    y = str_c(str_to_title(time_param), "Log Returns", sep = " "),
    x = "",
    title = "Prophet Model M2 MCMC Sampling",
    subtitle = str_c("Model Residuals - MCMC Samples: ", m_mcmc_samples)
  )

m2_resid_hist <- m2_residuals %>%
  ggplot(
    mapping = aes(
      x = .resid
    )
  ) +
  geom_histogram(bins = 30L, color = "black") +
  theme_tq() +
  labs(
    y = str_c(str_to_title(time_param), "Log Returns", sep = " "),
    x = "",
    title = "Prophet Model M2 MCMC Sampling",
    subtitle = str_c("Model Residuals - MCMC Samples: ", m_mcmc_samples)
  )

dyplot.prophet(m2, forecast2) # interactive plot

# M3 ----
# make future predictions for a determined amount of weeks
forecast3 <- predict(m3, future3) # use future time frame to predict m
plot(m3, forecast3) + 
  # initial plot with change points identified
  add_changepoints_to_plot(m2) +
  theme_tq() +
  labs(
    y = str_c(str_to_title(time_param), "Log Returns", sep = " "),
    x = "",
    title = "Prophet Model M2 MCMC Sampling",
    subtitle = str_c("Red lines are Change Points - MCMC Samples: ", m_mcmc_samples)
  )

# note approximately 9 per year
tail(forecast3[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]) # for validation, delete later. Note vertical lines for inflection points
prophet_plot_components(m3, forecast3) # component analysis

m3_yhat <- forecast3 %>%
  as_tibble() %>%
  select(ds, yhat) %>%
  mutate(ds = as_date(ds))

m3_residuals <- df_prophet %>%
  as_tibble() %>%
  inner_join(m3_yhat, by = c("ds"="ds")) %>%
  mutate(.resid = yhat - y)

m3_resid_plt <- m3_residuals %>%
  ggplot(
    mapping = aes(
      x = ds,
      y = .resid
    )
  ) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  theme_tq()+
  labs(
    y = str_c(str_to_title(time_param), "Log Returns", sep = " "),
    x = "",
    title = "Prophet Model M3 MCMC Sampling",
    subtitle = str_c("Model Residuals - MCMC Samples: ", m_mcmc_samples * 5)
  )

m3_resid_hist <- m2_residuals %>%
  ggplot(
    mapping = aes(
      x = .resid
    )
  ) +
  geom_histogram(bins = 30L, color = "black") +
  theme_tq() +
  labs(
    y = str_c(str_to_title(time_param), "Log Returns", sep = " "),
    x = "",
    title = "Prophet Model M2 MCMC Sampling",
    subtitle = str_c("Model Residuals - MCMC Samples: ", m_mcmc_samples * 2)
  )

dyplot.prophet(m2, forecast2) # interactive plot

# Yhat ----
df_y_yhat <- m1_residuals %>%
  inner_join(m2_residuals, by = c("ds" = "ds")) %>%
  inner_join(m3_residuals, by = c("ds" = "ds")) %>%
  as_tibble() %>%
  set_names(
    "ds","y_m1","yhat_m1","resid_m1",
    "y_m2","yhat_m2","resid_m2",
    "y_m3","yhat_m3","resid_m3"
  ) %>%
  select(-y_m2, -y_m3)

# Prophet CV ----
# now we measure forecast error with cross-validation
# take 53 weeks, predict 26 iteratively for the entire data set to see how accurate we are (will cycle through multiple times)
df.cv <- cross_validation(m2, initial = 53, horizon = 26, units = "weeks") # this will do 6 iterations of 4 markov chains
df.p <- performance_metrics(df.cv) # check performance
head(df.p)
tail(df.p)
plot_cross_validation_metric(df.cv, metric = "rmse")
plot_cross_validation_metric(df.cv, metric = "mape")
dyplot.prophet(m2, df.cv)

# AutoTS() ----
ats_ob <- AutoTS(
  data = df_prophet
  , TargetName = "y"
  , DateName = "ds"
  , FCPeriods = ifelse(time_param == "weekly", 52, 12)
  , TimeUnit = ifelse(time_param == "weekly","week","month")
  , HoldOutPeriods = round(nrow(df_prophet) * 0.7, 0)
)
print(ats_ob)

ats_fitted <- ats_ob[["TimeSeriesModel"]][["residuals"]] %>% 
  enframe() %>% 
  set_names("name","ats_yhat") %>%
  select(-name) %>%
  as_tibble() %>%
  mutate(ats_yhat = as.numeric(ats_yhat))

df_y_yhat <- df_y_yhat %>% 
  bind_cols(ats_fitted)

ats_residuals <- df_prophet %>%
  bind_cols(ats_fitted) %>%
  mutate(ats_resid = ats_yhat - y)

ats_resid_plt <- ats_residuals %>%
  ggplot(
    mapping = aes(
      x = ds,
      y = ats_resid
    )
  ) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  theme_tq() +
  labs(
    y = str_c(str_to_title(time_param), "Log Returns", sep = " "),
    x = "",
    title = "RemixAutoML AutoTS()",
    subtitle = "Model Residuals"
  )

ats_resid_hist <- ats_residuals %>%
  ggplot(
    mapping = aes(
      x = ats_resid
    )
  ) +
  geom_histogram(bins = 30L, color = "black") +
  theme_tq() +
  labs(
    y = str_c(str_to_title(time_param), "Log Returns", sep = " "),
    x = "",
    title = "RemixAutoML AutoTS()",
    subtitle = "Model Residuals"
  )

# Viz ----
density_plt <- df_y_yhat %>%
  ggplot(
    mapping = aes(
      x = y_m1
    )
  ) +
  # Actual
  geom_density(color = "black", size = 1) +
  # M1
  geom_density(
    data = df_y_yhat,
    mapping = aes(
      x = yhat_m1
    ),
    color = "red",
    size = 1
  ) +
  # M2
  geom_density(
    data = df_y_yhat,
    mapping = aes(
      x = yhat_m2
    ),
    color = "green",
    size = 1
  ) +
  # M3
  geom_density(
    data = df_y_yhat,
    mapping = aes(
      x = yhat_m3
    ),
    color = "orange",
    linetype = "dashed",
    size = 1
  ) +
  # AutoTS
  geom_density(
    data = df_y_yhat,
    mapping = aes(
      x = ats_yhat
    ),
    color = "purple",
    size = 1
  ) +
  theme_tq() +
  labs(
    title = str_c("Density of ", time_param," Log returns"),
    subtitle = str_glue("Black line actual returns.
                        Red line fbProphet Model 1 estimates.
                        Green line fbProphet Model 2 estimates.
                        Orange Dashed line fbProphet Model 3 estimates.
                        Purple line AutoTS estimates."),
    x = "",
    caption = str_c("Returns from ", min.date, " through ", max.date)
  )

density_plt
m1_resid_plt / m2_resid_plt / m3_resid_plt / ats_resid_plt
m1_resid_hist / m2_resid_hist / m3_resid_hist / ats_resid_hist
