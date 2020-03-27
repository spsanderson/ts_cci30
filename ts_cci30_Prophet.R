# Lib Load ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  "prophet",
  "tidyverse",
  "Matrix",
  "lubridate",
  "RcppRoll",
  "tidyquant",
  "tsibble"
)

# Read in the data sets
df <- read.csv("cci30_OHLCV.csv")

# Adjust dtg
df$Date <- ymd(df$Date)
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

# Time Parameter ----
time_param <- "monthly"
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

# Prohpet ----
# Get into prophet format ds/y
ds <- df.ts$Date
y <- return_col
df_prophet <- data.frame(ds, y)

# Models ----
m1 <- prophet(
  df_prophet, 
  growth = "linear", 
  mcmc.samples = 50000, 
  yearly.seasonality = "auto",
  weekly.seasonality = "auto",
  daily.seasonality = "auto",
  interval.width = 0.95
  )

m2 <- prophet(
  df_prophet,
  growth = "linear",
  mcmc.samples = 1000,
  yearly.seasonality = "auto",
  weekly.seasonality = "auto",
  daily.seasonality = "auto",
  seasonality.mode = "additive",
  uncertainty.samples = 100,
  interval.width = 0.95
)

m3 <- prophet(
  df_prophet,
  growth = "linear",
  mcmc.samples = 2000,
  yearly.seasonality = "auto",
  weekly.seasonality = "auto",
  daily.seasonality = "auto",
  seasonality.mode = "additive",
  uncertainty.samples = 10,
  interval.width = 0.95
)

# Prediction Horizon ----
# sets the prediction frequency to hour and covers the next 7 days or 168 hours
future1 <- make_future_dataframe(m1, periods = m_periods, freq = m_freq) 
future2 <- make_future_dataframe(m2, periods = m_periods, freq = m_freq)
future3 <- make_future_dataframe(m3, periods = m_periods, freq = m_freq)
range(future1$ds)# makes sure we have enough predictions

# Forecasts ----
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

m1_residuals %>%
  ggplot(
    mapping = aes(
      x = ds,
      y = .resid
    )
  ) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  theme_tq()

dyplot.prophet(m1, forecast1) # interactive plot

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
    subtitle = "Red lines are Change Points"
  )
# note approximately 9 per year
tail(forecast2[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]) # for validation, delete later. Note vertical lines for inflection points
prophet_plot_components(m2, forecast2) # component analysis

dyplot.prophet(m2, forecast2) # interactive plot

# make future predictions for a determined amount of weeks
forecast3 <- predict(m3, future3) # use future time frame to predict m
plot(m3, forecast3) + 
  # initial plot with change points identified
  add_changepoints_to_plot(m3) +
  theme_tq() +
  labs(
    y = str_c(str_to_title(time_param), "Log Returns", sep = " "),
    x = "",
    title = "Prophet Model M3 MCMC Sampling",
    subtitle = "Red lines are Change Points"
  )
# note approximately 9 per year
tail(forecast3[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]) # for validation, delete later. Note vertical lines for inflection points
prophet_plot_components(m3, forecast3) # component analysis

dyplot.prophet(m3, forecast3) # interactive plot


#### Cross-validation #####
# now we measure forecast error with cross-validation
# take 53 weeks, predict 26 iteratively for the entire data set to see how accurate we are (will cycle through multiple times)
df.cv <- cross_validation(m1, initial = 64, horizon = 12, units = "weeks") # this will do 6 iterations of 4 markov chains
df.p <- performance_metrics(df.cv) # check performance
head(df.p) # note that our MSE drops with each simulation
tail(df.p)
plot_cross_validation_metric(df.cv, metric = "rmse")
plot_cross_validation_metric(df.cv, metric = "mape")
dyplot.prophet(m, df.cv)
######