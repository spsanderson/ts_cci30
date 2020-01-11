# Load Libs ####
# Time Series analysis of CCI30 crypto index
install.load::install_load(
  "tidyquant"
  ,"timetk"
  # , "tibbletime"
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
)

# Daily OHLCV ####
url <- "https://cci30.com/ajax/getIndexHistory.php"
destfile <- "cci30_OHLCV.csv"
download.file(url, destfile = destfile)
df <- read.csv("cci30_OHLCV.csv")
class(df)

# Format Date ####
df$Date <- lubridate::ymd(df$Date)
df.tibble <- as_tsibble(df, index = Date)
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
  , y = df$Close
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

# Make ts object
df.ts <- tk_ts(
  df.tibble$Daily_Log_Return,
  start = c(min.year, min.month)
  , end = c(max.year, max.month)
  , frequency = 365
)
class(df.ts)
has_timetk_idx(df.ts)

mstl(df.ts)%>%
  autoplot(col = T) +
  theme_tq()

df.ts %>%
  autoplot() +
  theme_tq()

# Time Parameter ----
time_param <- "monthly"

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
    , span = 1/4
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
    , subtitle = "Twitter and GEST Methods"
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
    , subtitle = "Twitter and GEST Methods"
  )

# Make tsibble ---
# Add observed cleaned to df.ts in both a tibble and tsibble
df_tbl <- df.ts
class(df_tbl)

df_tsbl <- as_tsibble(df_tbl, index = Date)
class(df_tsbl)
# Make XTS object ####
# Forecast with FPP, will need to convert data to an xts/ts object
monthly.log.ret.ts <- ts(
  return_col
  , frequency = 12
  , start = c(min.year, min.month)
  , end = c(max.year, max.month)
)

monthly.sub.xts <- window(
  monthly.log.ret.ts
  , start = c(min.year, min.month)
  , end = c(max.year, max.month)
)
monthly.sub.xts

# TS components ####
components <- decompose(monthly.sub.xts)
names(components)
components$seasonal
plot(components)

# Get stl object ####
monthly.compl <- stl(monthly.sub.xts, s.window = "periodic")
plot(monthly.compl)

dfa_tsa <- df.tibble %>%
  time_decompose(Daily_Log_Return, method = "twitter") %>%
  anomalize(remainder, method = "gesd") %>%
  time_recompose()

dfa_tsa %>%
  plot_anomalies(
    ncol = 3
    , alpha_dots = 0.25
  ) +
  xlab("") +
  ylab("Daily Log Returns") +
  labs(
    title = "Anomaly Detection for CCI30 Daily Log Returns"
    , subtitle = "Twitter and GEST Methods"
  )

dfa_tsa %>%
  plot_anomaly_decomposition() + 
  xlab("Daily Log Return") + 
  ylab("Value") +
  labs(
    title = "Anomaly Detection for CCI30 Daily Log Return"
    , subtitle = "Method: GESD"
  )

dfa_tsb <- df.ts.monthly %>%
  time_decompose(Monthly_Log_Returns, method = "twitter") %>%
  anomalize(remainder, method = "gesd") %>%
  clean_anomalies() %>%
  time_recompose()

dfa_tsb %>%
  plot_anomalies(
    ncol = 3
    , alpha_dots = 0.25
  ) +
  xlab("") +
  ylab("Daily Log Returns") +
  labs(
    title = "Anomaly Detection for CCI30 Monthly Log Returns"
    , subtitle = "Twitter and GESD Methods"
  )

dfa_tsb %>%
  plot_anomaly_decomposition() +
  xlab("Monthly Log Return") +
  ylab("Value") +
  labs(
    title = "Anomaly Detection for CCI30 Monthly Log Returns"
    , subtitle = "Method: GESD"
  )

# HW Model ####
dfa_tsb %>% as_tsibble(index = Date) %>% autoplot(observed_cleaned)
monthly.fit.hw <- HoltWinters(monthly.sub.xts)
monthly.fit.hw
monthly.hw.est.params <- sw_tidy(monthly.fit.hw)
plot(monthly.fit.hw)
plot.ts(monthly.fit.hw$fitted)

# Forecast HW ####
monthly.hw.fcast <- hw(
  monthly.sub.xts
  , h = 12
  , alpha = monthly.fit.hw$alpha
  , gamma = monthly.fit.hw$gamma
  # , beta  = monthly.fit.hw$beta
)
summary(monthly.hw.fcast)

# HW Errors
monthly.hw.perf <- sw_glance(monthly.fit.hw)
mape.hw <- monthly.hw.perf$MAPE
model.desc.hw <- monthly.hw.perf$model.desc

# Monthly HW predictions
monthly.hw.pred <- sw_sweep(monthly.hw.fcast) %>%
  filter(sw_sweep(monthly.hw.fcast)$key == 'forecast')
print(monthly.hw.pred)
hw.pred <- head(monthly.hw.pred$value, 1)

# Vis HW predict ####
monthly.hw.fcast.plt <- sw_sweep(monthly.hw.fcast) %>%
  ggplot(
    aes(
      x = index
      , y = value
      , color = key
    )
  ) +
  geom_ribbon(
    aes(
      ymin = lo.95
      , ymax = hi.95
    )
    , fill = "#D5DBFF"
    , color = NA
    , size = 0
  ) +
  geom_ribbon(
    aes(
      ymin = lo.80
      , ymax = hi.80
      , fill = key
    )
    , fill = "#596DD5"
    , color = NA
    , size = 0
    , alpha = 0.8
  ) +
  geom_line(
    size = 1
  ) +
  labs(
    title = "Forecast for CCI30 Monthly Log Returns: 12-Month Forecast"
    , x = ""
    , y = ""
    ,  subtitle = paste0(
      "Model Desc - "
      , model.desc.hw
      , "\n"
      , "MAPE = "
      , round(mape.hw, 2)
      , " - Forecast = "
      , round(hw.pred, 3)
    )
  ) +
  scale_x_yearmon(n = 12, format = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq()
print(monthly.hw.fcast.plt)

# S-Naive Model ####
monthly.snaive.fit <- snaive(monthly.sub.xts, h = 12)
monthly.sn.pred <- sw_sweep(monthly.snaive.fit) %>%
  filter(sw_sweep(monthly.snaive.fit)$key == 'forecast')
print(monthly.sn.pred)
sn.pred <- head(monthly.sn.pred$value, 1)

# Calculate Errors
test.residuals.snaive <- monthly.snaive.fit$residuals
pct.err.snaive <- (test.residuals.snaive / monthly.snaive.fit$fitted) * 100
mape.snaive <- mean(abs(pct.err.snaive), na.rm = TRUE)

monthly.snaive.plt <- sw_sweep(monthly.snaive.fit) %>%
  ggplot(
    aes(
      x = index
      , y = value
      , color = key
    )
  ) +
  geom_ribbon(
    aes(
      ymin = lo.95
      , ymax = hi.95
    )
    , fill = "#D5DBFF"
    , color = NA
    , size = 0
  ) +
  geom_ribbon(
    aes(
      ymin = lo.80
      , ymax = hi.80
      , fill = key
    )
    , fill = "#596DD5"
    , color = NA
    , size = 0
    , alpha = 0.8
  ) +
  geom_line(
    size = 1
  ) +
  labs(
    title = "Forecast for CCI30 Monthly Log Returns: 12-Week Forecast"
    , x = ""
    , y = ""
    ,  subtitle = paste0(
      "Model Desc - S-Naive"
      , "\n"
      , "MAPE = "
      , round(mape.snaive, 2)
      , " - Forecast = "
      , round(sn.pred, 3)
    )
  ) +
  scale_x_yearmon(n = 12, format = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() 
print(monthly.snaive.plt)

# ETS Model #####
monthly.ets.fit <- monthly.sub.xts %>%
  ets()
summary(monthly.ets.fit)

monthly.ets.ref <- monthly.sub.xts %>%
  ets(
    ic = "bic"
    , alpha = monthly.ets.fit$par[["alpha"]]
    # , beta  = monthly.ets.fit$par[["beta"]]
    # , gamma = monthly.ets.fit$par[["gamma"]]
    # , phi   = monthly.ets.fit$par[["phi"]]
  )

# Caclulate errors
mape.ets <- sw_glance(monthly.ets.ref)$MAPE
monthly.ets.ref.model.desc <- sw_glance(monthly.ets.ref)$model.desc

# Forecast ets model
monthly.ets.fcast <- monthly.ets.ref %>%
  forecast(h = 12)

# Tidy Forecast Object
monthly.ets.pred <- sw_sweep(monthly.ets.fcast) %>%
  filter(sw_sweep(monthly.ets.fcast)$key == 'forecast')
print(monthly.ets.pred)
ets.pred <- head(monthly.ets.pred$value, 1)

# Visualize
monthly.ets.fcast.plt <- sw_sweep(monthly.ets.fcast) %>%
  ggplot(
    aes(
      x = index
      , y = value
      , color = key
    )
  ) +
  geom_ribbon(
    aes(
      ymin = lo.95
      , ymax = hi.95
    )
    , fill = "#D5DBFF"
    , color = NA
    , size = 0
  ) +
  geom_ribbon(
    aes(
      ymin = lo.80
      , ymax = hi.80
      , fill = key
    )
    , fill = "#596DD5"
    , color = NA
    , size = 0
    , alpha = 0.8
  ) +
  geom_line(
    size = 1
  ) +
  labs(
    title = "Forecast for CCI30 Monthly Log Returns: 12-Month Forecast"
    , x = ""
    , y = ""
    ,  subtitle = paste0(
      "Model Desc - "
      , monthly.ets.ref.model.desc
      , "\n"
      , "MAPE = "
      , round(mape.ets, 2)
      , " - Forecast = "
      , round(ets.pred, 3)
    )
  ) +
  scale_x_yearmon(
    n = 12
    , format = "%Y"
  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq()
print(monthly.ets.fcast.plt)

# Auto Arima ####
# Is the data stationary?
monthly.log.ret.ts %>% ur.kpss() %>% summary()
# Is the data stationary after differencing
monthly.log.ret.ts %>% diff() %>% ur.kpss() %>% summary()
# How many differences make it stationary
ndiffs(monthly.log.ret.ts)
dsch.diffs <- ndiffs(monthly.log.ret.ts)
# Seasonal differencing?
nsdiffs(monthly.log.ret.ts)
# Re-plot
monthly.log.ret.ts.diff <- diff(monthly.log.ret.ts)#, differences = rr.diffs)
plot.ts(monthly.log.ret.ts.diff)
acf(monthly.log.ret.ts.diff, lag.max = 20)
acf(monthly.log.ret.ts.diff, plot = F)

# Auto Arima
monthly.aa.fit <- auto.arima(monthly.log.ret.ts)
sw_glance(monthly.aa.fit)
monthly.aa.fcast <- forecast(monthly.aa.fit, h = 12)
tail(sw_sweep(monthly.aa.fcast), 12)

# Monthly AA predictions
monthly.aa.pred <- sw_sweep(monthly.aa.fcast) %>%
  filter(sw_sweep(monthly.aa.fcast)$key == 'forecast')
print(monthly.aa.pred)
aa.pred <- head(monthly.aa.pred$value, 1)

# AA Errors
monthly.aa.perf.model.desc <- sw_glance(monthly.aa.fit)$model.desc
mape.aa <- sw_glance(monthly.aa.fit)$MAPE

# Plot fitted aa model
monthly.aa.fcast.plt <- sw_sweep(monthly.aa.fcast) %>%
  ggplot(
    aes(
      x = index
      , y = value
      , color = key
    )
  ) +
  geom_ribbon(
    aes(
      ymin = lo.95
      , ymax = hi.95
    )
    , fill = "#D5DBFF"
    , color = NA
    , size = 0
  ) +
  geom_ribbon(
    aes(
      ymin = lo.80
      , ymax = hi.80
      , fill = key
    )
    , fill = "#596DD5"
    , color = NA
    , size = 0
    , alpha = 0.8
  ) +
  geom_line(
    size = 1
  ) +
  labs(
    title = "Forecast for CCI30 Monthly Log Returns: 12-Month Forecast"
    , x = ""
    , y = ""
    , subtitle = paste0(
      "Model Desc - "
      , monthly.aa.perf.model.desc
      , "\n"
      , "MAPE = "
      , round(mape.aa, 2)
      , " - Forecast = "
      , round(aa.pred, 3)
    )
  ) +
  scale_x_yearmon(n = 12, format = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq()
print(monthly.aa.fcast.plt)

# Bagged Model ####
monthly.bagged.model <- baggedModel(monthly.log.ret.ts)

# Forecast Bagged ETS Model
monthly.bagged.fcast <- forecast(monthly.bagged.model, h = 12)

# Tidy Forecast Object
monthly.bagged.pred <- sw_sweep(monthly.bagged.fcast) %>%
  filter(sw_sweep(monthly.bagged.fcast)$key == 'forecast')
print(monthly.bagged.pred)
bagged.pred <- head(monthly.bagged.pred$value, 1)

# Baggd Model Errors
pct.err.bagged <- (
  monthly.bagged.fcast$residuals / monthly.bagged.fcast$fitted
) * 100
mape.bagged <- mean(abs(pct.err.bagged), na.rm = T)

# Visualize
monthly.bagged.fcast.plt <- sw_sweep(monthly.bagged.fcast) %>%
  ggplot(
    aes(
      x = index
      , y = value
      , color = key
    )
  ) +
  geom_ribbon(
    aes(
      ymin = lo.100
      , ymax = hi.100
      , fill = key
    )
    , fill = "#596DD5"
    , color = NA
    , size = 0
    , alpha = 0.8
  ) +
  geom_line(
    size = 1
  ) +
  labs(
    title = "Forecast for CCI30 Monthly Log Returns: 12-Month Forecast"
    , x = ""
    , y = ""
    , subtitle = paste0(
      "Model Desc - Bagged ETS"
      , "\n"
      , "MAPE = "
      , round(mape.bagged, 2)
      , " - Forecast = "
      , round(bagged.pred, 3)
    )
  ) +
  scale_x_yearmon(
    n = 12
    , format = "%Y"
  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq()
print(monthly.bagged.fcast.plt)

# Compare models ####
qqnorm(monthly.hw.fcast$residuals)
qqline(monthly.hw.fcast$residuals)

qqnorm(monthly.snaive.fit$residuals)
qqline(monthly.snaive.fit$residuals)

qqnorm(monthly.ets.fcast$residuals)
qqline(monthly.ets.fcast$residuals)

qqnorm(monthly.aa.fcast$residuals)
qqline(monthly.aa.fcast$residuals)

qqnorm(monthly.bagged.fcast$residuals)
qqline(monthly.bagged.fcast$residuals)

checkresiduals(monthly.hw.fcast)
checkresiduals(monthly.snaive.fit)
checkresiduals(monthly.ets.fcast)
checkresiduals(monthly.aa.fcast)
checkresiduals(monthly.bagged.fcast)

# Pick Model ####
# 1 Month Pred
hw.pred <- head(monthly.hw.pred$value, 1)
hw.pred.lo.95 <- head(monthly.hw.pred$lo.95, 1)
hw.pred.hi.95 <- head(monthly.hw.pred$hi.95, 1)

sn.pred <- head(monthly.sn.pred$value, 1)
sn.pred.lo.95 <- head(monthly.sn.pred$lo.95, 1)
sn.pred.hi.95 <- head(monthly.sn.pred$hi.95, 1)

ets.pred <- head(monthly.ets.pred$value, 1)
ets.pred.lo.95 <- head(monthly.ets.pred$lo.95, 1)
ets.pred.hi.95 <- head(monthly.ets.pred$hi.95, 1)

aa.pred <- head(monthly.aa.pred$value, 1)
aa.pred.lo.95 <- head(monthly.aa.pred$lo.95, 1)
aa.pred.hi.95 <- head(monthly.aa.pred$hi.95, 1)

bagged.pred <- head(monthly.bagged.pred$value, 1)
bagged.pred.lo.100 <- head(monthly.bagged.pred$lo.100, 1)
bagged.pred.hi.10 <- head(monthly.bagged.pred$hi.100, 1)

mod.pred <- c(hw.pred, sn.pred, ets.pred, aa.pred, bagged.pred)

mod.pred.lo.95 <- c(
  hw.pred.lo.95
  , sn.pred.lo.95
  , ets.pred.lo.95
  , aa.pred.lo.95
  , bagged.pred.lo.100
)

mod.pred.hi.95 <- c(
  hw.pred.hi.95
  , sn.pred.hi.95
  , ets.pred.hi.95
  , aa.pred.hi.95
  , bagged.pred.hi.10
)

err.mape <- c(
  mape.hw
  , mape.snaive
  , mape.ets
  , mape.aa
  , mape.bagged
)

pred.tbl.row.names <- c(
  "HoltWinters"
  , "Seasonal Naive"
  , "ETS"
  , "Auto ARIMA"
  , "Bagged ETS"
)
pred.tbl <- data.frame(
  mod.pred
  , mod.pred.lo.95
  , mod.pred.hi.95
  , err.mape
)
rownames(pred.tbl) <- pred.tbl.row.names
pred.tbl <- tibble::rownames_to_column(pred.tbl)
pred.tbl <- arrange(pred.tbl, pred.tbl$err.mape)
print(pred.tbl)

# fbProphet Model ####
df.ts.monthly.prophet <- df.ts.monthly
colnames(df.ts.monthly.prophet) <- c('ds','y')

# Prophet Model
prophet.model <- prophet(df.ts.monthly.prophet)
prophet.future <- make_future_dataframe(prophet.model, periods = 12, freq = 'month')
tail(prophet.future, 12)

prophet.forecast <- predict(prophet.model, prophet.future)
prophet.one.month.pred <- tail(prophet.forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')],12)
prophet.pred <- head(prophet.one.month.pred$yhat, 1)
print(prophet.pred)
prophet.model.plt <- plot(
  prophet.model
  , prophet.forecast
) + 
  labs(
    title = "Forecast for CCI30 Monthly Log Returns: 12-Month Forecast"
    , subtitle = paste0(
      "Model Desc - fbProphet - Forecast = "
      , round(prophet.pred, 3)
    )
    , x = ""
    , y = ""
  ) +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq()
print(prophet.model.plt)

prophet.dyplt <- dyplot.prophet(prophet.model, prophet.forecast)
print(prophet.dyplt)

gridExtra::grid.arrange(
  monthly.hw.fcast.plt
  , monthly.snaive.plt
  , monthly.ets.fcast.plt
  , monthly.aa.fcast.plt
  , monthly.bagged.fcast.plt
  , prophet.model.plt
  , nrow = 3
  , ncol = 2
)

# AutoTS ####
library(RemixAutoML)

ats.output <- AutoTS(
  data = df.ts.monthly
  , TargetName = 'Monthly.Log.Returns'
  , DateName = 'Date'
  , FCPeriods = 12
  , TimeUnit = "month"
)


AutoTS(
  data = df.ts.tbl
  , TargetName = 'Log.Daily.Return'
  , DateName = 'Date'
  , FCPeriods = 30
  , HoldOutPeriods = round(nrow(df.ts.tbl) * 0.7, 0)
  , TimeUnit = "day"
) 
