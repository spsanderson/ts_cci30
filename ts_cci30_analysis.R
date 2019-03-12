# Load Libs ####
# Time Series analysis of CCI30 crypto index
library(tidyquant)
library(timetk)
library(tibbletime)
library(sweep)
library(anomalize)
library(caret)
library(forecast)
library(funModeling)
library(xts)
library(fpp)
library(forecast)
library(lubridate)
library(dplyr)
library(urca)
library(prophet)

# Daily OHLCV ####
url <- "https://cci30.com/ajax/getIndexHistory.php"
destfile <- "cci30_OHLCV.csv"
download.file(url, destfile = destfile)
df <- read.csv("cci30_OHLCV.csv")
rm(list = c('url','destfile'))

# Format Date ####
df$Date <- lubridate::ymd(df$Date)
df.tibble <- as_tbl_time(df, index = Date)
head(df.tibble, 1)
tail(df.tibble, 1)

min.date  <- min(df.tibble$Date)
min.year  <- year(min.date)
min.month <- month(min.date)
max.date  <- max(df.tibble$Date)
max.year  <- year(max.date)
max.month <- month(max.date)

# Coerce df to tibble ####
df <- as_tibble(df)

featurePlot(
  x = df[,c("Open","High","Low","Volume")]
  , y = df$Close
  , plot = "pairs"
  , auto.key = list(columns = 4)
  , na.action(na.omit)
)

# Add varaibles ####
df <- df %>% tq_mutate(
  select = Close
  , mutate_fun = dailyReturn
  )
head(df, 5)

df <- df %>% tq_mutate(
  select = Close
  , mutate_fun = periodReturn
  , period = "daily"
  , type = "log"
)
head(df, 5)

df <- rename(df, Log.Daily.Return = daily.returns..1)
head(df, 5)

profiling_num(df$Log.Daily.Return)

# Create Time Aware Tibble ####
df.ts.tbl <- as_tbl_time(df, index = Date)
head(df.ts.tbl)
str(df.ts.tbl)
plot.ts(df.ts.tbl)

# Make ts object from time aware tibble
df.ts <- tk_ts(
  df.ts.tbl,
  start = c(min.year, min.month)
  , end = c(max.year, max.month)
  , frequency = 365
  )
class(df.ts)

# timetk_index <- tk_index(tk_d, timetk_idx = TRUE)
# head(timetk_index)
# class(timetk_index)
has_timetk_idx(df.ts)

# Make a monthly log returns of close object
df.ts.monthly <- df.ts.tbl %>%
  tq_transmute(
    select = Close
    , periodReturn
    , period = "monthly"
    , type = "log"
    , col_rename = "Monthly.Log.Returns"
    )
head(df.ts.monthly, 5)

# Get some Params ####
# get max and min discharges
max.daily.log.return <- max(df.ts.monthly$Monthly.Log.Returns)
min.daily.log.return <- min(df.ts.monthly$Monthly.Log.Returns)
start.date <- min(df.ts.monthly$Date)
end.date   <- max(df.ts.monthly$Date)

training.region <- round(nrow(df.ts.monthly) * 0.7, 0)
test.region     <- nrow(df.ts.monthly) - training.region
training.stop.date <- as.Date(max(df.ts.monthly$Date)) %m-% months(
  as.numeric(test.region), abbreviate = F)

plot.ts(df.ts.monthly$Monthly.Log.Returns)

# Plot intial Data ####
df.ts.monthly %>%
  ggplot(
    aes(
      x = Date
      , y = Monthly.Log.Returns
    )
  ) +
  geom_rect(
    xmin = as.numeric(ymd(training.stop.date))
    , xmax = as.numeric(ymd(end.date))
    , ymin = (min.daily.log.return * 1.1)
    , ymax = (max.daily.log.return * 1.1)
    , fill = palette_light()[[4]]
    , alpha = 0.01
  ) +
  annotate(
    "text"
    , x = ymd("2016-01-01")
    , y = min.daily.log.return
    , color = palette_light()[[1]]
    , label = "Training Region"
  ) +
  annotate(
    "text"
    , x = ymd("2018-06-01")
    , y = max.daily.log.return
    , color = palette_light()[[1]]
    , label = "Testing Region"
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
    , method = 'auto'
    , color = 'red'
  ) +
  labs(
    title = "Monthly Returns: Log Scale"
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

m <- ts(df$Log.Daily.Return, frequency = 365.25, start(min.year, min.month))
components <- decompose(m)
plot(components)
ggAcf(m)

# Split data ####
train_data <- df.ts.monthly %>%
  filter(Date <= training.stop.date)
train_data

test_data <- df.ts.monthly %>%
  filter(Date > training.stop.date)
test_data

# Add time series signature to training set
train_augmented <- train_data %>%
  tk_augment_timeseries_signature()
train_augmented

# Make XTS object ####
# Forecast with FPP, will need to convert data to an xts/ts object
monthly.log.ret.ts <- ts(
  df.ts.monthly$Monthly.Log.Returns
  , frequency = 12
  , start = c(min.year, min.month)
  , end = c(max.year, max.month)
)
plot.ts(monthly.log.ret.ts)
class(monthly.log.ret.ts)
monthly.xts <- as.xts(monthly.log.ret.ts)
head(monthly.xts)
monthly.sub.xts <- window(
  monthly.log.ret.ts
  , start = c(min.year, min.month)
  , end = c(max.year, max.month)
)
monthly.sub.xts

# TS components ####
monthly.components <- decompose(monthly.sub.xts)
names(monthly.components)
monthly.components$seasonal
plot(monthly.components)

# Get stl object ####
monthly.compl <- stl(monthly.sub.xts, s.window = "periodic")
plot(monthly.compl)

# HW Model ####
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
      , " - MAPE = "
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
    title = "Forecast for CCI30 Monthly Log Returns: 12-Month Forecast"
    , x = ""
    , y = ""
    ,  subtitle = paste0(
      "Model Desc - S-Naive - MAPE = "
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
      , " - MAPE = "
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
      , " - MAPE = "
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
      "Model Desc - Bagged ETS - MAPE = "
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
gridExtra::grid.arrange(
  monthly.hw.fcast.plt
  , monthly.snaive.plt
  , monthly.ets.fcast.plt
  , monthly.aa.fcast.plt
  , monthly.bagged.fcast.plt
  , nrow = 3
  , ncol = 2
)

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
tail(prophet.forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')],12)
prophet.model.plt <- plot(
  prophet.model
  , prophet.forecast
  , main = "Forecast for CCI30 Monthly Log Returns: 12-Month Forecast"
  , sub = "Model Desc - fbProphet - Forecast = 0.190"
  , xlab = ""
  , ylab = ""
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
