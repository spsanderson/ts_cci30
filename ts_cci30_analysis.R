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

# Daily OHLCV ####
url <- "https://cci30.com/ajax/getIndexHistory.php"
destfile <- "cci30_OHLCV.csv"
download.file(url, destfile = destfile)
df <- read.csv("cci30_OHLCV.csv")
rm(list = c('url','destfile'))

# Format Date ####
df$Date <- lubridate::mdy(df$Date)
head(df, 1)
tail(df, 1)

# Coerce df to tibble ####
df <- as.tibble(df)

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
  start = 2015,
  frequency = 365
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
    , y = "Count"
    , x = ""
  ) +
  theme_tq()

m <- ts(df$Log.Daily.Return, frequency = 365.25, start(2015, 1))
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

# make linear models
fit_lm_a <- lm(
  Monthly.Log.Returns ~ Date
  + mweek
  , data = train_augmented, na.action = na.exclude)
summary(fit_lm_a)
plot(fit_lm_a)
