# Load Libs ####
# Time Series analysis of CCI30 crypto index
library(tidyquant)
library(timetk)
library(tibbletime)
library(sweep)
library(anomalize)

# Daily OHLCV ####
url <- "https://cci30.com/ajax/getIndexHistory.php"
destfile <- "cci30_OHLCV.csv"
download.file(url, destfile = destfile)
df <- read.csv("cci30_OHLCV.csv")
rm(list = c('url','destfile'))

# Create TS ####
df$Date <- lubridate::ymd(df$Date)
df_ts <- tk_ts(df, start = 2015, frequency = 365)
head(df_ts)
str(df_ts)

# Create time aware tibble
df_tibble_ts <- as_tbl_time(df, index = Date)
df_tibble_ts

# Extract original index in the date format using tk_index
timetk_index <- tk_index(df_ts, timetk_idx = TRUE)
head(timetk_index)
class(timetk_index)

df_tibble_ts

idx_date <- tk_index(df_tibble_ts)
str(idx_date)
tk_get_timeseries_signature(idx_date)
df_tibble_ts_sig <- tk_augment_timeseries_signature(df_tibble_ts)
df_tibble_ts_sig

plot.ts(df_tibble_ts_sig[, c("Open","High","Low","Close")])

df_tibble_ts_sig %>%
  ggplot(aes(x = Date, y = Close)) +
  geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
  labs(title = "CCI30 Candlestick Chart",
       subtitle = "Zoomed in using coord_x_date",
       y = "Closing Price", x = "") +
  coord_x_date(xlim = c("2017-01-01", "2018-08-26"),
                ylim = c(min.value, max.value)) + 
  theme_tq()

# Take a look at data
max.value <- max(df_tibble_ts$Close)
min.value <- min(df_tibble_ts$Close)
start.date <- min(df_tibble_ts$Date)
end.date <- max(df_tibble_ts$Date)
training.region <- round(nrow(df_tibble_ts) * 0.7, 0)
test.region <- nrow(df_tibble_ts) - training.region
training.stop.date <- as.Date(max(df_tibble_ts$Date)) %m-% days(
  as.numeric(test.region))

df_tibble_ts %>%
  ggplot(
    aes(
      x = Date
      , y = Close
    )
  ) +
  geom_rect(
    xmin = as.numeric(ymd(training.stop.date))
    , xmax = as.numeric(ymd(end.date))
    , ymin = (min.value * 0.9)
    , ymax = (max.value * 1.1)
    , fill = palette_light()[[4]]
    , alpha = 0.01
  ) +
  annotate(
    "text"
    , x = ymd("2016-01-01")
    , y = 5000
    , color = palette_light()[[1]]
    , label = "Training Region"
  ) +
  annotate(
    "text"
    , x = ymd("2018-01-01")
    , y = 1200
    , color = palette_light()[[1]]
    , label = "Testing Region"
  ) +
  geom_point(
    alpha = 0.5
    , color = palette_light()[[1]]
  ) +
  labs(
    title = "Closing Price of CCI30: Daily Scale"
    , subtitle = "Source: www.cci30.com"
    , y = "Closing Price"
    , x = ""
  ) +
  theme_tq()

# Split data ####
train <- df_tibble %>%
  filter(index <= training.stop.date)
train

test <- df_tibble %>%
  filter(index > training.stop.date)
test

# Add time series signature to training set
train_augmented <- train %>%
  tk_augment_timeseries_signature()
train_augmented

# make linear models
fit_lm_a <- lm(Open ~ 
  index
  + year
  + half
  + quarter
  + month
  + qday
  + yday
  + mweek
  , data = train_augmented, na.action = na.exclude)
summary(fit_lm_a)
plot(fit_lm_a)
