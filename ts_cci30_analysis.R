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
# create time aware tibble
df_tibble <- as_tbl_time(df, index = Date)
df_tibble

# Extract original index in the date format using tk_index
timetk_index <- tk_index(df_ts, timetk_idx = TRUE)
head(timetk_index)
