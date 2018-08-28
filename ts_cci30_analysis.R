# Load Libs #########################################################
# Time Series analysis of CCI30 crypto index
library(tidyquant)
library(timetk)
library(tibbletime)
library(sweep)
library(anomalize)

# Daily OHLCV ######################################################
url <- "https://cci30.com/ajax/getIndexHistory.php"
destfile <- "cci30_OHLCV.csv"
download.file(url, destfile = destfile)
df <- read.csv("cci30_OHLCV.csv")
rm(list = c('url','destfile'))
