# Data ----
url      <- "https://cci30.com/ajax/getIndexHistory.php"
destfile <- "00_data/cci30_OHLCV.csv"
download.file(url, destfile = destfile)