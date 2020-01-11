# Lib Load ####
install.load::install_load(
  "tidyquant"
  ,"timetk"
  #, "tibbletime"
  , "tsibble"
  , "sweep"
  , "anomalize"
  , "forecast"
  , "xts"
  , "fpp"
  , "lubridate"
  , "tidyverse"
  , "prophet"
  , "rvest"
  , "quantmod"
  , "RemixAutoML"
)

# Base URL ####
base_crypto_url = "https://finance.yahoo.com/cryptocurrencies?count=100&offset=0"

# Top 100 ####
crypto <- base_crypto_url %>%
  read_html() %>%
  html_table() %>%
  map_df(bind_cols) %>%
  as_tibble()

# Get list of symbols
crypto_symbol <- as.list(crypto$Symbol)

# Asset Loop ####
start = as.Date('2010-01-01')
end = Sys.Date()

df <- tibble(
  'Time'
  , 'Adjusted_Close'
  , 'Asset'
  , 'Monthly_Log_Return'
  , .rows = 0
)

column.names <- c(
  'Time'
  ,'Adj_Close'
  ,'Asset'
  )

df_sharpe <- tibble(
  'Asset'
  , 'VaRSharpe'
  , .rows = 0
)

col_names <- c(
  'Asset'
  , 'VaRSharpe'
)

# The size of the final portfolio
portfolio_size <- 5
# The assets that will be analyzed for correlation sorted by Sharpe Ratio and later by Rivin Sharpe
top_n_assets <- 10


for (i in 1:length(crypto_symbol)){

  print(
    paste0(
      "Processing data for: "
      , crypto_symbol[[i]]
      )
  )
  # Ticker Holder
  sym <- crypto_symbol[[i]]
  # Get xts object
  x <- getSymbols(sym, src = "yahoo", from = start, to = end, auto.assign = FALSE)
  # Get Adjusted Closing Price
  adj_p <- x[,6]
  # Coerces to timetk table
  adj_p <- tk_tbl(adj_p)
  # Clean update date
  adj_p$index <- lubridate::ymd(adj_p$index)
  # Get Asset name column
  adj_p$Asset <- crypto_symbol[[i]]
  # Coerce to tibble time
  adj_p <- as_tbl_time(adj_p, index = index)
  # Clean column names
  colnames(adj_p) <- column.names
  # drop na values
  adj_p <- adj_p %>% drop_na()
  # Make a monthly log returns of close object
  df_ts_monthly <- adj_p %>%
    tq_transmute(
      select = Adj_Close
      , periodReturn
      , period = "monthly"
      # , period = "weekly"
      , type = "log"
      , col_rename = "Monthly_Log_Returns"
    )
  # Join adj_p and df_ts_monthly by date into a monthly object
  df_join <- inner_join(adj_p, df_ts_monthly, by = "Time")
  # rbind data to final df
  df <- rbind(df, df_join)
  
  # Sharpe Ratio df
  df_sharpe_tmp <- df_join %>%
    tq_performance(
      Ra = Adj_Close
      , Rf = 0.03
      , performance_fun = SharpeRatio
    )
  sharpe_val <- df_sharpe_tmp[[3]]
  # df_sharpe_tmp column names
  df_sharpe_tmp <- tibble(
    sym
    , sharpe_val
  )
  colnames(df_sharpe_tmp) <- col_names
  # Bind rows to df_sharpe
  df_sharpe <- rbind(df_sharpe, df_sharpe_tmp)
  
}

# coerce to tibble time
df <- df %>%
  as_tbl_time(df, index = 'Time') %>%
  select(
    Time
    , Asset
    , Adj_Close
    , Monthly_Log_Returns
  )

df_sharpe <- df_sharpe %>% drop_na()
# Merge df and df_sharpe to only get those with VarSharpe
df_merged <- merge(
  df
  , df_sharpe
  , by = "Asset"
) %>%
  arrange(
    Time
    , Asset
  )

df_merged %>%
  filter(Monthly_Log_Returns != "Inf") %>%
  filter(Monthly_Log_Returns != "NaN") %>%
  ggplot(
    mapping = aes(
      x = Monthly_Log_Returns
    )
  ) + 
  geom_histogram(
    binwidth = 0.1
    , color = "black"
    , aes(fill = ..count..)
  ) +
  labs(
    title = "Histogram Montly Log Retrns"
    , y = ""
    , x = ""
  ) +
  scale_fill_gradient("Count", low = "blue", high = "red", guide = F) +
  theme_tq()

target <- "AE-USD"
test_data <- dplyr::filter(df_merged, Asset == target)

AutoTS(
  data = test_data
  , TargetName = "Monthly_Log_Returns"
  , DateName = 'Time'
  , FCPeriods = 12
  , HoldOutPeriods = round(nrow(test_data) * 0.7, 0)
  , TimeUnit = "month"
)
