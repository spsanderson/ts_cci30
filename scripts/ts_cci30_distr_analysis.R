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
  "fitdistrplus",
  "gamlss",
  "gamlss.dist",
  "gamlss.add",
  "boot"
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
  dplyr::select(Date, Open, High, Low, Close, Volume)

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

fit_norm <- fitdist(return_col, "norm")
plot(fit_norm)
fit_norm$aic

gamlss_fit <- fitDist(
  return_col
  , k = 2
  , type = "realAll"
  , trace = FALSE
  , try.gamlss = TRUE
  )
summary(gamlss_fit)
plot(gamlss_fit)

# Boot Strap ----
n = 5000
mean = rep(NA, n)
sd   = rep(NA, n)
var  = rep(NA, n)
for (i in 1:n){
  samp <- sample(
    return_col
    , 500
    , replace = TRUE
  )
  mean[i] <- mean(samp)
  sd[i]   <- sd(samp)
  var[i]  <- var(samp)
}
hist(mean)
hist(sd)
hist(var)
