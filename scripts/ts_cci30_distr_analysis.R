# Lib Load ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  "lubridate",
  "tidyquant",
  "tsibble",
  "patchwork",
  "fitdistrplus",
  "gamlss",
  "gamlss.dist",
  "gamlss.add",
  "tidyverse",
  "timetk",
  "anomalize"
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
return_col_shifted <- return_col %>% 
  as_tibble() %>%
  mutate(min_ret = min(value)) %>%
  mutate(val_shift = abs(min_ret) + .0000001) %>%
  mutate(shifted_val = value + val_shift) %>%
  pull(shifted_val)

# Return Dist ----
descdist(data = return_col_shifted, boot = 5000)
fit_n <- fitdist(return_col_shifted, "norm")
fit_l <- fitdist(return_col_shifted, "lnorm")
fit_g <- fitdist(return_col_shifted, "gamma")
fit_w <- fitdist(return_col_shifted, "weibull")

par(mfrow=c(2,2))
plot.legend <- c("Normal", "Log-Normal","Gamma","Weibull")
denscomp(list(fit_n, fit_l, fit_g, fit_w), legendtext = plot.legend)
cdfcomp (list(fit_n, fit_l, fit_g, fit_w), legendtext = plot.legend)
qqcomp  (list(fit_n, fit_l, fit_g, fit_w), legendtext = plot.legend)
ppcomp  (list(fit_n, fit_l, fit_g, fit_w), legendtext = plot.legend)
dev.off()

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
# sample tibble
n = 100
samp_tbl <- tibble(
  "sample_id" = 1:n
  , "data_sample" = replicate(
    n
    , sample(
      df.ts$Weekly_Log_Returns
      , 100
      , replace = TRUE
    ) %>% as_tibble()
    , simplify = FALSE
  )
)

unnested_tbl <- samp_tbl %>%
  unnest(cols = c(data_sample))

unnested_tbl %>%
  ggplot(
    mapping = aes(
      x = value
      , group = sample_id
    )
  ) +
  geom_density() +
  theme_tq() +
  labs(
    x = "Weekly Log Returns of CCI30"
    , title = "Sampled Weekly Log Returns"
    , subtitle = str_glue("Samples Used: {n}")
  )

unnested_tbl %>%
  group_by(sample_id) %>%
  summarise(mean_ret = mean(value)) %>%
  ungroup() %>%
  ggplot(
    mapping = aes(
      x = mean_ret
    )
  ) +
  geom_density(color = "red", size = 1) +
  theme_tq() +
  labs(
    x = "Mean Sample Density Estimate"
    , color = ""
  )
  