pacman::p_load(
  "quantmod",
  "libstableR",
  "EnvStats",
  "patchwork",
  "tidyverse",
  "janitor",
  "tidyquant"
)

getSymbols('ETH-USD') # Get daily price data from yahooFinance
eth <- `ETH-USD` # change the name of the data
eth <- eth[,4] %>% Delt(k=1) %>% na.omit() # pull adjusted close, convert to return, and remove NA data

# time_param <- "weekly"
# log_returns_tbl <- cci_index_tbl %>%
#   tq_transmute(
#     select = close
#     , mutate_fun = periodReturn
#     , period = time_param
#     , type = "log"
#     , col_rename = "value"
#   ) %>%
#   set_names("date_col", "value")

pars_init <- eth %>% coredata() %>% stable_fit_init()
pars_as <- eth %>% coredata() %>% stable_fit_koutrouvelis(pars_init = pars_init)
pars_gauss <- c(mean(coredata(eth)), sd(coredata(eth)))
pars_logistic <- eth %>% coredata() %>% as.numeric() %>% elogis()

N = nrow(eth)
rnd_as <- stable_rnd(N, pars = pars_as)
rnd_gauss <- rnorm(N, mean = pars_gauss[1], sd = pars_gauss[2])
rnd_logistic <- rlogis(N, location = pars_logistic$parameters[1],
                       scale = pars_logistic$parameters[2])

data_tbl <- tibble(
  ETH_Empirical = eth$Delt.1.arithmetic %>% as.numeric()
  , Random_Alpha_Stable = rnd_as
  , Random_Gaussian = rnd_gauss
  , Random_Logistic = rnd_logistic
) %>%
  rowid_to_column() %>%
  pivot_longer(
    -rowid
  )

p1 <- data_tbl %>%
  ggplot(aes(x = value, fill = as.factor(name))) +
  geom_density(alpha = .2) +
  theme_minimal() +
  scale_fill_tq() +
  facet_wrap(name ~ ., scales = "free") +
  labs(
    fill = "",
    x = "",
    y = ""
  ) +
  theme(legend.position = "bottom")

p2 <- data_tbl %>%
  filter(name %in% c("ETH_Empirical","Random_Alpha_Stable")) %>%
  ggplot(aes(x = value, fill = as.factor(name))) +
  geom_density(alpha = .2) +
  theme_minimal() +
  scale_fill_tq() +
  labs(
    x = "Log Returns"
    , y = "Density Estimate"
    , title = "ETH Weekly Empirical Log Returns"
    , fill = ""
  ) +
  theme(legend.position = "bottom")

p2 + p1

