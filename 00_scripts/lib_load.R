if(!require(pacman)) {install.packages("pacman")}

pacman::p_load(
# * Forecasting Libraries ----
"forecast"     # Auto ARIMA, ETS
,"prophet"      # FB Prophet

# * Machine Learning Libraries ----
,"glmnet"       # Elastic Net
,"earth"        # Mars Regression Splines
,"kernlab"      # Support Vector Machine
,"kknn"         # K-Nearest Neighbors
,"randomForest" # Random Forest
,"ranger"       # Random Forest
,"xgboost"      # Boosted Trees
,"Cubist"       # Cubist Rule-Based Algorithm

# * Deep Learning Libraries ----
,"reticulate"   # Python interface

# * Time Series ML ----
,"tidymodels"   # Meta - workflows, parsnip, tune, dials, recipes, rsample, yardstick
,"rules"        # Rule-based models (cubist"
,"modeltime"    # tidymodels time series extension
,"modeltime.ensemble"
,"modeltime.resample"
,"modeltime.h2o"
,"modeltime.gluonts"

# * Core Libraries ----
,"tidyverse"    # Meta - dplyr, ggplot2, purrr, tidyr, stringr, forcats
,"lubridate"    # date and time
,"timetk"       # Time series data wrangling, visualization and preprocessing
,"tidyquant"

# Extras
,"DataExplorer"
,"fs"
,"janitor"
)