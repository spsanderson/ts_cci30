# Lib Load ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  "prophet",
  "dplyr",
  "Matrix",
  "lubridate",
  "RcppRoll",
  "magrittr"
)

# Read in the data sets
#data<- read.csv("CACC_TRAININGC2_classification.csv", stringsAsFactors = FALSE)
#new_data<- read.csv("Clustered_new_products.csv", stringsAsFactors = FALSE)
# Adjust dtg
data$DATE <- lubridate::mdy(data$DATE)

##### Cluster 1 style 082 / 087 #####
# create inner joint segments between two data files to analyze
new_id<- unique(new_data$cluster)
tdata<- data[data$cluster %in% new_id,]
group<- tdata[tdata$cluster == "1",]
group <- group[group$CHANNEL == "Online",]
group <- group[group$FABRICATION == "Synthetic",]
table(group$SUBCATEGORY_DESC) # checking for more than 1,000 data points
# aggregate data
group_mean<- aggregate(DEMAND~TOTAL_WEEKS+FISCAL_YEAR, data = group, FUN = mean)
# transition to ds / y formats
ds<-  seq(as.Date(range(group$DATE)[1]), as.Date(range(group$DATE)[2]), "weeks")
y<- group_mean$DEMAND
# create a single dataframe
df<- data.frame(ds, y)
range(df$ds) # checking that the time frame for all training data
# establish a saturating minimum (lower bound)
# Saturating minimum (also cap required)
# Set Current cap / floor
df$cap <- max(df$y) * 1.2 # take max in that category and increase it by a factor
df$floor <- 0 # nothing below 0
# Fit the model
m <- prophet(df, growth = 'linear', mcmc.samples = 1500, yearly.seasonality = TRUE, interval.width = 0.95)
# could also use growth = 'logistic', but it is less accurate for this. You do not need to set mcmc samples (markov chain), 
# but we found it to be more accurate. Make samples > 300 minimum, we found anything less than 1,000 had errors. 
# Identify whether you want yearly.seasonality, monthly.seasonality, weekly.seasonlity etc.
future <- make_future_dataframe(m, periods = 44, freq = 'week')  # sets the prediction frequency to week and covers through wk 2, june 2020
range(future$ds)# makes sure we have enough predictions
# Set future cap / floor
future$cap <- max(df$y) * 3 # same
future$floor <- 0 # same
# adjust predictions

# make future predictions for a determined amount of weeks
forecast <- predict(m, future) # use future time frame to predict m
plot(m, forecast)  + add_changepoints_to_plot(m) # initial plot with change points identified
# note approximately 9 per year
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]) # for validation, delete later. Note vertical lines for inflection points
prophet_plot_components(m, forecast) # component analysis

dyplot.prophet(m, forecast) # interactive plot
Style082_Linear <- forecast

Style082_Linear_filtered <- Style082_Linear %>%
  filter(ds >= "2020-03-01") %>%
  filter(ds < "2020-06-01") # grab only those elements within the correct time window
Style082_Linear_filtered$yhat[Style082_Linear_filtered$yhat < 0] <- 0 # if any y predictions are below 0, change them to 0
sum(Style082_Linear_filtered$yhat) # sum of the total in the time period
View(Style082_Linear_filtered) # open, scroll right, yhat are your predictions, yhat upper are your upper bound.


#### Cross-validation #####
# now we measure forecast error with cross-validation
# take 53 weeks, predict 26 iteratively for the entire data set to see how accurate we are (will cycle through multiple times)
df.cv <- cross_validation(m, initial = 53, horizon = 26, units = 'weeks') # this will do 6 iterations of 4 markov chains
df.p <- performance_metrics(df.cv) # check performance
head(df.p) # note that our MSE drops with each simulation
tail(df.p)
plot_cross_validation_metric(df.cv, metric = "rmse")
plot_cross_validation_metric(df.cv, metric = "mape")
dyplot.prophet(m, df.cv)
######