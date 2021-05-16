setwd("C:/Users/Marilena/Documents/StepUp/R/baselines_forecast/Benchmark")
require(caret)
require(forecast)
require(timeSeries)
require(tseries)
require(ISLR)
require(modules)
require(data.table)
require(Metrics)
require(stringr)

modules<- modules::use('../R')
transform <- modules$help_functions
models <- modules$models
cross_validation <- modules$cross_validation

# A. User-defined parameters and data input ------------------------------------

dt_In <- fread('../Data/model_sales.csv')
dt <- dt_In[, .(Time, YEAR, WEEK, PERIOD, Price, Sales = sales_w_noise)]
dt <- dt[!is.na(Sales)]
names(dt)

# B. Data preparation -------------------------------- 

v <- ts(dt$Sales, frequency = 52)
# Standardize
mean <- mean(v)
sd <- sd(v)    
v <- transform$standardize(v)

# D. Model evaluation and forecast ---------------------------------------------
# Find the model parameters from a train set (the size is approx 2 years)
# Calculate the prediction error on a test set (the size is 1 year)
# Use the model parameters from the train set and apply the model on the full dataset.
# Calculate the cross-validation error.
# Forecast with the model one year ahead
# The cross-validation takes some time so the for loop can be parallelized

# train-test split
end <- length(v)
test_set <- subset(v, start = end - 51, end = end)
train_set <- subset(v, start = 1, end = end-52)
# train the model on the train set
# prediction & prediction error
naive_predict <- snaive(y = train_set, h = 52)
rw_predict <- rwf(y = train_set, h = 52, drift = TRUE)
naive_fitting_error <- naive_predict$residuals[which(!is.na(naive_predict$residuals))]
naive_fitting_error <- sd*sqrt(mean(naive_fitting_error^2))

rw_fitting_error <- rw_predict$residuals[which(!is.na(rw_predict$residuals))]
rw_fitting_error <- sd*sqrt(mean(rw_fitting_error^2))

naive_prediction_error <- rmse(test_set, naive_predict$mean)*sd
rw_prediction_error <- rmse(test_set, rw_predict$mean)*sd

# Cross-Validation for one year horizon
# Each row of the matrix corresponds to the error of a step in the Cross-Validation
# Take the MSE for each cross-validation step. The cross-validation error
# corresponds to the average over all steps.

CV_naive <- tsCV(v, snaive, h = 52, initial = 52)
CV_error_naive <- cross_validation$get_tsCV_error(CV = CV_naive)*sd
# 197.738

CV_rwf <- tsCV(v, rwf, drift = TRUE, h = 52, initial = 52)
CV_error_rwf <- cross_validation$get_tsCV_error(CV = CV_rwf)*sd
# 175.3493

# Use the same model for 1 year forecast, but forecast only new periods
naive_forecast <- snaive(v, h = 52)
rw_forecast <- rwf(v, drift = TRUE, h = 52)

seas <- dt[Time %in% seq(end - 51, end)]$Sales
seas <- 52*seas/sum(seas)

dt_forecast <- data.table(Time = end + seq(1, 52), 
                          NAIVE_FORECAST = as.numeric(tail(x = naive_forecast$mean, n = 52)), 
                          RW_FORECAST = as.numeric(tail(x = rw_forecast$mean, n = 52)) * seas)

dt_forecast <- merge(dt_forecast, dt[, .(Time, Sales)], by = "Time", all = T)
dt_forecast <- merge(dt_forecast, dt_In[, .(Time, Actual_forecast)], all.x = T)

# E. Inverse standardise the HW forecast ------------------------------------
dt_forecast[,NAIVE_FORECAST := transform$inv_standardize(NAIVE_FORECAST,mean,sd)]
dt_forecast[,RW_FORECAST := transform$inv_standardize(RW_FORECAST,mean,sd)]

# F. Plot ---------------------------------------------------------
# Get a table with weekly periods
periods <- unique(dt[, .(Time, PERIOD, YEAR, WEEK)])
min_week <- periods[Time == 1]$WEEK
new_Index <- c(max(periods$Time)+(1:52))
new_weeks <- (new_Index+min_week-1)%%52
new_weeks <- replace(new_weeks, which(new_weeks == 0), values = 52)

new_years <- (new_Index+min_week-2)%/%52
new_periods <- str_c("W", 2015 + new_years, formatC(new_weeks, flag = "0", width = 2), sep = " ")
periods <- rbind(periods, data.table(Time = new_Index, PERIOD = new_periods, YEAR = new_years, WEEK = new_weeks))
View(periods)
dt_forecast <- merge(dt_forecast, periods, by = "Time", all.x = T)

dt_forecast[, WEEK := str_extract(PERIOD, pattern = "[:digit:]{2}$")]
dt_forecast[, YEAR := str_extract(PERIOD, pattern = "[:digit:]{4}")]
min_YEAR <- min(dt_forecast$YEAR)
dt_forecast[, YEAR:= as.numeric(YEAR)%%as.numeric(min_YEAR)+1]
dt_forecast[, PERIOD:=paste("Y", YEAR, "W", WEEK, sep = " ")]

colors <- c("Seasonal Naive" = "red","Naive with drift" = "red4", "Actual" = "black")
plot <- ggplot(data = dt_forecast, aes(x = Time)) +
  geom_point(aes(y = Sales, color = "Actual"), size = 0.5)+
  geom_line(aes(y = NAIVE_FORECAST, color = "Seasonal Naive"), size = 0.5) +
  geom_line(aes(y = RW_FORECAST, color = "Naive with drift"), size = 0.5) +
  geom_line(aes(y = Actual_forecast, color = "Actual"), size = 0.5)+
  scale_x_continuous(breaks = dt_forecast[seq(1, max(dt_forecast$Time), 10)]$Time, 
                     labels = dt_forecast[seq(1, max(dt_forecast$Time), 10)]$PERIOD, 
                     guide = guide_axis(angle = 90)) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = -15)))+
  theme_bw()+
  scale_color_manual(values = colors, breaks = c("Actual", "Seasonal Naive", 
                                                 "Naive with drift"))+
  labs(y = "Sales", color = NULL)+
  theme(legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6), 
        legend.text = element_text(size = 8))
plot
ggsave("../Documentation/figures/naive.png", width = 5, height = 3)
