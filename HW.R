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

hw_forecast <- data.table()

# forecasting function that will be used for cross-validation of the Holt-Winters method
f_hw <- function(x, h, alpha, beta, gamma){
  forecast(HoltWinters(x, alpha = alpha, beta = beta, gamma = gamma), h=h)
}

# train-test split
end <- length(v)
test_set <- subset(v, start = end - 51, end = end)
train_set <- subset(v, start = 1, end = end-52)
# train the model on the train set
hw_fit<- HoltWinters(train_set, seasonal = "additive")
alpha <- hw_fit$alpha
beta <- hw_fit$beta
gamma <- hw_fit$gamma
hw_residuals <- residuals(hw_fit)
hw_fitting_error <- sqrt(mean(hw_residuals^2))*sd
# prediction & prediction error
hw_predict <- forecast(hw_fit, h = 52)
prediction_error <- rmse(test_set, hw_predict$mean)*sd

# Cross-Validation for one year horizon
# Each row of the matrix corresponds to the error of a step in the Cross-Validation
# Take the MSE for each cross-validation step. The cross-validation erroe
# corresponds to the average over all steps.
CV <-tsCV(v, f_hw, h = 52, alpha = alpha, beta = beta, gamma = gamma, initial = 52)
CV_error <- cross_validation$get_tsCV_error(CV = CV)

# Use the same model for 1 year forecast, but forecast only new perioads
hw_fit_final <- HoltWinters(v, alpha = alpha, beta = beta, gamma = gamma)
#plot(c(hw_fit[[market]][[segment]]$fitted[, "xhat"], hw_predict$mean))
#lines(as.numeric(hw_fit_final$fitted[, "xhat"]))

forecast <- forecast(hw_fit_final, h = 52)
hw_forecast <- data.table(Time = end + seq(1, 52), 
                          HW_FORECAST = as.numeric(forecast$mean))

# Error calculation
# To inverse standardise the residuals I just multiply with the standard deviation, 
# because the mean cancels out
# Calculate the mean square root error
error <- data.table(hw_fitting_error = sd*sqrt(sum(hw_residuals**2)/length(hw_residuals)), 
                                hw_prediction_error = prediction_error*sd, 
                                hw_CV_error = CV_error*sd)


hw_forecast <- merge(hw_forecast, dt[, .(Time, Sales)], by = "Time", all = T)
View(hw_forecast)
hw_forecast <- merge(hw_forecast, dt_In[, .(Time, Actual_forecast)], all.x = T)

# E. Inverse standardise the HW forecast ------------------------------------
hw_forecast[,HW_FORECAST := transform$inv_standardize(HW_FORECAST,mean,sd)]

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
decomp <- merge(hw_forecast, periods, by = "Time", all.x = T)

colors <- c("Forecast" = "red", "Actual" = "black")
plot <- ggplot(data = hw_forecast, aes(x = Time)) +
  geom_point(aes(y = Sales, color = "Actual"), size = 0.5)+
  geom_line(aes(y = HW_FORECAST, color = "Forecast"), size = 0.5) +
  geom_line(aes(y = Actual_forecast, color = "Actual"), size = 0.5)+
  scale_x_continuous(breaks = decomp[seq(1, max(decomp$Time), 10)]$Time, 
                     labels = decomp[seq(1, max(decomp$Time), 10)]$PERIOD, 
                     guide = guide_axis(angle = 90)) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = -15)))+
  theme_bw()+
  scale_color_manual(values = colors, breaks = c("Actual", "Forecast"))+
  labs(y = "Sales", color = NULL)+
  theme(legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6), 
        legend.text = element_text(size = 8))
plot
ggsave("../Documentation/figures/hw.png", width = 5, height = 3)

fwrite(hw_forecast, "data/hw_forecast.csv", append = F)

