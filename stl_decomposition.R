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

# C. Decompose the sales using STL -------------------------------- 
# Take the timeseries of vectors that have at least two years of sales. Decompositions 
# needs at least two years of sales.
seas_decomp_add<- stl(v, s.window = 52, s.degree = 1, t.window = 53, robust = TRUE)
decomp <- data.table(Time = seq(1, length(v)), 
                     TREND = as.numeric(trendcycle(seas_decomp_add)), 
                     SEASONAL = as.numeric(seasonal(seas_decomp_add)),
                     REMAINDER = as.numeric(remainder(seas_decomp_add)))


# Remove NA from TREND components
decomp <-decomp[!is.na(TREND)]

# D. Apply regression to the trend component -----------------------------------

decomp <- merge(decomp, dt[, .(Time, Price)], by = "Time", all.x = T)

t_max <- max(decomp$Time)
# __ 1. Perform cross validation -----------------------------------------------
#
# For each market-segment take the trend and apply regression.
# Model: It is a linear model, but I need to explore non-linear models.
# Cross-validation : 
# How does the linear regression model generalize for prediction over different regions of the dataset?
# Cross-Validation error measured for one year forecasts.

# Cross-Validation with an expanding window
CV_error <- data.table()
for (n in seq(-5, 5)){
  for(m in seq(1, 5)){
    if(n < 0){
      expanding_CV_error <- cross_validation$nlm_CV(dt = decomp,forecast_window = 52, 
                                                    training_window = 52,
                                                    timestep = 1,
                                                    formula = TREND ~ poly(Time, m) + poly(1/Price, (-n)),
                                                    method = 'expanding', target = "TREND")
    }else{
      if(n==0){
        expanding_CV_error <- cross_validation$nlm_CV(dt = decomp,forecast_window = 52, 
                                                      training_window = 52,
                                                      timestep = 1,
                                                      formula = TREND ~ poly(Time, m),
                                                      method = 'expanding', target = "TREND")
      }else{
        expanding_CV_error <- cross_validation$nlm_CV(dt = decomp,forecast_window = 52, 
                                                      training_window = 52,
                                                      timestep = 1,
                                                      formula = TREND ~ poly(Time, m) + poly(Price, n),
                                                      method = 'expanding', target = "TREND")
      }
    }
    CV_error <- rbind(CV_error, list(n = n, m = m, expanding_CV_error = expanding_CV_error))
  }
}

setorder(CV_error,n, m)

# Cross-Validation with an rolling window
#rolling_error <- data.table()
#for (n in seq(1, 5)){
#  for(m in seq(1, 5)){      
#    rolling_error <- rbind(rolling_error, list(n = n, m = m,rolling_CV_error = cross_validation$nlm_CV(dt = decomp,
#                                                                                                       forecast_window = 52,training_window = 52,
#                                                                                                       timestep = 4,formula = TREND ~ poly(Time, m) + poly(Price, n),
#                                                                                                       method = 'rolling', target = "TREND")))      }
#}
#CV_error <- merge(CV_error, rolling_error, by = c("n", "m"), all.x = T)

# Keep the polynomial orders that minimize the Cross-Validation error
CV_error[, min_expanding_CV_error := min(expanding_CV_error)]
error <- CV_error[expanding_CV_error == min_expanding_CV_error, .(n, m, expanding_CV_error)]
error
#    n m expanding_CV_error
#   -1 2         0.08504647

# __ 2. Trend forecast ------------------------------------------------------------------
# Make the prediction with train-test splitting (use 3 years for training and 1 year for testing) 
# using the optimal polynomial

# train-test split
test_set <- decomp[Time %between% c(t_max-51, t_max), .(Time, TREND, Price)]
train_set <- decomp[Time < t_max-51, .(Time, TREND, Price)]
t0 <- min(train_set$Time)
# model fitting
O_Index <- error$m
O_Price <- error$n
if(O_Price<0){
  lm.fit <- lm(TREND ~ poly(Time, O_Index) + poly(1/Price, -(O_Price)), data = train_set)
}else{
  if(O_Price == 0){
    lm.fit <- lm(TREND ~ poly(Time, O_Index), data = train_set)
  }else{
    lm.fit <- lm(TREND ~ poly(Time, O_Index) + poly(Price, O_Price), data = train_set)
  }
}
# prediction & prediction error
prediction <- predict(lm.fit, test_set[, .(Time, Price)])
prediction_error <- sd*rmse(actual = test_set$TREND, predicted = prediction)
#26.7695

# take 1 year forecast
horizon <- max(dt_In$Time)-t_max
price1 <- dt[Time == t_max]$Price
price2 <- rep(last(price1), times = horizon - length(price1))
forecast <- predict(lm.fit, data.table(Time = t_max+seq(1:horizon), Price = c(price1,price2)))
dt_forecast <- data.table(Time = t_max+seq(1, horizon), 
                          TREND_FORECAST = forecast)

decomp <- merge(decomp, dt_forecast, by = c("Time"), all = T)

plot(decomp[, .(Time, TREND)])

points(decomp[, .(Time, TREND_FORECAST)])
plot(dt_forecast[, .(Time, TREND_FORECAST)])

# F. Add the fitting error and the decomposition error -------------------------

reg_residuals <- residuals(lm.fit)

# To inverse standardise the residuals I just multiply with the standard deviation, 
# because the mean cancels out
# Calculate the mean square root error

TREND_Fitting_error <- sd*sqrt(sum((reg_residuals)**2)/length(reg_residuals)) 
# 8.876328

decomp_residuals <- decomp[!is.na(REMAINDER), .(Time, REMAINDER)]

# Here the trend fitting error might be adding up to some of the residuals error
# The trend fitting error is insignificant compared to the decomposition error.
decomposition_error <- sd*sqrt(sum(decomp_residuals$REMAINDER**2)/length(decomp_residuals$REMAINDER))
#49.74199
CV_error <- error$expanding_CV_error*sd
#29.80087

# G. Add the seasonal component to the trend forecast --------------------------
# Take naive forecast for the seasonal component, just replicating the values of the last 52 weeks
decomp[, lead_seas := shift(SEASONAL, type = "lag", n = 52)]
decomp[is.na(SEASONAL), SEASONAL := lead_seas]
decomp[,lead_seas:=NULL]

View(decomp)

decomp[, Sales_FORECAST := TREND_FORECAST + SEASONAL]

# H. Inverse standardise the forecast and decomposition ------------------------
decomp[, Sales_FORECAST := transform$inv_standardize(Sales_FORECAST,mean,sd)]

# Add the original Sales info
decomp <- merge(decomp, dt[, .(Time, Sales)], by = "Time", all = T)
# Add also the Actual forecast
decomp <- merge(decomp, dt_In[, .(Time, Actual_forecast)], all.x = T)

# I. Plot ----------------------------------------------------------------------
# Get a table with weekly periods
periods <- unique(dt[, .(Time, PERIOD, YEAR, WEEK)])
min_week <- periods[Time == 1]$WEEK
new_Index <- c(max(periods$Time)+(1:52))
new_weeks <- (new_Index+min_week-1)%%52
new_weeks <- replace(new_weeks, which(new_weeks == 0), values = 52)

new_years <- (new_Index+min_week-2)%/%52
new_periods <- str_c("W", 2015 + new_years, formatC(new_weeks, flag = "0", width = 2), sep = " ")
periods <- rbind(periods, data.table(Time = new_Index, PERIOD = new_periods, YEAR = new_years, WEEK = new_weeks))
decomp <- merge(decomp, periods, by = "Time", all.x = T)

colors <- c("Forecast" = "red", "Actual" = "black")
plot <- ggplot(data = decomp, aes(x = Time)) +
  geom_point(aes(y = Sales, color = "Actual"))+
  geom_line(aes(y = Sales_FORECAST, color = "Forecast"), size = 1) +
  geom_line(aes(y = Actual_forecast, color = "Actual"), size = 1)+
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

# J. STL forecast using the Naive/drift/ets methods ----------------------------

# train-test split
end <- length(v)
test_set <- subset(v, start = end - 52, end = end)
train_set <- subset(v, start = 1, end = end-53)
# train the model on the train set
stl_train<- stl(train_set, s.window = 52, s.degree = 1, t.window = 53, robust = TRUE)
stl_residuals <- remainder(stl_train)
stl_fitting_error = sd*sqrt(sum(stl_residuals^2)/length(stl_residuals))

# prediction & prediction error
stl_predict_naive <- forecast(stl_train, h = 52, method = "naive")
stl_predict_rw_drift <- forecast(stl_train, h = 52, method = "rwdrift")
stl_predict_ets <- forecast(stl_train, h = 52, method = "ets")

stl_fitting_error_naive <- residuals(stl_predict_naive)[which(!is.na(residuals(stl_predict_naive)))]
stl_fitting_error_naive <- sqrt(mean(stl_fitting_error_naive^2))*sd

stl_fitting_error_rw_drift <- residuals(stl_predict_rw_drift)[which(!is.na(residuals(stl_predict_rw_drift)))]
stl_fitting_error_rw_drift <- sqrt(mean(stl_fitting_error_rw_drift^2))*sd

stl_fitting_error_ets <- residuals(stl_predict_ets)[which(!is.na(residuals(stl_predict_ets)))]
stl_fitting_error_ets <- sqrt(mean(stl_fitting_error_ets^2))*sd

# "STL +  ETS(A,A,N)"
#ETS(A,A,N) 

#Smoothing parameters:
#  alpha = 0.105 
#beta  = 0.0038 

#Initial states:
#  l = -1.2711 
#b = 0.0017 

#sigma:  0.1502

prediction_error_naive <- rmse(test_set, stl_predict_naive$mean)*sd
prediction_error_rw_drift <- rmse(test_set, stl_predict_rw_drift$mean)*sd
prediction_error_ets <- rmse(test_set, stl_predict_ets$mean)*sd

# Cross-Validation for one year horizon
f_stl <- function(x, h, method){
  stl_x <- stl(x, s.window = 52, s.degree = 1, t.window = 53, robust = TRUE)
  forecast(stl_x, h = h, method = method)
}

CV_naive <-tsCV(v, f_stl, h = 52, method = "naive", initial = 52)
CV_rw_drift <-tsCV(v, f_stl, h = 52, method = "rwdrift", initial = 52)
CV_ets <-tsCV(v, f_stl, h = 52, method = "ets", initial = 52)

CV_error_naive <- cross_validation$get_tsCV_error(CV_naive)*sd
CV_error_rw_drift <- cross_validation$get_tsCV_error(CV_rw_drift)*sd
CV_error_ets <- cross_validation$get_tsCV_error(CV_ets)*sd

# Forecasts - use the full dataset and forecast for 1 year
forecast_naive <- forecast(seas_decomp_add, h = 52, method = "naive")
forecast_rw_drift <- forecast(seas_decomp_add, h = 52, method = "rwdrift")
forecast_ets <- forecast(seas_decomp_add, h = 52, method = "ets")

stl_forecast <- data.table(Time = t_max + seq(1, 52),
                           STL_NAIVE_FORECAST = as.numeric(forecast_naive$mean),
                           STL_RW_DRIFT_FORECAST = as.numeric(forecast_rw_drift$mean), 
                           STL_ETS_FORECAST = as.numeric(forecast_ets$mean))    


decomp <- merge(decomp[, .(Time, PERIOD, Sales_FORECAST, Sales, Actual_forecast)], 
                stl_forecast, by = "Time", all.x = T)
View(decomp)

# __2. Inverse standardise the stl forecast ------------------------------------
decomp[, STL_NAIVE_FORECAST := transform$inv_standardize(STL_NAIVE_FORECAST,mean,sd)]
decomp[, STL_RW_DRIFT_FORECAST := transform$inv_standardize(STL_RW_DRIFT_FORECAST,mean,sd)]
decomp[, STL_ETS_FORECAST := transform$inv_standardize(STL_ETS_FORECAST,mean,sd)]

# __3. Add to the plot ---------------------------------------------------------
colors <- c("STL - NLR & seasonal naive" = "red", 
            "STL & seasonal naive" = "red4", 
            "STL & drift" = "red3", "STL & ETS" = "orange", 
            "Actual" = "black")
plot <- ggplot(data = decomp, aes(x = Time)) +
  geom_point(aes(y = Sales, color = "Actual"))+
  geom_line(aes(y = Sales_FORECAST, color = "STL - NLR & seasonal naive"), size = 1) +
  geom_line(aes(y = STL_NAIVE_FORECAST, color = "STL & seasonal naive"), size = 1, linetype = "dashed") +
  geom_line(aes(y = STL_RW_DRIFT_FORECAST, color = "STL & drift"), size = 1, linetype = "dotted") +
  geom_line(aes(y = STL_RW_DRIFT_FORECAST, color = "STL & ETS"), size = 1, linetype = "dotted") +
  geom_line(aes(y = Actual_forecast, color = "Actual"), size = 1)+
  scale_x_continuous(breaks = decomp[seq(1, max(decomp$Time), 10)]$Time, 
                     labels = decomp[seq(1, max(decomp$Time), 10)]$PERIOD, 
                     guide = guide_axis(angle = 90)) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = -15)))+
  theme_bw()+
  scale_color_manual(values = colors, breaks = c("Actual", "STL - NLR & seasonal naive", 
                                                 "STL & seasonal naive", 
                                                 "STL & drift", "STL & ETS"))+
  labs(y = "Sales", color = NULL)+
  theme(legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6), 
        legend.text = element_text(size = 8))
plot

ggsave("figures/stl_decomposition.png", width = 5, height = 3)

fwrite(decomp[, .(Time, PERIOD, Sales_FORECAST, STL_NAIVE_FORECAST, 
                  STL_RW_DRIFT_FORECAST, STL_ETS_FORECAST, Sales, Actual_forecast)], 
       "data/STL_decomposition_nl.csv", append = F)

