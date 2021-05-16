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

# Create a vector with the new periods that will be used for regression
periods <- unique(dt$PERIOD)
week <- (as.numeric(str_extract(max(periods), pattern = "[:digit:]*$"))+1)%%52
year <- as.numeric(str_extract(max(periods), pattern = "(?<=W\\s)[:digit:]{4}"))
weeks <- seq(as.numeric(week), 52)

new_periods <- str_c("W", year+1, formatC(weeks, flag = "0", width = 2), sep = " ")

if (week > 1) {new_periods <- c(new_periods, str_c("W", year, formatC(seq(1, as.numeric(week)), flag = "0", width = 2), sep = " "))}

horizon <- uniqueN(new_periods)

# Train-test split for the main timeseries vector as well as the regression vector.
test_size <- 52
t <- max(dt$Time)

end <- length(v)
train_v <- subset(x = v, start = 1, end = t - test_size)
test_v <- subset(x = v, start = t - test_size + 1, end = t)
start_time <- start(train_v)
start_time <- (start_time[1]-1)*52+start_time[2]

# C. Forecast using autoarima --------------------------------------------------
# Estimate the order based on a single auto arima fit.
# Cross-validate the performance of that specific order.
# Forecast function using ARIMA
Arima_forecast_CV <- function(x, h, order, sorder){
  forecast(Arima(x, order = order, seasonal = sorder), 
           h = h)
}

# Take the order parameters on the full set and get the order of the model
system.time(
ss_arima_fit <- auto.arima(y = v, seasonal = T, stepwise = FALSE, 
                           approximation = FALSE,
                           parallel = T, num.cores = 8, allowdrift = F)
)

model_order <- arimaorder(ss_arima_fit)
order <- c(arimaorder(ss_arima_fit)[["p"]], arimaorder(ss_arima_fit)[["d"]], 
           arimaorder(ss_arima_fit)[["q"]])
sorder <- c(arimaorder(ss_arima_fit)[["P"]], arimaorder(ss_arima_fit)[["D"]], 
            arimaorder(ss_arima_fit)[["Q"]])

# Cross-validation error
# Cross-Validation for one year horizon
# Each row of the matrix corresponds to the error of a step in the Cross-Validation
# Take the MSE for each cross-validation step. The cross-validation error
# corresponds to the average over all steps.
CV <- models$tsCV(y = v,forecastfunction = Arima_forecast_CV, h = 52, 
                  initial = 52, step = 1, parallel = T, clusterSize = 8,
                  order = order, sorder = sorder)
CV_error <- cross_validation$get_tsCV_error(CV = CV)*sd

# training and testing errors
# train the model on the train set and take the regression coefficients
ss_arima_model<- try(Arima(y = train_v, order = order, seasonal = sorder))
if(class(ss_arima_model)=="try-error"){
  ss_arima_model<- Arima(y = train_v, order = order, seasonal = sorder, method = "CSS")
}
ss_arima_residuals <- residuals(ss_arima_model)
arima_fitting_error <- sqrt(mean(ss_arima_residuals^2))*sd

# prediction & prediction error referring to the regression coefficients
arima_predict <- forecast(ss_arima_model, h = 52)
ss_arima_prediction_error <- rmse(test_v, arima_predict$mean)*sd

# Train the model parameters using the full dataset, but keeping the model order
arima_fit_final <- Arima(y = v, model = ss_arima_model)

forecast <- forecast(arima_fit_final, h = 52)

# Combine the forecast to a new table
ss_arima_forecast <- data.table(Time = end + seq(1, 52), PERIOD = new_periods, 
                                SS_ARIMA_FORECAST = as.numeric(forecast$mean))

ss_arima_forecast[, SS_ARIMA_FORECAST := transform$inv_standardize(x = ss_arima_forecast$SS_ARIMA_FORECAST, mean = mean, std = sd)]


# D. Perform ARIMA on the order that is minimizing the cross-validation error -----
# Forecast function using ARIMA
Arima_forecast_CV_order <- function(x, h, order, s_order){
  forecast(Arima(x, order = order, seasonal = s_order, method = 'ML'), h = h)
}

# Get the values of the d parameter
d_values <- models$Get_d(ts = v, freq = 52, seasonal = T)

# Perform cross-validation running through different combinations of the model 
# parameters
system.time(
  res <- models$Get_params_CV(ts = v, d = d_values$d, ds = d_values$ds, seasonal = T,
                              clusterSize = 8, forecastfunction = Arima_forecast_CV_order,
                            chunkSize = 7, h_CV = 52, initial = 52, step = 10, 
                            qmax = 2, pmax = 2, psmax = 1, qsmax = 1))
#user  system elapsed 
#1.82    1.14 1153.42 #927.25

setorder(res$log, 'CV_error')
log_CV <- res$log
order <- c(res$CV_order$p, d_values$d, res$CV_order$q)
s_order <- c(res$CV_order$ps, d_values$ds, res$CV_order$qs)
CV_error_CV_order <- res$CV_order$CV_error*sd

# train the model on the train set
CV_arima_fit <- Arima(y = train_v, order = order, seasonal = s_order, method = "CSS")
CV_arima_residuals <- residuals(CV_arima_fit)
CV_arima_fitting_error <- sqrt(mean(CV_arima_residuals^2))*sd

# prediction & prediction error
CV_arima_predict <- forecast(CV_arima_fit, h = 52)
prediction_error <- rmse(test_v, CV_arima_predict$mean)*sd

# Train the model parameters using the full dataset, but keeping the model order
final_fit <- try(Arima(y = v, model = CV_arima_fit), silent = T)

# Get the actual forecast
forecast <- forecast(final_fit, h = horizon)

# start_time: the actual time when the ts starts, it can be different than 1 if there are leading zeros
# (1,end) is the actual length of the timeseries
CV_arima_forecast <- data.table(Time = end + seq(1, 52), PERIOD = new_periods, CV_ARIMA_FORECAST = forecast$mean)


# Get the table with the new periods
CV_arima_forecast[, CV_ARIMA_FORECAST := transform$inv_standardize(x = CV_arima_forecast$CV_ARIMA_FORECAST, 
                                                     mean = mean, std = sd)]
# Add the columns of the old sales to table
arima_forecast <- merge(ss_arima_forecast, CV_arima_forecast, by = c("Time", "PERIOD"), all = T)
new_dt <- merge(dt[, .(Time, PERIOD, Sales)], arima_forecast, by = c("Time", "PERIOD"), all = T)
new_dt <- merge(new_dt, dt_In[, .(Time, Actual_forecast)], by = "Time", all.x = T)

# E. Plot --------------------
#new_dt <- fread("data/arima.csv")
new_dt[, WEEK := str_extract(PERIOD, pattern = "[:digit:]{2}$")]
new_dt[, YEAR := str_extract(PERIOD, pattern = "[:digit:]{4}")]
min_YEAR <- min(new_dt$YEAR)
new_dt[, YEAR:= as.numeric(YEAR)%%as.numeric(min_YEAR)+1]
new_dt[, PERIOD:=paste("Y", YEAR, "W", WEEK, sep = " ")]

colors <- c("ARIMA from AIC" = "red", 
            "ARIMA from CV" = "blue", "Actual" = "black")
plot <- ggplot(data = new_dt, aes(x = Time)) +
  geom_point(aes(y = Sales, color = "Actual"), size = 0.5)+
  geom_line(aes(y = Actual_forecast, color = "Actual"), size = 0.5)+
  geom_line(aes(y = SS_ARIMA_FORECAST, color = "ARIMA from AIC"), size = 0.5) +
  geom_line(aes(y = CV_ARIMA_FORECAST, color = "ARIMA from CV"), size = 0.5)+
  scale_x_continuous(breaks = new_dt[seq(1, nrow(new_dt), 10)]$Time, 
                     labels = new_dt[seq(1, nrow(new_dt), 10)]$PERIOD, 
                     guide = guide_axis(angle = 90)) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = -15)))+
  theme_bw()+
  scale_color_manual(values = colors, breaks = c("Actual", "ARIMA from AIC", 
                                                 "ARIMA from CV"))+
  labs(y = "Sales", color = NULL)+
  theme(legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6), 
        legend.text = element_text(size = 8))
plot
ggsave("../Documentation/figures/arima_no_price.png", width = 5, height = 3)
fwrite(new_dt, "data/arima_no_price.csv", append = F)
#new_dt <- fread("data/arima.csv")
# autoarima: Regression with ARIMA(0,1,2)(1,1,1)[52]
ss_error <- data.table(fitting_error = arima_fitting_error, prediction_error = ss_arima_prediction_error, CV_error = CV_error)

# CV_arima: Regression with ARIMA(1,1,1)(1,1,1)[52] errors 
CV_error <- data.table(fitting_error = CV_arima_fitting_error, prediction_error = prediction_error, CV_error = CV_error_CV_order)

