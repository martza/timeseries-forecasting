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
require(foreach)
require(parallel)
require(doParallel)
require(mgcv)
modules<- modules::use('../R')
transform <- modules$help_functions
models <- modules$models
cross_validation <- modules$cross_validation

# A. User-defined parameters and data input ------------------------------------

dt_In <- fread('../Data/model_sales.csv')
dt <- dt_In[, .(Time, YEAR, WEEK, PERIOD, Price, Sales = sales_w_noise)]
dt <- dt[!is.na(Sales)]
names(dt)
dt[, Price1:=1/Price]
# B. Data preparation ----------------------------------------------------------

# C. Decompose the sales per market and segment -------------------------------- 
# __ 1.Preparation for decomposition -------------------------------------------
# Take the timeseries of vectors that have at least two years of sales. Decompositions 
# needs at least two years of sales.

v <- ts(dt$Sales, frequency = 52)
# Standardize
mean <- mean(v)
sd <- sd(v)    
v <- transform$standardize(v)


# __ 2.Get classical decomposition of the sales --------------------------------

seas_decomp_add<- decompose(v, type = 'additive')

decomp <- data.table(Time = seq(1, length(v)), 
                     TREND = as.numeric(trendcycle(seas_decomp_add)), 
                     SEASONAL = as.numeric(seasonal(seas_decomp_add)),
                     REMAINDER = as.numeric(remainder(seas_decomp_add)))
seasonal <- data.table(seas_Index = seq(1:52), FIGURE = seas_decomp_add$figure)

# Remove NA from TREND components
decomp <-decomp[!is.na(TREND)]

# D. Apply regression to the trend component -----------------------------------

decomp <- merge(decomp, dt[, .(Time, Price, Price1)], by = "Time", all.x = T)

t_max <- max(decomp$Time)
# __ 2. Perform cross validation -----------------------------------------------
#
# For each market-segment take the trend and apply regression.
# Model: It is a linear model, but I need to explore non-linear models.
# Cross-validation : 
# How does the linear regression model generalize for prediction over different regions of the dataset?
# Cross-Validation error measured for one year forecasts.

# Cross-Validation with an expanding window

numcores <- detectCores()
cl <- makeCluster(numcores)
registerDoParallel(cl = cl, cores = numcores)
opts <- list(chunkSize = numcores/2)

# CV parameters
forecast_window <- 52 
training_window <- 52 # size of the initial window for expanding window cross-validation.
timestep <- 1

tb <- copy(decomp)      
tb[, ID:=.I]
IDmax <- max(tb$ID)
# Iterate over the model order
CV_error_GAM <- foreach(oTime = seq(0.1, 0.1, 0.1), .combine = rbind)%:%
  foreach(oPrice = seq(1, 5, 1), .combine = rbind)%:%
  foreach(oPrice1 = seq(1, 5, 1), .inorder = FALSE, .combine = rbind,
          .packages = c('mgcv', 'data.table','Metrics'), .errorhandling = "pass",
          .options.nws=opts)%dopar%{
            start <- 0L
            stop <- training_window
            
            prediction_error <- c()
            while (stop < IDmax){
              
              train <- tb[ID %between% c(start,stop)]
              test <- tb[ID %between% c(stop+1, stop + forecast_window)]
              
              fit <- mgcv::gam(TREND ~ s(Time, fx = F) + s(Price, fx = F) + s(Price1, fx = F), 
                               sp = c(oTime, oPrice, oPrice1), 
                               data = train)
              #        print(fit)
              prediction <- predict.gam(fit,test)
              prediction_error <- c(prediction_error,rmse(actual = test$TREND, predicted = prediction))
              #        print(paste0("start = ", start, ", stop = ", stop, ", prediction error = ", last(prediction_error)))
              stop <- stop + timestep
            }
            
            data.table(lambda_Time = oTime, lambda_Price = oPrice, lambda_Price1 = oPrice1, 
                       CV_error = mean(prediction_error))
}

fwrite(CV_error_GAM[, .(lambda_Time, lambda_Price, lambda_Price1, CV_error)], file = "data/classical_decomposition_nl_ss_CV_error.csv")

# Keep the orders that minimize the Cross-Validation error
CV_error_GAM[, min_CV_error_GAM := min(CV_error)]
error_GAM <- CV_error_GAM[CV_error == min_CV_error_GAM, .(lambda_Time, lambda_Price, 
                                                          lambda_Price1, CV_error)]
error_GAM
#lambda_Time lambda_Price   CV_error
#1:         0.1            1 0.08619843
# E. Forecast ------------------------------------------------------------------
# Make the prediction with train-test splitting (use 3 years for training and 1 year for testing) 
# using the optimal polynomial

# train-test split
test_set <- decomp[Time %between% c(t_max-51, t_max), .(Time, TREND, Price, Price1)]
train_set <- decomp[Time < t_max-51, .(Time, TREND, Price, Price1)]
t0 <- min(decomp$Time)
# model fitting
lambda_Time <- error_GAM$lambda_Time
lambda_Price <- error_GAM$lambda_Price
lambda_Price1 <- error_GAM$lambda_Price1

gam.fit <- try(mgcv::gam(TREND ~ s(Time, fx = F) + s(Price, fx = F) + s(Price1, fx = F), 
                         sp = c(lambda_Time, lambda_Price, lambda_Price1), data = train_set))

# prediction & prediction error
prediction <- predict.gam(gam.fit, test_set[, .(Time, Price, Price1)])
prediction_error <- rmse(test_set$TREND, prediction)*sd

# prediction & prediction error
# take 1 year forecast
horizon <- max(dt_In$Time)-t_max # Here the horizon of the trend forecast is larger than a year.
price1 <- dt[Time>t_max]$Price
price2 <- rep(last(price1), times = horizon - length(price1))
price11 <- dt[Time>t_max]$Price1
price12 <- rep(last(price11), times = horizon - length(price11))
forecast <- predict.gam(gam.fit, data.table(Time = t_max+seq(1:horizon), Price = c(price1,price2), 
                                            Price1 = c(price11,price12)))
dt_forecast <- data.table(Time = t_max+seq(1, horizon),TREND_FORECAST = forecast)

decomp <- merge(decomp, dt_forecast, by = c("Time"), all = T)

View(decomp)
plot(decomp[, .(Time, TREND)])

points(decomp[, .(Time, TREND_FORECAST)])
plot(dt_forecast[, .(Time, TREND_FORECAST)])
# F. Add the fitting error and the decomposition error -------------------------

reg_residuals <- residuals(gam.fit)

# To inverse standardise the residuals I just multiply with the standard deviation, 
# because the mean cancels out
# Calculate the mean square root error

TREND_Fitting_error <- sd*sqrt(sum((reg_residuals)**2)/length(reg_residuals)) 

decomp_residuals <- decomp[!is.na(REMAINDER), .(Time, REMAINDER)]

# Here the trend fitting error might be adding up to some of the residuals error
# The trend fitting error is insignificant compared to the decomposition error.
decomposition_error <- sd*sqrt(sum(decomp_residuals$REMAINDER**2)/length(decomp_residuals$REMAINDER))

CV_error <- error_GAM$CV_error*sd

# G. Add the seasonal component to the trend forecast --------------------------
decomp[, seas_Index := Time %% 52]
decomp[seas_Index == 0, seas_Index:=52]

decomp <- merge(decomp, seasonal, by = "seas_Index", all.x = T)
setorder(decomp, Time)

decomp[is.na(SEASONAL), SEASONAL:= FIGURE]

decomp[, Sales_Decomp := TREND_FORECAST + SEASONAL]

# H. Inverse standardise the forecast and decomposition ------------------------
decomp[,Sales_Decomp := transform$inv_standardize(Sales_Decomp,mean,sd)]

decomp <- merge(decomp, dt[, .(Time, Sales)], by = "Time", all = T)
# Add also the Actual forecast
decomp <- merge(decomp, dt_In[, .(Time, Actual_forecast)], all.x = T)

# I. Plot ----------------------------------------------------------------------
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
  geom_line(aes(y = Sales_Decomp, color = "Forecast"), size = 1) +
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
ggsave("figures/classical_decomposition_nl_splines.png", width = 5, height = 3)

fwrite(decomp[, .(Time, PERIOD, Sales_Decomp, Sales, Actual_forecast)], 
       "data/classical_decomposition_nl_splines.csv", append = F)
