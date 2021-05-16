setwd("C:/Users/Marilena/Documents/StepUp/R/baselines_forecast/Benchmark")
require(caret)
require(forecast)
require(timeSeries)
require(tseries)
require(ISLR)
require(modules)

modules<- modules::use('../R')
transform <- modules$help_functions
models <- modules$models
cross_validation <- modules$cross_validation

# A. User-defined parameters and data input ------------------------------------

dt_In <- fread('../Data/model_sales.csv')
dt <- dt_In[, .(Time, YEAR, WEEK, PERIOD, Price, Sales = sales_w_noise)]
dt <- dt[!is.na(Sales)]
names(dt)

# Standardize the Sales
mean <- mean(dt$Sales)
sd <- sd(dt$Sales)
dt[, Sales := transform$standardize(Sales)]

# B. Model evaluation and forecast ---------------------------------------------
# Calculate the model cross-validation. Start from an initial set of two years 
# and use an expanding window. 
# Find the model parameters from a train set (the size is 4 years)
# Calculate the prediction error on a test set (the size is 1 year)
# Forecast with the model one year ahead

# train-test split
t_max <- max(dt$Time)
test_set <- dt[Time %in% seq(t_max-51, t_max)]
train_set <- dt[Time <= t_max - 52]

# Make a linear fit of the sales across the years. Use this fit for making a prediction
new_YEAR <- max(dt$YEAR)+1
lm.fit <- vector(mode = "list")
CV_error <- vector(mode = "list")

for (w in c(1:52)){
  lm.fit[[w]] <- lm(Sales~YEAR, data = train_set[WEEK == w, .(YEAR, Sales)])
  prediction <- predict(lm.fit[[w]], test_set[WEEK == w, .(YEAR)])
  # Keep the prediction in the test set
  test_set[WEEK == w, Prediction := prediction]
  # Cross-Validation on the full dataset for one-year forecasts
  CV_error[[w]] <- cross_validation$nlm_CV(dt = dt[WEEK == w, .(YEAR, Sales)], 
                                        training_window = 2, forecast_window = 1, 
                                        timestep = 1, formula = Sales~YEAR, method = "expanding", target = "Sales")
  # Forecast
  forecast <- predict(lm.fit[[w]], data.table(YEAR = new_YEAR))
  dt <- rbind(dt, data.table(YEAR = new_YEAR, WEEK = w, Forecast = forecast), fill = T)
}

# Inverse standardise the Sales and Forecast
dt[, Sales := transform$inv_standardize(Sales,mean,sd)]
dt[, Forecast := transform$inv_standardize(Forecast,mean,sd)]

# Add period information to the prediction part of the data table
dt[!is.na(Forecast), `:=`(Time = YEAR*52+WEEK, 
                            PERIOD = str_c("W", 2015 + YEAR, formatC(WEEK, flag = "0", width = 2), sep = " "))]

# Add the actual forecast
dt <- merge(dt, dt_In[, .(Time, Actual_forecast)], by = "Time", all.x = T)
View(dt)

# C. Model errors ---------------------------------------------
# Prediction error
# Cross-validation error
# Model fitting error
# To inverse standardise the residuals I just multiply with the standard deviation, 
# because the mean cancels out

# prediction error
prediction_error <- rmse(test_set$Sales, test_set$Prediction)
prediction_error <- prediction_error*sd

# cross-validation error
CV_error <- mean(simplify2array(CV_error))
CV_error <- CV_error * sd

# fitting error
lm_residuals <- lapply(lm.fit, residuals)
# Calculate the mean square root error for each week of the year
fitting_errors <- sapply(lm_residuals, function(x) {sqrt((1/length(x))*sum(x^2))})
# Calculate the fitting error as a mean over the weeks in the year
mean_fitting_error <- mean(fitting_errors)*sd

# D.Plot -----------------------------------------------------------------------
dt[, WEEK := str_extract(PERIOD, pattern = "[:digit:]{2}$")]
dt[, YEAR := str_extract(PERIOD, pattern = "[:digit:]{4}")]
min_YEAR <- min(dt$YEAR)
dt[, YEAR:= as.numeric(YEAR)%%as.numeric(min_YEAR)+1]
dt[, PERIOD:=paste("Y", YEAR, "W", WEEK, sep = " ")]

colors <- c("Weekly model" = "red", "Actual" = "black")
plot <- ggplot(data = dt, aes(x = Time)) +
  geom_point(aes(y = Sales, color = "Actual"), size = 0.5)+
  geom_line(aes(y = Forecast, color = "Weekly model"), size = 0.5) +
  geom_line(aes(y = Actual_forecast, color = "Actual"), size = 0.5)+
  scale_x_continuous(breaks = dt[seq(1, max(dt$Time), 10)]$Time, 
                     labels = dt[seq(1, max(dt$Time), 10)]$PERIOD, 
                     guide = guide_axis(angle = 90)) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = -15)))+
  theme_bw()+
  scale_color_manual(values = colors, breaks = c("Actual", "Weekly model"))+
  labs(y = "Sales", color = NULL)+
  theme(legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6), 
        legend.text = element_text(size = 8))
plot
ggsave("../Documentation/figures/linear_model.png", width = 5, height = 3)
