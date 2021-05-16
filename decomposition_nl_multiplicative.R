source(paste0(Sys.getenv("HOME"), "/StepUp/R/Common Scripts/load.R"), local = FALSE)
SetWD()

require(caret)
require(forecast)
require(timeSeries)
require(tseries)
require(ISLR)
require(modules)

modules<- modules::use('R')
transform <- modules$help_functions
models <- modules$models
cross_validation <- modules$cross_validation

# A. User-defined parameters and data input ------------------------------------

# Choose if you want to fit the seasonal component
seasonal <- F
# Choose to keep the corona period in the training dataset or not
corona <- F # try to add the corona period as a dummy variable
# Perform Arima with regression
regression <- T

# Read data tables from the database
SqlConnect("Alpro")
dtAlproIn <- dbReadTable("Baselines_Combined")[SCOPE == "OWN"]
dictionary <- dbReadTable("enrich_TOTAL")
SqlDisconnect(con)

dtAlpro <- copy(dtAlproIn)
dict <- unique(dictionary[, .(PRODUCT = BASEFILE, SEGMENT)])

# B. Data preparation ----------------------------------------------------------

# Take the sales and price per SEGMENT, MARKET, PERIOD
dtAlpro <- dtAlpro[,.(PRODUCT, MARKET, PERIOD, Sales = Baseline, Price = Baseline_Price)]
dt <- merge(dtAlpro, dict, by = c("PRODUCT"), all.x = T)
dt[, PRODUCT:=NULL]
dt[str_detect(string = MARKET, pattern = "Delhaize"), MARKET:="Delhaize"]
dt <- dt[, lapply(.SD, sum), by = .(MARKET,PERIOD,SEGMENT), .SDcols = c("Sales", "Price")]

# Add the variables week and year to the dataset
dt <- dt[, `:=` (YEAR = as.numeric(str_extract(PERIOD, "(?<=\\s)[:digit:]*(?=\\s)")), 
                 WEEK = as.numeric(str_extract(PERIOD, "(?<=\\s)[:digit:]{2}$")))]
dt <- dt[!(WEEK == 53)] # Remove week 53


# Take the last period of the fitting dataset depending on the corona parameter
max_period <- ifelse(!corona, 'W 2020 08', max(dt$PERIOD))
max_week <- as.numeric(str_extract(max_period, "(?<=\\s)[:digit:]{2}$"))
dt <- dt[PERIOD <= max_period]

# Take the set of weeks in 2020 that are missing from the dataset
weeks <- unique(dt[WEEK>max_week]$WEEK)
markets <- unique(dt$MARKET)
segments <- unique(dt$SEGMENT)
t <- uniqueN(dt$PERIOD)

# Create an incremental year variable
dt[, YEAR := YEAR - min(YEAR)]

# Map the periods to a time index
setorder(dt, MARKET, SEGMENT, PERIOD)
dt[, Index := 1:.N, by = .(MARKET, SEGMENT)]
View(dt)

# C. Decompose the sales per market and segment -------------------------------- 
# __ 1.Preparation for decomposition -------------------------------------------
# Take the timeseries of vectors that have at least two years of sales. Decompositions 
# needs at least two years of sales.
# For multiplicative decomposition I need to work with the real values.

v <- vector(mode = 'list')
mean <- vector(mode = 'list')
sd <- vector(mode = 'list')

for (market in markets){
  for (segment in segments){
    # Get timeseries
    data <- dt[MARKET == market & SEGMENT == segment]
    # Keep the indices of the periods with non-zero sales
    Indices <- data[, .I[Sales>0]]
    Imin <- min(Indices)
    Imax <- max(Indices)
    start <- c(Imin %/% 52+1, Imin %% 52)
    stop <- c(Imax %/% 52+1, Imax %% 52)
    Indices <- seq(Imin, Imax) # make sure that there are no gaps
    
    # Take timeseries only for Segment-market combinations with at least three years of sales
    skip <- ifelse(length(Indices) < 156, TRUE, FALSE)
    
    if (!skip){
      v[[market]][[segment]] <- ts(data[Index %between% c(Imin, Imax)]$Sales, frequency = 52, start = start, end = stop)

      # Standardize
      mean[[market]][[segment]] <- mean(v[[market]][[segment]])
      sd[[market]][[segment]] <- sd(v[[market]][[segment]])    
      v[[market]][[segment]] <- transform$standardize(v[[market]][[segment]])
      
      # Remove outliers
      replace(x = v[[market]][[segment]], list = which(v[[market]][[segment]]<(-3)), values = (-3))
      replace(x = v[[market]][[segment]], list = which(v[[market]][[segment]]> 3), values = 3)
      if(sum(abs(v[[market]][[segment]])>3)!=0) {warning("You have outliers in the Sales.")}
      
      # Inverse standardize
      v[[market]][[segment]] <- transform$inv_standardize(x = v[[market]][[segment]], 
                                                          mean = mean[[market]][[segment]], 
                                                          std = sd[[market]][[segment]])
      
      
    }
  }
}


# __ 2.Get classical decomposition of the sales --------------------------------
decomp <- data.table() # A table for keeping the decomposition components
seasonal <- data.table() # A table for keeping the seasonal components
for(market in markets){
  segments <- names(v[[market]])
  for (segment in segments){
    seas_decomp_add<- decompose(v[[market]][[segment]], type = 'multiplicative')
    # Map the timeseries to the correct indexes
    start <- sum((start(v[[market]][[segment]])-c(1, 0))*c(52 , 1))
    end <- sum((end(v[[market]][[segment]])-c(1, 0))*c(52 , 1))
    
    decomp <- rbind(decomp, data.table(Index = seq(start, end), SEGMENT = segment, MARKET = market, 
                                       TREND = as.numeric(trendcycle(seas_decomp_add)), SEASONAL = as.numeric(seasonal(seas_decomp_add)), 
                                       REMAINDER = as.numeric(remainder(seas_decomp_add))))    
    seasonal <- rbind(seasonal, data.table(seas_Index = seq(1:52), SEGMENT = segment, MARKET = market, FIGURE = seas_decomp_add$figure))
  }
}

# Remove NA from TREND components
decomp <-decomp[!is.na(TREND)]

View(decomp)

# D. Apply regression to the trend component -----------------------------------
# __ 1. Take additional regressors: the price ----------------------------------
# Standardize price
# Remove outliers
# scale the price to the maximum price per SEGMENT MARKET
max_price <- vector(mode = "list")
dt[, meanPrice := mean(Price), by = .(SEGMENT,MARKET)]
dt[, sdPrice := sd(Price), by = .(SEGMENT,MARKET)]
dt[, standardPrice := (Price - meanPrice)/sdPrice]
dt[standardPrice>3, standardPrice := 3]
dt[standardPrice<(-3), standardPrice := -3]
#plot0 <- vector(mode = "list")

#for (segment in segments){
#  plot0[[segment]] <- ggplot(data = dt[SEGMENT == segment, .(Index,SEGMENT,MARKET,standardPrice)] , aes(x = Index, color = MARKET))+
#    geom_point(aes(y = standardPrice))
#}
#transform$multiplot(plot0)

decomp <- merge(decomp, dt[, .(SEGMENT, MARKET, Index, standardPrice)], by = c("SEGMENT", "MARKET", "Index"), all.x = T)
# fwrite(decomp,"decomposition.csv")

t1 <- max(decomp$Index)
# __ 2. Perform cross validation -----------------------------------------------
#
# For each market-segment take the trend and apply regression.
# Model: It is a linear model, but I need to explore non-linear models.
# Cross-validation : 
# How does the linear regression model generalize for prediction over different regions of the dataset?
# Cross-Validation error measured for one year forecasts.

# Cross-Validation with an expanding window
CV_error <- data.table()
for (n in seq(1, 5)){
  for(m in seq(1, 5)){
    for (market in markets){
      segments <- names(v[[market]])
      for (segment in segments){
        CV_error <- rbind(CV_error, list(MARKET = market, SEGMENT = segment, n = n, m = m,
                                   expanding_CV_error = cross_validation$nlm_CV(dt = decomp[MARKET == market & SEGMENT == segment],
                                                                      forecast_window = NULL, 
                                                                      training_window = 52,
                                                                      timestep = 4,
                                                                      formula = TREND ~ poly(Index, m) + poly(standardPrice, n),
                                                                      method = 'expanding')))
      }
    }
  }
}
setorder(CV_error,MARKET,SEGMENT,n, m)
View(CV_error)

# Cross-Validation with an expanding window
rolling_error <- data.table()
for (market in markets){
  segments <- names(v[[market]])
  for (segment in segments){
    for (n in seq(1, 5)){
      for(m in seq(1, 5)){      
        rolling_error <- rbind(rolling_error, list(MARKET = market, SEGMENT = segment, n = n, m = m,
                                   rolling_CV_error = cross_validation$nlm_CV(dt = decomp[MARKET == market & SEGMENT == segment],
                                                                      forecast_window = 52, 
                                                                      training_window = 52,
                                                                      timestep = 4,
                                                                      formula = TREND ~ poly(Index, m) + poly(standardPrice, n),
                                                                      method = 'rolling')))      }
    }
  }
}
View(rolling_error)
CV_error <- merge(CV_error, rolling_error, by = c("MARKET", "SEGMENT", "n", "m"), all.x = T)

# Keep the polynomial orders that minimize the Cross-Validation error
CV_error[, min_expanding_CV_error := min(expanding_CV_error), by = .(MARKET, SEGMENT)]
error <- CV_error[expanding_CV_error == min_expanding_CV_error, .(MARKET, SEGMENT, n, m, expanding_CV_error)]
View(error)

# E. Forecast ------------------------------------------------------------------
# Make the prediction with train-test splitting (use 3 years for training and 1 year for testing) 
# using the optimal polynomial

lm.fit <- vector(mode = "list")
dt_forecast <- data.table()

for(market in markets){
  segments <- names(v[[market]])
  for (segment in segments){
    # train-test split
    test_set <- decomp[MARKET == market & SEGMENT == segment & Index %between% c(t1-51, t1), .(Index, TREND, standardPrice)]
    train_set <- decomp[MARKET == market & SEGMENT == segment & Index < t1-51, .(Index, TREND, standardPrice)]
    t0 <- min(train_set$Index)
    # model fitting
    O_Index <- error[SEGMENT == segment & MARKET == market]$m
    O_Price <- error[SEGMENT == segment & MARKET == market]$n
    lm.fit[[market]][[segment]] <- lm(TREND ~ poly(Index, O_Index) + poly(standardPrice, O_Price), data = train_set)
    # prediction & prediction error
    prediction <- predict(lm.fit[[market]][[segment]], test_set[, .(Index, standardPrice)])
    prediction_error <- rmse(test_set$TREND, prediction)
    error[MARKET == market & SEGMENT == segment, prediction_Error := prediction_error]
    
    # take 1 year forecast
    futurePrice <- test_set[Index == t1]$standardPrice
    forecast <- predict(lm.fit[[market]][[segment]], data.table(Index = t1+seq(1:52), standardPrice = rep(futurePrice, 52)))
    dt_forecast <- rbind(dt_forecast, data.table(Index = seq(t0, t1+52),MARKET = market, SEGMENT = segment,
                                                 TREND_FORECAST = c(lm.fit[[market]][[segment]]$fitted.values, prediction, forecast)), 
                                                 fill = T)
  }
}

decomp <- merge(decomp, dt_forecast[, !"standardPrice"], by = c("Index", "MARKET", "SEGMENT"), all = T)

setorder(decomp, SEGMENT, MARKET, Index)
View(decomp)
plot(decomp[MARKET == market & SEGMENT == segment, .(Index, TREND)])

points(decomp[MARKET == market & SEGMENT == market, .(Index, TREND_FORECAST)])
plot(dt_forecast[MARKET == market & SEGMENT == segment, .(Index, TREND_FORECAST)])
# F. Add the fitting error and the decomposition error -------------------------

reg_residuals <- vector(mode = "list")
for(market in markets){reg_residuals[[market]] <- lapply(X = lm.fit[[market]], FUN = residuals)}
stopifnot(sum(reg_residuals[[1]][[1]]-residuals(lm.fit[[1]][[1]]))==0)

for(market in markets){
  segments <- names(v[[market]])
  for(segment in segments){
    # Calculate the mean square root error
    error[MARKET == market & SEGMENT == segment, TREND_Fitting_error:=sqrt(sum((reg_residuals[[market]][[segment]])**2)/length(reg_residuals[[market]][[segment]]))] 
    decomp_residuals <- decomp[MARKET == market & SEGMENT == segment & !is.na(REMAINDER)]$REMAINDER
    error[MARKET == market & SEGMENT == segment, Decomposition_error:=sqrt(sum(decomp_residuals**2)/length(decomp_residuals))] 
  }
}

# G. Add the seasonal component to the trend forecast --------------------------
decomp[, seas_Index := Index %% 52]
decomp[seas_Index == 0, seas_Index:=52]

decomp <- merge(decomp, seasonal, by = c("MARKET", "SEGMENT", "seas_Index"), all.x = T)
setorder(decomp, SEGMENT, MARKET, Index)

View(decomp)
decomp[is.na(SEASONAL), SEASONAL:= FIGURE]

decomp[, Sales_Decomp := TREND_FORECAST * SEASONAL]

# H. Include the original sales ------------------------

decomp <- merge(decomp, dt[, .(MARKET,SEGMENT,Index, Sales)], by = c("MARKET", "SEGMENT", "Index"), all.x = T)
View(decomp)

# I. Plot ----------------------------------------------------------------------
# Get a table with weekly periods
periods <- unique(dt[, .(Index, PERIOD, YEAR, WEEK)])
min_week <- periods[Index == 1]$WEEK
new_Index <- c(max(periods$Index)+(1:52))
new_weeks <- (new_Index+min_week-1)%%52
new_weeks <- replace(new_weeks, which(new_weeks == 0), values = 52)

new_years <- (new_Index+min_week-2)%/%52
new_periods <- str_c("W", 2015 + new_years, formatC(new_weeks, flag = "0", width = 2), sep = " ")
periods <- rbind(periods, data.table(Index = new_Index, PERIOD = new_periods, YEAR = new_years, WEEK = new_weeks))
View(periods)

plot <- vector(mode = "list")

for (segment in segments) {
  plot[[segment]] <- ggplot(data = decomp[SEGMENT == segment], aes(x = Index)) + 
    geom_point(aes(y = Sales, shape = MARKET), size = 1)+
    geom_line(aes(y = Sales_Decomp, color = MARKET), size = 1)+
    scale_x_continuous(breaks = decomp[seq(1,decomp[SEGMENT == segment, .N]/uniqueN(decomp[SEGMENT == segment]$MARKET),10)]$Index, 
                       labels = periods[Index %in% decomp[seq(1,decomp[SEGMENT == segment, .N]/uniqueN(decomp[SEGMENT == segment]$MARKET),10)]$Index]$PERIOD, 
                       guide = guide_axis(angle = 90))+
    xlab("PERIOD")+
    theme_bw()+    
    ggtitle(segment)
}
transform$multiplot(plot)


ggplot(data = decomp[SEGMENT == "CREAMS"], aes(x = Index)) + 
  geom_point(aes(y = Sales, shape = MARKET), size = 1)+
  geom_line(aes(y = Sales_Decomp, color = MARKET), size = 1)+
  ggtitle(segment)+
  scale_x_continuous(breaks = decomp[seq(1,decomp[SEGMENT == "CREAMS", .N]/uniqueN(decomp[SEGMENT == "CREAMS"]$MARKET),10)]$Index, 
                     labels = periods[Index %in% decomp[seq(1,decomp[SEGMENT == "CREAMS", .N]/uniqueN(decomp[SEGMENT == "CREAMS"]$MARKET),10)]$Index]$PERIOD, 
                     guide = guide_axis(angle = 90))+
  xlab("PERIOD")+
  theme_bw()

unique(decomp$Index)
