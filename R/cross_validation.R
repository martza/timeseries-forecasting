import('stats')
import('Metrics')
import('data.table')
import('mgcv')

# Cross-Validation of linear regression, with rolling and expanding windows ----
# The input is a data table
# training_window : The size of the training set
# forecast_window : The size of the test set. If NULL, then test on the full remaining dataset.
# timestep : The step for rolling/expanding the window
# method : rolling/expanding
# returns the cross-validation error as rmse

lm_CV <- function(dt, forecast_window = 52, training_window = 52, timestep = 10, method = 'rolling'){

  dt[, ID:=.I]
  IDmax <- max(dt$ID)
  min_forecast_window <- 52
  
  start <- 0L
  stop <- training_window
  
  if (is.null(forecast_window)) {forecast_window <- IDmax - training_window}
  
  if (method == "rolling"){
    prediction_error <- c()
    while (stop <= IDmax - forecast_window){
      
      train <- dt[ID %between% c(start,stop)]
      test <- dt[ID %between% c(stop+1, stop + forecast_window)]
      
      fit <- lm(TREND~Index + scaledPrice, data = train)
      prediction <- predict(fit,test)
      prediction_error <- c(prediction_error,rmse(actual = test$TREND, predicted = prediction))
      print(paste0("start = ", start, ", stop = ", stop, ", prediction error = ", last(prediction_error)))
      
      start <- start + timestep
      stop <- stop + timestep
    }
  }else{
    if (method == "expanding"){
      prediction_error <- c()
      while (stop <= IDmax - min_forecast_window){
        
        train <- dt[ID %between% c(start,stop)]
        test <- dt[ID %between% c(stop+1, stop + forecast_window)]
        
        fit <- lm(TREND~Index + scaledPrice, data = train)
        prediction <- predict(fit,test)
        prediction_error <- c(prediction_error,rmse(actual = test$TREND, predicted = prediction))
        print(paste0("start = ", start, ", stop = ", stop, ", prediction error = ", last(prediction_error)))
        
        stop <- stop + timestep
      }
    }
  }
  CV_error <- mean(prediction_error)
  CV_error
}


# Cross-Validation of poynomial regression, with rolling and expanding windows ----
# The input is a data table
# training_window : The size of the training set
# forecast_window : The size of the test set. If NULL, then test on the full remaining dataset.
# timestep : The step for rolling/expanding the window
# method : rolling/expanding
# returns the cross-validation error as rmse

nlm_CV <- function(dt, forecast_window = 52, training_window = 52, timestep = 10, formula = formula, method = 'rolling', target = "TREND"){
  
  dt[, ID:=.I]
  IDmax <- max(dt$ID)
  
  start <- 0L
  stop <- training_window
  
  if (is.null(forecast_window)) {forecast_window <- IDmax - training_window}
  
  if (method == "rolling"){
    prediction_error <- c()
    while (stop < IDmax){
      
      train <- dt[ID %between% c(start,stop)]
      test <- dt[ID %between% c(stop+1, stop + forecast_window)]
      
      fit <- lm(formula, data = train)
      prediction <- predict(fit,test)
      prediction_error <- c(prediction_error,rmse(actual = test[, get(target)], predicted = prediction))
      
      start <- start + timestep
      stop <- stop + timestep
    }
  }else{
    if (method == "expanding"){
      prediction_error <- c()
      while (stop < IDmax){
        
        train <- dt[ID %between% c(start,stop)]
        test <- dt[ID %between% c(stop+1, stop + forecast_window)]
        
        fit <- lm(formula, data = train)
#        print(fit)
        prediction <- predict(fit,test)
        prediction_error <- c(prediction_error,rmse(actual = test[, get(target)], predicted = prediction))
#        print(paste0("start = ", start, ", stop = ", stop, ", prediction error = ", last(prediction_error)))
        
        stop <- stop + timestep
      }
    }
  }
  CV_error <- mean(prediction_error)
  CV_error
}

# Adjust the timeseries cross-validation with an option to run in parallel and a variable step
# Adaptation of forecast::tsCV
tsCV <- function(y, forecastfunction, h=1, window=NULL, xreg=NULL, initial=0, 
                 step = 1L, parallel = T, clusterSize, ...) {
  y <- as.ts(y)
  n <- length(y)
  e <- ts(matrix(NA_real_, nrow = n, ncol = h))
  if(initial >= n) stop("initial period too long")
  tsp(e) <- tsp(y)
  if (!is.null(xreg)) {
    # Make xreg a ts object to allow easy subsetting later
    xreg <- ts(as.matrix(xreg))
    if(NROW(xreg) != length(y))
      stop("xreg must be of the same size as y")
    tsp(xreg) <- tsp(y)
  }
  if (is.null(window))
    indx <- seq(1+initial, n - 1L, by = step)
  else
    indx <- seq(window+initial, n - 1L, by = step)
  
  if (parallel){
    cl <- makeCluster(clusterSize)
    
    registerDoParallel(cl = cl, cores = clusterSize)
    
    e <- foreach(i = indx, .combine = rbind, .packages = 'forecast', .errorhandling = 'remove') %dopar% {
      y_subset <- subset(
        y,
        start = ifelse(is.null(window), 1L,
                       ifelse(i - window >= 0L, i - window + 1L, stop("small window"))
        ),
        end = i
      )
      if (is.null(xreg)) {
        fc <- try(suppressWarnings(
          forecastfunction(y_subset, h = h, ...)
        ), silent = TRUE)
      }
      else {
        xreg_subset <- as.matrix(subset(
          xreg,
          start = ifelse(is.null(window), 1L,
                         ifelse(i - window >= 0L, i - window + 1L, stop("small window")))
        ))
        fc <- forecastfunction(y_subset, h = h, xreg = xreg_subset, ...)
      }
      if (!is.element("try-error", class(fc))) {
        y[i + (1:h)] - fc$mean
      }
    }
    stopCluster(cl)
  }else{
    e <- foreach(i = indx, .combine = rbind, .packages = 'forecast', .errorhandling = 'remove') %do% {
      y_subset <- subset(
        y,
        start = ifelse(is.null(window), 1L,
                       ifelse(i - window >= 0L, i - window + 1L, stop("small window"))
        ),
        end = i
      )
      if (is.null(xreg)) {
        fc <- try(suppressWarnings(
          forecastfunction(y_subset, h = h, ...)
        ), silent = TRUE)
      }
      else {
        xreg_subset <- as.matrix(subset(
          xreg,
          start = ifelse(is.null(window), 1L,
                         ifelse(i - window >= 0L, i - window + 1L, stop("small window")))
        ))
        fc <- forecastfunction(y_subset, h = h, xreg = xreg_subset, ...)
      }
      if (!is.element("try-error", class(fc))) {
        y[i + (1:h)] - fc$mean
      }
    }
  }
  if (h == 1) {
    return(e[, 1L])
  } else {
    colnames(e) <- paste("h=", 1:h, sep = "")
    return(e)
  }
}

# Get the cross-validation error from a tsCV object ----------
# Each row of the matrix corresponds to the error of a step in the Cross-Validation
# Take the RMSE for each cross-validation step. The cross-validation error
# corresponds to the average over all steps.

get_tsCV_error <- function(CV){
  lengths <- rowSums(!is.na(CV))
  lengths <- lengths[which(lengths>0)]
  ssumsRows <- rowSums(CV^2, na.rm = TRUE)
  ssumsRows <- ssumsRows[which(ssumsRows>0)]
  errors <- sqrt(ssumsRows/lengths)
  CV_error <- mean(errors)
  CV_error
}

# Selection of the model order of polynomials with cross-validation
# It runs through time, price and 1/price and calculates the cross-validation error using an
# expanding window for the training set.
# Parameters:
# dt: a table with the columns time and price. Other predictors can be added but the function will need modification.
# (min_time,max_time) and (min_price,max_price): pairs of minimum, maximum values for the orders of the polynomials.
# negative values for the price correspond to orders of 1/price
# training_window : The size of the training set
# forecast_window : The size of the test set. 
# timestep : The step for expanding the window

get_polynomial_CV <- function(dt, min_time = 1, max_time = 5, min_price = -5, max_price = 5, 
                              forecast_window = 52, training_window = 52, timestep = 1, 
                              numcores = detectCores()){
  cl <- makeCluster(numcores)
  registerDoParallel(cl = cl, cores = numcores)
  opts <- list(chunkSize = numcores/2)
  
  dt[, ID:=.I]
  IDmax <- max(dt$ID)
  # Iterate over the model order
  new_CV_error <- foreach(oTime = min_time:max_time, .combine = rbind)%:%
    foreach(oPrice = min_price:max_price, .inorder = FALSE, .combine = rbind, .packages = c('data.table', 'Metrics'), 
            .options.nws=opts, .verbose = T)%dopar%{
              print(oPrice,oTime)    
              start <- 0L
              stop <- training_window
              
              prediction_error <- c()
              while (stop < IDmax){
                
                train <- dt[ID %between% c(start,stop)]
                test <- dt[ID %between% c(stop+1, stop + forecast_window)]
                
                max_oPrice <- uniqueN(round(train$Price, digits = 2))
                # make sure that the order of the polynomial is smaller than the number of unique observations of the regressors
                if (abs(oPrice) < max_oPrice) {
                  if(oPrice < 0){
                    fit <- lm(TREND ~ poly(Time, oTime) + poly(1/Price, -oPrice), data = train)
                  }else{
                    if(oPrice==0){
                      fit <- lm(TREND ~ poly(Time, oTime), data = train)
                    }else{
                      fit <- lm(TREND ~ poly(Time, oTime) + poly(Price, oPrice), data = train)
                    }
                  }
                  prediction <- predict(fit,test)
                  prediction_error <- c(prediction_error,rmse(actual = test$TREND, predicted = prediction))
                }
                stop <- stop + timestep
              }
              
              data.table(order_Time = oTime, order_Price = oPrice, CV_error = mean(prediction_error))
            }
  stopCluster(cl)
  min_CV_error <- min(new_CV_error$CV_error)
  new_CV_error <- new_CV_error[CV_error == min_CV_error, .(order_Time, order_Price, CV_error)]
  new_CV_error
}

# Selection of the model order of smoothing splines with cross-validation
# It runs through values of the smoothing parameters fort time, price and 1/price and calculates 
# the cross-validation error using an expanding window for the training set.
# Parameters:
# dt: a table with the columns time price and price1 = 1/price. 
# Other predictors can be added but the function will need modification.
# (min_time,max_time,step_time) and (min_price,max_price,step_price): pairs of minimum, 
# maximum values and the increment for the values of the smoothing parameter. price1 is the inverse price.
# training_window : The size of the training set
# forecast_window : The size of the test set. 
# timestep : The step for expanding the window
get_ss_CV <- function(dt, min_time = 0.1, max_time = 0.5, step_time = 0.1,
                      min_price = 0.1, max_price = 1, step_price = 0.1, 
                      min_price1 = 0.1, max_price1 = 1, step_price1 = 0.1,
                      forecast_window = 52, training_window = 52, timestep = 1, 
                      numcores = detectCores()){

  cl <- makeCluster(numcores)
  registerDoParallel(cl = cl, cores = numcores)
  opts <- list(chunkSize = numcores/2)
  
  dt[, ID:=.I]
  IDmax <- max(dt$ID)
  # Iterate over the model order
  new_CV_error_ss <- foreach(oTime = seq(min_time, max_time, step_time), .combine = rbind)%:%
    foreach(oPrice = seq(min_price, max_price, step_price), .combine = rbind)%:%
    foreach(oPrice1 = seq(min_price1, max_price1, step_price1), .inorder = FALSE, .combine = rbind, 
            .packages = c('mgcv', 'data.table','Metrics'), .errorhandling = "remove",
            .options.nws=opts, .verbose = T)%dopar%{
              start <- 0L
              stop <- training_window
              
              prediction_error <- c()
              while (stop < IDmax){
                
                train <- dt[ID %between% c(start,stop)]
                test <- dt[ID %between% c(stop+1, stop + forecast_window)]
                
                fit <- mgcv::gam(TREND ~ s(Time, fx = F) + s(Price, fx = F) + s(Price1, fx = F), 
                                 sp = c(oTime, oPrice, oPrice1), data = train)
                prediction <- predict.gam(fit,test)
                prediction_error <- c(prediction_error,rmse(actual = test$TREND, predicted = prediction))
                stop <- stop + timestep
              }
              
              data.table(lambda_Time = oTime, lambda_Price = oPrice, lambda_Price1 = oPrice1,
                         CV_error = mean(prediction_error))
            }
  stopCluster(cl)

  if (!is.null(new_CV_error_ss)){
    min_CV_error <- min(new_CV_error_ss$CV_error)
    new_CV_error_ss <- new_CV_error_ss[CV_error == min_CV_error, 
                                       .(CV_error, lambda_Time, lambda_Price, lambda_Price1)]
  }
  new_CV_error_ss
}
  