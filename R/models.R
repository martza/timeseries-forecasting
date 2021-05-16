import('timeSeries')
import('tseries')
import('forecast')
import('stats')
import('foreach')
import('doParallel')
import('parallel')
import('data.table')

#export('Get_d', 'Get_params', 'Get_params_CV', 'Get_Arima', 'Get_Arima_seq', 'tsCV', 'autoarima_model')

####
# Detect the order of differencing testing the stationarity of the data. First 
# detect the order of the seasonal data. Then if needed take the differences and
# detect the order of differencing of the non-seasonal data.
####
Get_d <- function(ts, freq, method = 'kpss', seasonal = F){
  if (seasonal) {
    ds <- nsdiffs(ts)
    if (ds > 0) ts <- diff(ts, lag = freq)
  }else{ ds <- 0}
  d <- ndiffs(ts)
  return(list('d' = d, 'ds' =  ds))
}

####
# Detect the possible values of the ARIMA orders using ACF and PACF. 
####
Get_params <- function(ts, d, ds, freq, seasonal = F){
  
  # Take the stationary part of the timeseries
  i <- 1
  while (i <= ds) {
      ts <- diff(ts, lag = freq)
      i <- i + 1
    }
  i <- 1
  while (i <= d) {
    ts <- diff(ts)
    i <- i + 1
  }
  
  # find p and ps, q and qs
  # From the stationary part of the timeseries determine the Arima parameters.
  # Take q from the significant spikes in acf
  # Take p from the significant spikes in pacf
  
  acf_ts <- Acf(ts)
  
  pacf_ts <- Pacf(ts)
  
  lim <- 1.96/sqrt(length(ts))
  
  # For lag <= t/4 take the lag values where correlation is larger than the critical value.
  
  # non-seasonal parameters
  q_values <- acf_ts$lag[(abs(acf_ts$acf)>lim) & (acf_ts$lag<5)]
  if (length(q_values)>5) q_values <- q_values[1:5]
  
  p_values <- pacf_ts$lag[(abs(pacf_ts$acf)>lim) & (pacf_ts$lag<5)]
  if (length(p_values)>5) p_values <- p_values[1:5]
  
  # seasonal parameters
  ps <- 0L
  qs <- 0L
  if (seasonal) qs <- ifelse(abs(acf_ts$acf[acf_ts$lag == freq]) > lim, 1L, 0L)
  
  if (seasonal) ps <- ifelse(abs(pacf_ts$acf[pacf_ts$lag == freq]) > lim, 1L, 0L)
  
  
  return(list('p_values' = p_values, 'q_values' = q_values, 'ps' = ps, 'qs' = qs))
}



# Finds the Arima model that minimizes the cross-validation error. 
Get_params_CV <- function(ts, d, ds, seasonal = T, qmax = 1, pmax = 1, psmax = 1,
                          qsmax = 1, xreg = NULL, clusterSize = 4, chunkSize = 4, 
                          forecastfunction, window = NULL, h_CV = 52, 
                          initial = 104, step = 10, parallel = T){
  
  if (parallel) {
    # Make a cluster
    cl <- makeCluster(clusterSize)
    
    registerDoParallel(cl = cl, cores = clusterSize)
    
    opts <- list(chunkSize = chunkSize)
    
    # Parallel nested running over the values of the parameters
    log <- foreach(q = 0:qmax, .combine = rbind) %:% foreach(p = 0:pmax, .combine = rbind) %:% 
      foreach(qs = 0:qsmax, .combine = rbind) %:% 
      foreach(ps = 0:psmax, .inorder = FALSE, .errorhandling = 'remove', .combine = rbind, 
              .packages = 'forecast', .export = 'tsCV', .options.nws=opts, .verbose = T)%dopar%{
                
                temp <- tsCV(y = ts, forecastfunction = forecastfunction, h = h_CV, 
                             initial = initial, window = window, step = step, 
                             xreg = xreg, order = c(p,d,q), s_order = c(ps, ds, qs), 
                             parallel = F)
                
                row_sums <- rowSums(temp^2, na.rm = TRUE)
                row_counts <- rowSums(!is.na(temp^2), na.rm = TRUE) # count the logical elements that are non-zero
                CV_error <- ifelse(row_counts>0, sqrt(row_sums/row_counts), 0)
                
                CV_error <- CV_error[CV_error>0]
                
                
                
                c('p' = p, 'q' = q, 'ps' = ps, 'qs' = qs, 'CV_error' = mean(CV_error))
                
              }
    
    stopCluster(cl)
  }else{
    # Parallel nested running over the values of the parameters
    log <- foreach(q = 0:qmax, .combine = rbind) %:% foreach(p = 0:pmax, .combine = rbind) %:% 
      foreach(qs = 0:qsmax, .combine = rbind) %:% 
      foreach(ps = 0:psmax, .inorder = FALSE, .errorhandling = 'remove', .combine = rbind)%do%{
                
                temp <- tsCV(y = ts, forecastfunction = forecastfunction, h = h_CV, 
                             initial = initial, window = window, step = step, 
                             xreg = xreg, order = c(p,d,q), s_order = c(ps, ds, qs), 
                             parallel = T, clusterSize = clusterSize)
                
                row_sums <- rowSums(temp^2, na.rm = TRUE)
                row_counts <- rowSums(!is.na(temp^2), na.rm = TRUE) # count the logical elements that are non-zero
                CV_error <- ifelse(row_counts>0, sqrt(row_sums/row_counts), 0)
                
                CV_error <- CV_error[CV_error>0]
                
                c('p' = p, 'q' = q, 'ps' = ps, 'qs' = qs, 'CV_error' = mean(CV_error))
                
              }
    
  }
  

  log <- data.table(log)
  CV_order <- log[CV_error == min(log$CV_error)]
  
  # return the log and the fit
  return(list('CV_order' = CV_order,'log' = log))
}

# Forecast with order selection using an information criterion ----
# Parameters:
# ts : a list of time series objects with yearly seasonality and dimensions market, segment, product
# (markets, segments, objects) : 1d lists with the names of the attributes that you want to model
# h : The forecasting horizon. It also defines the forecasting horizon and the size of the initial window 
# in the cross-validation.
# xreg : a matrix of additional predictors to include in the ARIMA
# parallel : logical variable to activate parallel running of the auto.arima and CV
# ic : if needed, one can use a different information criterion. See auto.arima for options.
# CV_step : The step to expand the training window in cross-validation
# forecastfunction : A function that returns a forecast

autoarima_model <- function(ts, l1, l2, l3, h, xreg, parallel = T, 
                            num_cores = detectCores(), forecastfunction, ic = "aicc", CV_step, ...){
  
  ss_arima_forecast <- foreach(i1 = l1, .combine = rbind)%:%foreach(i2 = l2, .combine = rbind)%:%
    foreach(i3 = l3, .inorder = FALSE, .combine = rbind, 
            .packages = c('data.table', 'Metrics', 'forecast'), 
            .errorhandling = "remove", 
            .verbose = T)%do%{
              
              # train-test split
              end <- length(ts[[i1]][[i2]][[i3]])
              nreg <- ncol(xreg[[i1]][[i2]][[i3]])
              test_set <- subset(ts[[i1]][[i2]][[i3]], start = end - h + 1, end = end)
              train_set <- subset(ts[[i1]][[i2]][[i3]], start = 1, end = end - h)
              start_time <- start(train_set)
              start_time <- (start_time[1]-1)*52+start_time[2]
              
              xreg_test <- xreg[[i1]][[i2]][[i3]][(end - h + 1):end, 1:2]
              xreg_train <- xreg[[i1]][[i2]][[i3]][1:(end - h), 1:2]
              
              # Take the order parameters on the full set and get the order of the model
              ss_arima_fit<- auto.arima(y = ts[[i1]][[i2]][[i3]], seasonal = T,
                                        xreg = xreg[[i1]][[i2]][[i3]],
                                        stepwise = FALSE, approximation = TRUE, 
                                        parallel = parallel, num.cores = num_cores, 
                                        ic = ic, allowdrift = F)
              model_order <- arimaorder(ss_arima_fit)
              order <- c(model_order[["p"]], model_order[["d"]], model_order[["q"]])
              if(length(model_order)<6){
                seasonal <- FALSE
                sorder <- c(0,0,0)
              }else{
                seasonal <- TRUE
                sorder <- c(model_order[["P"]], model_order[["D"]], model_order[["Q"]])
              }
              # Cross-validation error
              # Cross-Validation for one year horizon
              # Each row of the matrix corresponds to the error of a step in the Cross-Validation
              # Take the RMSE for each cross-validation step. The cross-validation error
              # corresponds to the average over all steps.
              if (seasonal){
                CV <- tsCV(y = ts[[i1]][[i2]][[i3]],
                           forecastfunction = forecastfunction, h = h,
                           initial = h, xreg = xreg[[i1]][[i2]][[i3]],
                           step = CV_step, parallel = parallel, clusterSize = num_cores,
                           order = order, sorder = sorder)
              }else{
                CV <- forecast:tsCV(y = ts[[i1]][[i2]][[i3]], forecastfunction = forecastfunction,
                           h = h, initial = h, xreg = xreg[[i1]][[i2]][[i3]],
                           order = order, sorder = sorder)
              }
              
              CV_error <- get_tsCV_error(CV = CV)
              
              # training and testing errors
              # train the model on the train set and take the regression coefficients
              ss_arima_model<- try(Arima(y = train_set, xreg = xreg_train, order = order, 
                                         seasonal = sorder))
              if(class(ss_arima_model)=="try-error"){
                ss_arima_model<- Arima(y = train_set, xreg = xreg_train, order = order, 
                                       seasonal = sorder, method = "CSS")
              }
              ss_arima_residuals <- residuals(ss_arima_model)
              
              # prediction & prediction error referring to the regression coefficients
              arima_predict <- forecast(ss_arima_model, h = h, xreg = xreg_test)
              prediction_error <- rmse(test_set, arima_predict$mean)
              
              
              # Use the trained model for 1 year forecast
              arima_fit_final <- Arima(y = ts[[i1]][[i2]][[i3]], 
                                       xreg = xreg[[i1]][[i2]][[i3]],
                                       model = ss_arima_model)
              
              forecast <- forecast(arima_fit_final, h = h, 
                                   xreg = matrix(rep(tail(xreg[[i1]][[i2]][[i3]], 1), 
                                                     rep(h, times = nreg)), ncol = nreg))
              
              # start_time: the actual time when the ts starts, it can be different than 1 if there are leading zeros
              # (1,end) is the actual length of the timeseries
              
              data.table(Time = (start_time-1) + seq(1, end + h),v1 = i1, 
                         v2 = i2, v3 = i3, 
                         SS_ARIMA_FORECAST = c(arima_fit_final$fitted, forecast$mean),
                         arima_fitting_error = sqrt(mean(ss_arima_residuals**2)),  
                         arima_prediction_error = prediction_error, 
                         arima_CV_error = CV_error, SEASONAL = seasonal)
            }
  ss_arima_forecast
}

# Forecast with order selection using an cross-validation ----
# Parameters:
# ts : a list of time series objects with yearly seasonality and dimensions market, segment, product
# (markets, segments, objects) : 1d lists with the names of the attributes that you want to model
# h : The forecasting horizon. It also defines the forecasting horizon and the size of the initial window 
# in the cross-validation.
# xreg : a matrix of additional predictors to include in the ARIMA
# parallel : logical variable to activate parallel running of the auto.arima and CV.
# CV_step : The step to expand the training window in cross-validation
# forecastfunction : A function that returns a forecast

CV_arima_model <- function(ts, l1, l2, l3, h, xreg, parallel = F,forecastfunction, CV_step, ...){
  
  CV_arima_forecast <- foreach(i1 = l1, .combine = rbind)%:%foreach(i2 = l2, .combine = rbind)%:%
    foreach(i3 = l3, .inorder = FALSE, .combine = rbind, 
            .packages = c('data.table', 'Metrics', 'forecast'), .errorhandling = "remove",
            .options.nws=opts, .verbose = T)%do%{
              

              # Get the model order using ARIMA with cross-validation
              
              d_values <- Get_d(ts = ts[[i1]][[i2]][[i3]], freq = 52, seasonal = T)
              
              res <- Get_params_CV(ts = ts[[i1]][[i2]][[i3]], d = d_values$d,
                                   ds = d_values$ds, seasonal = T,
                                   xreg = xreg[[i1]][[i2]][[i3]],
                                   parallel = parallel, forecastfunction = forecastfunction,
                                   h_CV = h, initial = h, qmax = 2, pmax = 2, 
                                   psmax = 1, qsmax = 1, step = CV_step)
              
              
              setorder(res$log, 'CV_error')
              
              order <- c(res$CV_order$p, d_values$d, res$CV_order$q)
              s_order <- c(res$CV_order$ps, d_values$ds, res$CV_order$qs) 
              CV_error <- res$CV_order$CV_error
              
              # Take the model parameters using the full dataset, but keeping the model order from CV
              final_fit <- try(Arima(y = ts[[i1]][[i2]][[i3]], order = order, 
                                     seasonal = s_order, xreg = xreg[[i1]][[i2]][[i3]],
                                     method = 'ML'), silent = T)
              # If the ARIMA gives an error, then take the model order with the smaller CV
              # error that gives no error 
              irow <- 2
              while (class(final_fit) == 'try-error') {
                log <- res$log[irow]
                CV_error <- log$CV_error
                order <- c(log$p, d_values$d, log$q)
                s_order <- c(log$ps, d_values$ds, log$qs)
                final_fit <- try(Arima(y = ts[[i1]][[i2]][[i3]], order = order, 
                                       seasonal = s_order, xreg = xreg[[i1]][[i2]][[i3]], 
                                       method = 'ML'), silent = T)
                irow <- irow + 1
              } 
              
              # train-test split mainly for the calculation of training and prediction errors
              end <- length(ts[[i1]][[i2]][[i3]])
              nreg <- ncol(xreg[[i1]][[i2]][[i3]])
              test_set <- subset(ts[[i1]][[i2]][[i3]], start = end - h + 1, end = end)
              train_set <- subset(ts[[i1]][[i2]][[i3]], start = 1, end = end - h)
              start_time <- start(train_set)
              start_time <- (start_time[1]-1)*52+start_time[2]
              
              xreg_test <- xreg[[i1]][[i2]][[i3]][(end - h + 1):end, 1:2]
              xreg_train <- xreg[[i1]][[i2]][[i3]][1:(end - h), 1:2]
              
              # train the model on the train set using the order from cross-validation
              CV_arima_fit<- Arima(y = train_set, method = 'ML', 
                                   xreg = xreg_train, order = order, 
                                   seasonal = s_order)
              CV_arima_residuals <- residuals(CV_arima_fit)
              
              # prediction & prediction error
              CV_arima_predict <- forecast(CV_arima_fit,h = h, xreg = xreg_test)
              prediction_error <- rmse(test_set, CV_arima_predict$mean)
              
              # Get the actual forecast from the model fitted on the full data set
              # We do this to take advantage of the whole data set but we introduce some bias.
              # One can use the model trained in the training set and get a two years forecast, 
              # one for the prediction error and one with the actual forecast.
              
              forecast <- forecast(final_fit, h = h, 
                                   xreg = matrix(rep(tail(xreg[[i1]][[i2]][[i3]], 1), 
                                                     rep(h, times = nreg)), ncol = nreg))
              
              # start_time: the actual time when the ts starts, it can be different than 1 
              # if there are leading zeros
              # (1,end) is the actual length of the timeseries
              
              data.table(Time = (start_time-1) + seq(1, end + h), v1 = i1, 
                         v2 = i2, v3 = i3, CV_ARIMA_FORECAST = c(final_fit$fitted, forecast$mean), 
                         arima_fitting_error = sqrt(mean(CV_arima_residuals**2)), 
                         arima_prediction_error = prediction_error, arima_CV_error = CV_error)
              
            }
  CV_arima_forecast
}

# Fit a polynomial model, get the fitting and prediction errors
# We cheat by adding to the training set the last value of observations 
# Here price is used as an additional predictor, but one can add more.

polynomials_model <- function(dt, oPrice, oTime, tmax, h){
  # train-test split
  test_set <- dt[Time %between% c(tmax-h+1, tmax-1), .(Time, TREND, Price)]
  train_set <- dt[(Time < tmax-h+1 | Time == tmax), .(Time, TREND, Price)]
  t0 <- min(train_set$Time)
  
  if(o_Price<0){
    lm.fit <- lm(TREND ~ poly(Time, o_Time) + poly(1/Price, -o_Price), data = train_set)
  }else{
    if(o_Price == 0){
      lm.fit <- lm(TREND ~ poly(Time, o_Time), data = train_set)
    }else{
      lm.fit <- lm(TREND ~ poly(Time, o_Time) + poly(Price, o_Price), data = train_set)
    }
  }
  
  fitting_residuals <- residuals(lm.fit)
  trend_fitting_error <- sqrt(mean(fitting_residuals^2))
  
  # prediction & prediction error
  prediction <- predict(lm.fit, test_set[, .(Time, Price)])
  prediction_error <- rmse(test_set$TREND, prediction)
  
  # take 1 year forecast
  futurePrice <- train_set[Time == tmax]$Price
  forecast <- predict(lm.fit, data.table(Time = tmax+seq(1:h),Price = rep(futurePrice, h)))
  
  list(TREND_FITTING_ERROR = trend_fitting_error, PREDICTION_ERROR = prediction_error, forecast = forecast)
}

ss_model <- function (dt, tmax, lTime, lPrice, lPrice1, h){
  
  test_set <- dt[Time %between% c(tmax-h+1, tmax-1), .(Time, TREND, Price, Price1)]
  train_set <- dt[Time < tmax-h+1 | Time == tmax, .(Time, TREND, Price, Price1)]
  t0 <- min(train_set$Time)
  
  ss.fit <- try(mgcv::gam(TREND ~ s(Time, fx = F) + s(Price, fx = F) + s(Price1, fx = F),
                           sp = c(lTime, lPrice, lPrice1), data = train_set))
  if (class(ss.fit) == 'try-error'){
    forecast <- rep(NA, h)
    trend_fitting_error <- NA
    prediction_error <- NA
  }else{
    fitting_residuals <- residuals(ss.fit)
    trend_fitting_error <- sqrt(mean(fitting_residuals^2))
    
    # prediction & prediction error
    prediction <- predict.gam(ss.fit, test_set[, .(Time, Price, Price1)])
    prediction_error <- rmse(test_set$TREND, prediction)
    
    # take 1 year forecast
    futurePrice <- train_set[Time == tmax]$Price
    futurePrice1 <- train_set[Time == tmax]$Price1
    forecast <- predict.gam(ss.fit, data.table(Time = tmax+seq(1:h),Price = rep(futurePrice, h),
                                               Price1 = rep(futurePrice1, h)))
  }
  
  list(TREND_FITTING_ERROR = trend_fitting_error, PREDICTION_ERROR = prediction_error, forecast = forecast)
  
}