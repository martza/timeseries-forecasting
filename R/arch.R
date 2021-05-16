
rolling_cv <- function(dt, step , f = lm , formula = formula,  train_size, 
                       test_size, param, values){
  
  labels <-labels(terms(formula))
  target <- formula[[2]]
  print(labels)
  
  # a list that holds the cross-validation errors
  cv_error <- c()
  
  # a vector that holds the fitted models
  coef <- list()
  
  for (i in values){
    # update the model formula
    terms <- str_replace(labels, param, as.character(i))
    formula <- reformulate(termlabels = terms, response = target)
    print(formula)
    coef[[i]]<- list()
    # start cross validation of the model formula
    start <- 1
    end1 <- train_size
    end2 <- end1 + test_size
    
    error <- c()
    while (end2 < nrow(dt)) {
      train <- dt[start:end1]
      test <- dt[(end1+1):end2]
      
      # fit
      fit <- f(formula = formula, data = train)
      print(summary(fit))
      # forecast
      fcast <- predict(fit, newdata = test)
      # update error
      error <- append(error, mse(test[[target]],fcast))
      
      # update step
      start <- start + step
      end1 <- end1 + step
      end2 <- end2 + step
      
      coef[[i]] <- rbind(coef[[i]], fit$coefficient)
    }
    
    # update CV error
    cv_error <- append(cv_error,sum(error)/length(error))
    
  }
  return(list('CV_error' = cv_error, 'coefs' = coef))
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
####
# Finds the Arima model that minimizes the BIC criterion.
# Sequential implementation. Equivalent to auto.arima.
####
Get_Arima_seq <- function(ts, p_values, q_values, d, s, xreg){
  
  log <- c()
  fit <- Arima(ts, order = c(0,0,0), seasonal = s, xreg = xreg)
  bic <- BIC(fit)
  
  for (p in p_values){
    for (q in q_values){
      message('Fitting for p = ', p, ', q = ', q)
      fit_temp <- Arima(ts, order = c(p,d,q), seasonal = s, xreg = xreg)
      if (BIC(fit_temp) < bic){
        fit <- fit_temp
        bic <- BIC(fit_temp)
      }
      log <- rbind(log, c(p, q, BIC(fit_temp)))
    }
  }
  
  return(list('fit' = fit, 'log' = log))
}

####
# Finds the Arima model that is minimizing the Aicc metrics (you can also change the metrics). 
# Option to run in parallel. 
# What this function does is similar to the algorithm that auto.arima implements 
# to find the Arima model. I checked that in most of the cases it finds the same model 
# orders as auto.arima. So it is better to use the auto.arima instead.
####
Get_Arima <- function(ts, p_values, q_values, d, s, xreg, clusterSize = 4, chunkSize = 4, parallel = F){
  
  # Initialization of the error keeper and log list 
  aicc <- Inf
  log <- list()  
  
  if (parallel) {
    
    # Make a cluster
    cl <- makeCluster(clusterSize)
    
    registerDoParallel(cl = cl, cores = clusterSize)
    
    # Send chunks of calls to each core for optimal running 
    opts <- list(chunkSize = chunkSize)
    
    # Parallel nested running over the values of the parameters
    fits <- foreach(q = q_values, .options.nws=opts) %:%
      foreach(p = p_values, .packages = c('forecast'), .inorder = FALSE) %dopar%{
        message('Fitting for p = ', p, ', q = ', q)
        Arima(ts, order = c(p,d,q), seasonal = s, xreg = xreg, method = 'ML')
      }
    
    # Find the fit that minimizes the error and log the error for every fit
    for (i in seq_along(fits)) {
      core_result <- fits[[i]]
      for (j in seq_along(core_result)){
        temp_fit <- core_result[[j]]
        if (temp_fit$aicc < aicc) {
          fit <- temp_fit
          aicc <- temp_fit$aicc
        }  
        log <- rbind(log, c(arimaorder(temp_fit)[['p']], arimaorder(temp_fit)[['q']], 
                            temp_fit$aicc, AIC(temp_fit), BIC(temp_fit)))
      }
    }
    
    # stop the cluster
    stopCluster(cl)
    
  }else{
    #    fit <- Arima(ts, order = c(0,0,0), seasonal = s, xreg = xreg)
    
    for (p in p_values){
      for (q in q_values){
        message('Fitting for p = ', p, ', q = ', q)
        temp_fit <- Arima(ts, order = c(p,d,q), seasonal = s, xreg = xreg, method = 'ML')
        if (temp_fit$aicc < aicc){
          fit <- temp_fit
          aicc <- temp_fit$aicc
        }
        log <- rbind(log, c(p, q, temp_fit$aicc, AIC(temp_fit), BIC(temp_fit)))
      }
    }
  }
  
  
  # return the lof and the fit
  return(list('fit' = fit, 'log' = log))
}
