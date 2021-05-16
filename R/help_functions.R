import('stats')
import('forecast')
import('stringr')
import('Metrics')

#export('standardize', 'inv_standardize', 'get_trend', 'rolling_cv', 'multiplot')

standardize <- function(x){
  y <- (x - mean(x[which(x>0)]))/sd(x[which(x>0)])
  y
}

inv_standardize <- function(x, mean, std){
  y <- x*std + mean
  y
}


# Get the trend using STL decomposition with the option to remove outliers.
# Returns a timeseries object. Transform to numeric to use outside of timeseries.
get_trend <- function(x, method = 'stl', clean = T, window = 52){
  if (method == 'stl'){
    y <- ts(x, deltat = 1/52)
    if (clean) y <- tsclean(y)
    series <- stl(y, s.window = 52)
  }
  trend <- trendcycle(series)
  trend
}

get_seasonal <- function(x, method = 'stl'){
  if (method == 'stl'){
    y <- ts(x, deltat = 1/52)
    series <- stl(y, s.window = 52)
  }
  trend <- trendcycle(series)
  trend
}

