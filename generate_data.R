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


# Functions --------------------------------------------------------------------
seasonality <- function(frequency, tmax = 52, hw_max = 4, tmin = 28, hw_min = -4, tc = 52, te = 14, hw_c = 1, hw_e = 3){
  t <- seq(1, frequency)
  # winter raise
  f1 <- hw_max/(pi*((t-tmax)^2+hw_max^2))+hw_max/(pi*((t-(tmax-52))^2+hw_max^2))
  # summer drop
  f2 <- hw_min/(pi*((t-tmin)^2+hw_min^2))
  # christmas raise
  f3 <- hw_c/(pi*((t-tc)^2 +hw_c^2))+hw_c/(pi*((t-(tc-52))^2+hw_c^2))
  # easter raise - around April
  f4 <- hw_e/(pi*((t-te)^2+hw_e^2))
  
  # Normalization
  f <- f1+5*f2+0.5*f3+0.1*f4
  c <- sum(f)
  
  # Final function - centered around one (-c/52) and normalized to 1
  f <- 1+f-c/52
}

price <- function(t, tmax = 150){
  
  # Initial price per unit volume
  p0 <- 1
  # Price increase/decrease (linear part) - here assume an increase of 5% in a year
  p1 <- ((1.05-1)/52)*t
  # maximum: use a parabola to flatten around the maximum
  p2 <- -0.01*(t-tmax)^2/1000
  # plateau: use a sigmoid to switch off the price to a plateau
  p3 <- exp(-0.01*(t-tmax))/(exp(-0.01*(t-tmax))+1)
  
  # Take the final price
  p <- p0 + (p1 + p2)* p3
  
  # Normalization
  c <- sum(p)/length(t)
  p <- p/c
  
}

# Data creation ----------------------------------------------------------------
# Make a seven years dataset (six for modeling and one for forecast)
t <- seq(1, 7*52)
# Create the seasonal component
seas <-  seasonality(frequency = 52, hw_max = 6, hw_min = -8, hw_c = 0.5)
plot(seas)
# Take the price
price <- price(t)
plot(price)
# Take the trend
sales <- 1000 + 10 * t + 0.5* t^2*exp(-0.01*(t-250))/(exp(-0.01*(t-250))+1)
c <- sum(sales)/(1000*length(t)) 
sales <- sales / c # normalize
plot(sales)

# Add the price effect to the sales
sales_w_price <- sales+600/price
c <- sum(sales_w_price)/(1000*length(t))
sales_w_price <- sales_w_price/c # Normalize
points(sales_w_price)
# Add the seasonal effect to the sales
sales_w_seas <- sales_w_price * seas
c <- sum(sales_w_seas)/(1000*length(t))
sales_w_seas <- sales_w_seas/c
points(sales_w_seas)
# Add noise
sales_w_noise <- sales_w_seas +rnorm(n = length(sales_w_seas), mean = 0, sd = 50)
lines(sales_w_noise)

# Store all values to a table
dt <- data.table(Time = t, Price = price, sales = sales, sales_w_price = sales_w_price, 
                 sales_w_seas = sales_w_seas, sales_w_noise = sales_w_noise)
# Keep the last year as the actual forecast
dt[Time %in% (max(dt$Time) - seq(0, 51)), Actual_forecast := sales_w_noise]
dt[!is.na(Actual_forecast), sales_w_noise:=NA]

# Add period information -------------------------------------------------------
dt[, WEEK := Time %% 52]
dt[WEEK == 0, WEEK := 52]

dt[, YEAR := (Time-1) %/% 52]

dt[, PERIOD := str_c("W", 2015 + YEAR, formatC(WEEK, flag = "0", width = 2), sep = " ")]

dt[, WEEK := str_extract(PERIOD, pattern = "[:digit:]{2}$")]
dt[, YEAR := str_extract(PERIOD, pattern = "[:digit:]{4}")]
min_YEAR <- min(dt$YEAR)
dt[, YEAR:= as.numeric(YEAR)%%as.numeric(min_YEAR)+1]
dt[, PERIOD:=paste("Y", YEAR, "W", WEEK, sep = " ")]

# Plots ------------------------------------------------------------------------
# Exclude from the plots the actual forecast
price_plot <- ggplot(data = dt[!is.na(sales_w_noise)], mapping = aes(x = Time))+
  geom_line(mapping = aes(y = Price), color = "blue", size = 0.5) +
  scale_x_continuous(breaks = dt[seq(1, max(dt$Time), 10)]$Time, 
                   labels = dt[seq(1, max(dt$Time), 10)]$PERIOD, 
                   guide = guide_axis(angle = 90)) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = -15)))+
  theme_bw()
ggsave("../Documentation/figures/price.png", width = 5, height = 3)

price_plot

colors <- c("Including price effect" = "blue", "Starting trend" = "red", "Including seasonal effect" = "black")
sales_plot <- ggplot(data = dt[!is.na(sales_w_noise)], mapping = aes(x = Time))+
  geom_line(mapping = aes(y = sales, color = "Starting trend"), size = 0.5) +
  geom_line(mapping = aes(y = sales_w_price, color = "Including price effect"), size = 0.5) +
  geom_point(mapping = aes(y = sales_w_seas, color = "Including seasonal effect"), size = 0.5) +
  geom_line(mapping = aes(y = sales_w_noise, color = "Including seasonal effect"), size = 0.5) +
  scale_x_continuous(breaks = dt[seq(1, max(dt$Time), 10)]$Time, 
                     labels = dt[seq(1, max(dt$Time), 10)]$PERIOD, 
                     guide = guide_axis(angle = 90)) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = -15)))+
  theme_bw()+
  scale_color_manual(values = colors, breaks = c("Starting trend", "Including price effect" , "Including seasonal effect"))+
  labs(y = "Sales", color = NULL)+
  theme(legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6), 
        legend.text = element_text(size = 8))
  
sales_plot
ggsave("../Documentation/figures/sales_model.png", width = 5, height = 3)

# Save the dataset
fwrite(x = dt, file = "model_sales.csv")

# additional plot

dt <- fread("../Data/model_sales.csv")
sales_plot <- ggplot(data = dt, mapping = aes(x = Time))+
  geom_line(mapping = aes(y = sales_w_noise), size = 0.5, color = "black") +
  scale_x_continuous(breaks = dt[seq(1, max(dt$Time), 10)]$Time, 
                     labels = dt[seq(1, max(dt$Time), 10)]$Time, 
                     guide = guide_axis(angle = 90)) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = -15)))+
  theme_bw()+
#  xlim(0,400)+
  labs(y = "Observable", color = NULL)+
  theme(legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6), 
        legend.text = element_text(size = 8))

sales_plot
ggsave("../Presentation/figures/timeseries3.png", width = 5, height = 3)
