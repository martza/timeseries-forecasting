setwd("C:/Users/Marilena/Documents/StepUp/R/baselines_forecast/Benchmark")
require(modules)
require(stringr)
require(data.table)
require(ggplot2)

modules<- modules::use('../R')
transform <- modules$help_functions
models <- modules$models
cross_validation <- modules$cross_validation

# A. User-defined parameters and data input ------------------------------------

dt <- fread('data/model_sales.csv')
cl_decomp_l <- fread('data/classical_decomposition.csv')
cl_decomp_nl <- fread('data/classical_decomposition_nl.csv')
cl_decomp_nl_ss <- fread('data/classical_decomposition_nl_splines.csv')
STL_models <- fread('data/STL_decomposition_nl.csv')
hw <- fread('data/hw_forecast.csv')
cl_decomp_l
cl_decomp_nl
cl_decomp_nl_ss

cl_decomp <- merge(cl_decomp_l[, .(Time, PERIOD, cl_Decomp_l = Sales_Decomp, Sales, Actual_forecast)], 
                   cl_decomp_nl[, .(Time, cl_Decomp_nl = Sales_Decomp)], by = "Time", all.x = T)
cl_decomp <- merge(cl_decomp, cl_decomp_nl_ss[, .(Time, cl_Decomp_nl_ss = Sales_Decomp)], by = "Time", all.x = T)
cl_decomp
STL_models
hw_stl <- merge(STL_models[,.(Time, PERIOD, STL_ETS_FORECAST, Sales, Actual_forecast)], 
                hw[, .(Time,HW_FORECAST)], by = "Time", all.x = T)
arima_price <- fread('data/arima.csv')
arima <- fread('data/arima_no_price.csv')
arima
arima <- merge(arima, arima_price[, .(Time, SS_ARIMA_FORECAST_PRICE = SS_ARIMA_FORECAST)], by = "Time", all.x = T)

# Plot STL decomposition -------------------------------------------------
STL_models[, WEEK := str_extract(PERIOD, pattern = "[:digit:]{2}$")]
STL_models[, YEAR := str_extract(PERIOD, pattern = "[:digit:]{4}")]
min_YEAR <- min(STL_models$YEAR)
STL_models[, YEAR:= as.numeric(YEAR)%%as.numeric(min_YEAR)+1]
STL_models[, PERIOD:=paste("Y", YEAR, "W", WEEK, sep = " ")]

colors <- c("STL - NLR" = "red", 
            "STL & naive" = "green4", 
            "STL & drift" = "purple", "STL & ETS" = "blue4", 
            "Actual" = "black")
plot <- ggplot(data = STL_models, aes(x = Time)) +
  geom_point(aes(y = Sales, color = "Actual"), size = 0.5)+
  geom_line(aes(y = Sales_FORECAST, color = "STL - NLR"), size = 0.5) +
  geom_line(aes(y = STL_NAIVE_FORECAST, color = "STL & naive"), size = 0.5) +
  geom_line(aes(y = STL_RW_DRIFT_FORECAST, color = "STL & drift"), size = 0.5) +
  geom_line(aes(y = STL_ETS_FORECAST, color = "STL & ETS"), size = 0.5) +
  geom_line(aes(y = Actual_forecast, color = "Actual"), size = 0.5)+
  scale_x_continuous(breaks = STL_models[seq(1, max(STL_models$Time), 10)]$Time, 
                     labels = STL_models[seq(1, max(STL_models$Time), 10)]$PERIOD, 
                     guide = guide_axis(angle = 90)) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = -15)))+
  theme_bw()+
  scale_color_manual(values = colors, breaks = c("Actual", "STL - NLR", 
                                                 "STL & naive", 
                                                 "STL & drift", "STL & ETS"))+
  labs(y = "Sales", color = NULL)+
  theme(legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(1, 1, 1, 1), 
        legend.text = element_text(size = 7))
plot

ggsave("../Documentation/figures/stl_decomposition.png", width = 5, height = 3)

# Plot classical decomposition -------------------------------------------------
cl_decomp[, WEEK := str_extract(PERIOD, pattern = "[:digit:]{2}$")]
cl_decomp[, YEAR := str_extract(PERIOD, pattern = "[:digit:]{4}")]
min_YEAR <- min(cl_decomp$YEAR)
cl_decomp[, YEAR:= as.numeric(YEAR)%%as.numeric(min_YEAR)+1]
cl_decomp[, PERIOD:=paste("Y", YEAR, "W", WEEK, sep = " ")]

colors <- c("Trend forecast LR" = "red", "Trend forecast NLR" = "blue", "Trend forecast Ss" = "orange4", "Actual" = "black")
plot <- ggplot(data = cl_decomp, aes(x = Time)) +
  geom_point(aes(y = Sales, color = "Actual"), size = 0.5)+
  geom_line(aes(y = cl_Decomp_l, color = "Trend forecast LR"), size = 0.5) +
  geom_line(aes(y = cl_Decomp_nl, color = "Trend forecast NLR"), size = 0.5) +
  geom_line(aes(y = cl_Decomp_nl_ss, color = "Trend forecast Ss"), size = 0.5) +
  geom_line(aes(y = Actual_forecast, color = "Actual"), size = 0.5)+
  scale_x_continuous(breaks = cl_decomp[seq(1, max(cl_decomp$Time), 10)]$Time, 
                     labels = cl_decomp[seq(1, max(cl_decomp$Time), 10)]$PERIOD, 
                     guide = guide_axis(angle = 90)) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = -15)))+
  theme_bw()+
  scale_color_manual(values = colors, breaks = c("Actual", "Trend forecast LR", "Trend forecast NLR", "Trend forecast Ss"))+
  labs(y = "Sales", color = NULL)+
  theme(legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6), 
        legend.text = element_text(size = 7))
plot
ggsave("../Documentation/figures/cl_decomposition.png", width = 5, height = 3)

# Plot comparison HW with STL naive --------------------------------------------
hw_stl[, WEEK := str_extract(PERIOD, pattern = "[:digit:]{2}$")]
hw_stl[, YEAR := str_extract(PERIOD, pattern = "[:digit:]{4}")]
min_YEAR <- min(hw_stl$YEAR)
hw_stl[, YEAR:= as.numeric(YEAR)%%as.numeric(min_YEAR)+1]
hw_stl[, PERIOD:=paste("Y", YEAR, "W", WEEK, sep = " ")]


colors <- c("Holt-Winters" = "green4",
            "STL & ETS" = "blue4",
            "Actual" = "black")
plot <- ggplot(data = hw_stl, aes(x = Time)) +
  geom_point(aes(y = Sales, color = "Actual"), size = 0.5)+
  geom_line(aes(y = HW_FORECAST, color = "Holt-Winters"), size = 0.5) +
  geom_line(aes(y = STL_ETS_FORECAST, color = "STL & ETS"), size = 0.5) +
  geom_line(aes(y = Actual_forecast, color = "Actual"), size = 0.5)+
  scale_x_continuous(breaks = hw_stl[seq(1, max(hw_stl$Time), 10)]$Time, 
                     labels = hw_stl[seq(1, max(hw_stl$Time), 10)]$PERIOD, 
                     guide = guide_axis(angle = 90)) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = -15)))+
  theme_bw()+
  scale_color_manual(values = colors, breaks = c("Actual", "Holt-Winters", "STL & ETS"))+
  labs(y = "Sales", color = NULL)+
  theme(legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6), 
        legend.text = element_text(size = 7))
plot
ggsave("../Documentation/figures/hw_stl.png", width = 5, height = 3)

# Plot comparison arima --------------------------------------------
hw_stl[, WEEK := str_extract(PERIOD, pattern = "[:digit:]{2}$")]
hw_stl[, YEAR := str_extract(PERIOD, pattern = "[:digit:]{4}")]
min_YEAR <- min(hw_stl$YEAR)
hw_stl[, YEAR:= as.numeric(YEAR)%%as.numeric(min_YEAR)+1]
hw_stl[, PERIOD:=paste("Y", YEAR, "W", WEEK, sep = " ")]


colors <- c("ARIMA from AIC" = "red", 
            "ARIMA from CV" = "blue", 
            "ARIMA from AIC with price" = "orange4",
            "Actual" = "black")
plot <- ggplot(data = arima, aes(x = Time)) +
  geom_point(aes(y = Sales, color = "Actual"), size = 0.5)+
  geom_line(aes(y = Actual_forecast, color = "Actual"), size = 0.5)+
  geom_line(aes(y = SS_ARIMA_FORECAST, color = "ARIMA from AIC"), size = 0.5) +
  geom_line(aes(y = CV_ARIMA_FORECAST, color = "ARIMA from CV"), size = 0.5)+
  geom_line(aes(y = SS_ARIMA_FORECAST_PRICE, color = "ARIMA from AIC with price"), size = 0.5)+
  scale_x_continuous(breaks = arima[seq(1, nrow(arima), 10)]$Time, 
                     labels = arima[seq(1, nrow(arima), 10)]$PERIOD, 
                     guide = guide_axis(angle = 90)) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=10), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = -15)))+
  theme_bw()+
  scale_color_manual(values = colors, breaks = c("Actual", "ARIMA from AIC", 
                                                 "ARIMA from CV", "ARIMA from AIC with price"))+
  labs(y = "Sales", color = NULL)+
  theme(legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6), 
        legend.text = element_text(size = 8))
plot
ggsave("../Documentation/figures/arima.png", width = 5, height = 3)

# ACF PACF plots

v <- ts(dt[!is.na(sales_w_noise)]$sales_w_noise, frequency = 52)
v1 <- diff(v, lag = 52)

png("../Documentation/figures/ts.png", width = 5, height = 3, units = "in", res = 300)
ts.plot(v1, ylab = "Sales", xlab = "Year")
dev.off()

v2 <- diff(v1)

png("../Documentation/figures/ts1.png", width = 5, height = 3, units = "in", res = 300)
ts.plot(v2, ylab = "Sales", xlab = "Year")
dev.off()


acf_plot <- ggAcf(v2)+ggtitle("")
ggsave(plot = acf_plot, filename =  "../Documentation/figures/ACF.png", width = 5, height = 3)

pacf_plot <- ggPacf(v2)+ggtitle("")
ggsave(plot = pacf_plot, filename = "../Documentation/figures/PACF.png", width = 5, height = 3)

# Plot the order of the CV_error as a function of the order of the polynomials
CV_error_nl <- fread('Data/classical_decomposition_nl_CV_error.csv')
CV_error_ss <- fread('Data/classical_decomposition_nl_ss_CV_error.csv')
plot_CV_nl <- rbind(CV_error_nl[Time == 2, .(order = Price, expanding_CV_error, predictor = "Price")], 
                    CV_error_nl[Price == -1, .(order = Time, expanding_CV_error, predictor = "Time")])
colors <- c("Price" = "blue", "Time" = "red")
plot <- ggplot(plot_CV_nl, aes(x = order, color = predictor))+
  geom_line(aes(y = expanding_CV_error), size = 1)+
  scale_x_continuous(breaks = seq(-5,5, by = 1)) +
  theme_bw()+
  scale_color_manual(values = colors, breaks = c("Time", "Price"))+
  labs(y = "error", color = NULL)+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6), 
        legend.text = element_text(size = 12))
plot
ggsave("../Documentation/figures/CV_error_nl.png", width = 5, height = 3)

plot_CV_ss <- rbind(CV_error_ss[lambda_Time == 0.1, .(order = lambda_Price, CV_error, predictor = "Price")], 
                    CV_error_ss[lambda_Price == 3, .(order = lambda_Time, CV_error, predictor = "Time")])
colors <- c("Price" = "blue", "Time" = "red")
plot <- ggplot(CV_error_ss[lambda_Time == 0.1], aes(x = lambda_Price))+
  geom_line(aes(y = CV_error), color = "blue", size = 1)+
  theme_bw()+
  labs(y = "error", x = "smoothing", color = NULL)+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6), 
        legend.text = element_text(size = 12))
plot
ggsave("../Documentation/figures/CV_error_ss_price.png", width = 5, height = 3)

plot <- ggplot(CV_error_ss[lambda_Price == 3], aes(x = lambda_Time))+
  geom_line(aes(y = CV_error), color = "red", size = 1)+
  theme_bw()+
  labs(y = "error", x = "smoothing", color = NULL)+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15),
        legend.position = c(.01, .99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        legend.margin = margin(6, 6, 6, 6), 
        legend.text = element_text(size = 12))
plot
ggsave("../Documentation/figures/CV_error_ss_time.png", width = 5, height = 3)

