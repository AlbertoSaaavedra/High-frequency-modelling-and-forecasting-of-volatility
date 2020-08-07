
# Libraries

require(lubridate)
require(data.table)
require(e1071)
require(Hmisc)
require(zoo)


# Functions ---------------------------------------------------------------

# Rolling window and daily roughness indexes 

roughness_index_estim <- function(vol_ts,bandwidth, cut_alpha = TRUE){
  vrgm <- c()
  # Conputing variogram to estimate roughness index
  for (k in 1:bandwidth) {
    vrgm[k] <- mean(abs(Lag(vol_ts,shift = k) -vol_ts)^2,na.rm = TRUE)
  }
  # Data frame for linear model to compute roughness index
  rfsv_df <- data.frame(lag = 1:bandwidth,vrgm)
  # Linear model to compute roughness index
  rfsv_ols_lm <- lm(formula = log(vrgm) ~ log(lag),data = rfsv_df)
  # Extracting coefficients of linear model
  rfsv_ols_est <- coef(rfsv_ols_lm)
  # Estimating roughness index
  alpha_rough <- (rfsv_ols_est[2]-1)/2
  if (cut_alpha) {
    # Ensuring roughness index falls within its theoretical bounds
    alpha_rough <- ifelse(test = alpha_rough <= -0.5, yes = -0.4999, no = alpha_rough)
  }
  return(alpha_rough)
}


# Computing daily roughness estimates

daily_roughness_index <- function(vol_ts, obs_in_day, bw = 6){
  # Number of days over which alpha will be computed
  N_days <- length(vol_ts)/obs_in_day
  # Data frames and vector to store estimates of alpha
  Alpha_Index <- c()
  # Progress bar to monitor time until all forecasts are computed
  pb = txtProgressBar(min = 0, max = N_days, initial = 0)
  i <- 0
  for (j in 1:N_days) {
    i <- i+1
    # Start of the window. Each window has the number of observations in a day
    start_w <- (obs_in_day*(j-1)+1)
    # End of the window, we move the window the number of observations in a day
    end_w <- (obs_in_day*j)
    current_ts_section <- vol_ts[start_w:end_w]
    # Computing rough index for specified bandwidth. We pass the current section of the squared
    # volatility time series
    Alpha_Index[j] <-  roughness_index_estim(vol_ts = log(current_ts_section), 
                                             bandwidth = 6, cut_alpha = FALSE)
    setTxtProgressBar(pb,i)
  }
  return(Alpha_Index)
}


# Rolling window for alpha

roughness_index_rolling_window <- function(vol_ts, w_length = 10, obs_in_day, bw = 6){
  # Number of days over which alpha will be computed
  N_days <- length(vol_ts)/obs_in_day - w_length + 1
  # Data frames and vector to store estimates of alpha
  Alpha_Index <- c()
  # Progress bar to monitor time until all forecasts are computed
  pb = txtProgressBar(min = 0, max = N_days, initial = 0)
  i <- 0
  for (j in 1:N_days) {
    i <- i+1
    # Start of the window. The lenght of the window is given in an integer number of days
    start_w <- (obs_in_day*(j-1)+1)
    # We move the window 1 day at the time 
    end_w <- (w_length*obs_in_day + (j-1)*obs_in_day)
    current_ts_section <- vol_ts[start_w:end_w]
    # Computing rough index for specified bandwidth. We pass the current section of the squared
    # volatility time series
    Alpha_Index[j] <-  roughness_index_estim(vol_ts = log(current_ts_section), bandwidth = bw)
    setTxtProgressBar(pb,i)
  }
  return(Alpha_Index)
}


# Applying functions ------------------------------------------------------

# Daily roughness index estimates

daily_alpha_eurusd <- daily_roughness_index(vol_ts = Quarter_Hour_BV_1min_eurusd_fcst$std_bv_tml,
                                            obs_in_day = 84, bw = 6)
daily_alpha_gbpusd <- daily_roughness_index(vol_ts = Quarter_Hour_BV_1min_gbpusd_fcst$std_bv_tml,
                                            obs_in_day = 84, bw = 6)
daily_alpha_usdcad <- daily_roughness_index(vol_ts = Quarter_Hour_BV_1min_usdcad_fcst$std_bv_tml,
                                            obs_in_day = 84, bw = 6)
daily_alpha_usdjpy <- daily_roughness_index(vol_ts = Quarter_Hour_BV_1min_usdjpy_fcst$std_bv_tml,
                                            obs_in_day = 84, bw = 6)


# Rolling window for roughness index 

roll_alpha_eurusd <- roughness_index_rolling_window(vol_ts = Quarter_Hour_BV_1min_eurusd_fcst$std_bv_tml, 
                                                    w_length = 10, obs_in_day = 84, bw = 6)
roll_alpha_gbpusd <- roughness_index_rolling_window(vol_ts = Quarter_Hour_BV_1min_gbpusd_fcst$std_bv_tml, 
                                                    w_length = 10, obs_in_day = 84, bw = 6)
roll_alpha_usdcad <- roughness_index_rolling_window(vol_ts = Quarter_Hour_BV_1min_usdcad_fcst$std_bv_tml, 
                                                    w_length = 10, obs_in_day = 84, bw = 6)
roll_alpha_usdjpy <- roughness_index_rolling_window(vol_ts = Quarter_Hour_BV_1min_usdjpy_fcst$std_bv_tml, 
                                                    w_length = 10, obs_in_day = 84, bw = 6)

# Plotting results --------------------------------------------------------

setwd("C:/Users/Alberto Saavedra/OneDrive/Documentos/MSc in Statistics/Research project/Programing/Roughness parameter/Periodicity estimates")

load("per_eurusd2.Rout")
load("per_gbpusd2.Rout")
load("per_usdcad2.Rout")
load("per_usdjpy2.Rout")


# Plotting periodicity estimates

col_eurusd <- "#1B998B"
col_gbpusd <- "#D1495B"
col_usdcad <- "#296EB4"
col_usdjpy <- "#848C8E"

dates_ax <- seq(from = strptime(x = "23/07/2019 00:15:00", format = "%d/%m/%Y %H:%M:%S", tz = "GMT"),
                to = strptime(x = "23/07/2019 21:00:00", format = "%d/%m/%Y %H:%M:%S", tz = "GMT"), 
                by = "15 mins")

par(mfrow=c(2,2),oma = c(0, 0, 2, 0), mai = c(0.7, 0.65, 0.3, 0.1), mar=c(4.1, 4.1, 3.1, 1.1))

plot(dates_ax, per_eurusd$`from_5/2018_to_4/2019`$freq_15_mins$fi_ols, type = "l", lwd=2, col= col_eurusd, 
     ylab = expression("FFF estimate of intraday seasonality"), xlab = expression("Time of the day (UTC)"),
     main = expression("EUR/USD"))

plot(dates_ax, per_gbpusd$`from_5/2018_to_4/2019`$freq_15_mins$fi_ols, type = "l", lwd=2, col= col_gbpusd,
     ylab = expression("FFF estimate of intraday seasonality"), xlab = expression("Time of the day (UTC)"),
     main = expression("GBP/USD"))

plot(dates_ax, per_usdcad$`from_5/2018_to_4/2019`$freq_15_mins$fi_ols, type = "l", lwd=2, col= col_usdcad,
     ylab = expression("FFF estimate of intraday seasonality"), xlab = expression("Time of the day (UTC)"),
     main = expression("USD/CAD"))

plot(dates_ax, per_usdjpy$`from_5/2018_to_4/2019`$freq_15_mins$fi_ols, type = "l", lwd=2, col= col_usdjpy,
     ylab = expression("FFF estimate of intraday seasonality"), xlab = expression("Time of the day (UTC)"),
     main = expression("USD/JPY"))

mtext(expression(paste("FFF Intraday Seasonality Estimates: OLS",sep = "")), outer = TRUE, cex = 1.5)

plot(dates_ax, per_eurusd$`from_5/2018_to_4/2019`$freq_15_mins$fi_tml, type = "l", lwd=2, col= col_eurusd, 
     ylab = expression("FFF estimate of intraday seasonality"), xlab = expression("Time of the day (UTC)"),
     main = expression("EUR/USD"))

plot(dates_ax, per_gbpusd$`from_5/2018_to_4/2019`$freq_15_mins$fi_tml, type = "l", lwd=2, col= col_gbpusd,
     ylab = expression("FFF estimate of intraday seasonality"), xlab = expression("Time of the day (UTC)"),
     main = expression("GBP/USD"))

plot(dates_ax, per_usdcad$`from_5/2018_to_4/2019`$freq_15_mins$fi_tml, type = "l", lwd=2, col= col_usdcad,
     ylab = expression("FFF estimate of intraday seasonality"), xlab = expression("Time of the day (UTC)"),
     main = expression("USD/CAD"))

plot(dates_ax, per_usdjpy$`from_5/2018_to_4/2019`$freq_15_mins$fi_tml, type = "l", lwd=2, col= col_usdjpy,
     ylab = expression("FFF estimate of intraday seasonality"), xlab = expression("Time of the day (UTC)"),
     main = expression("USD/JPY"))

mtext(expression(paste("FFF Intraday Seasonality Estimates: TML",sep = "")), outer = TRUE, cex = 1.5)

# Plotting daily roughness indexes 

par(mfrow=c(2,2),oma = c(0, 0, 2, 0), mai = c(0.7, 0.65, 0.3, 0.1), mar=c(4.1, 4.1, 3.1, 1.1))
plot(unique(as.Date(Quarter_Hour_BV_1min_eurusd_fcst$date.by_delta)),
     daily_alpha_eurusd, type = "p",pch = 16,cex = 0.6, col = col_eurusd,
     ylab = expression(alpha), xlab = expression("Time"), 
     main = expression("EUR/USD"), ylim = c(-0.6,-0.1))
abline(h = median(daily_alpha_eurusd), col = "orange", lty = 1, lwd = 2)
abline(h = quantile(x = daily_alpha_eurusd, probs = 0.90), col = "black", 
       lty = 2, lwd = 2)
abline(h = quantile(x = daily_alpha_eurusd, probs = 0.10), col = "black",
       lty = 2, lwd = 2)

plot(unique(as.Date(Quarter_Hour_BV_1min_gbpusd_fcst$date.by_delta)),
     daily_alpha_gbpusd, type = "p",pch = 16,cex = 0.6, col = col_gbpusd,
     ylab = expression(alpha), xlab = expression("Time"), 
     main = expression("GBP/USD"),ylim = c(-0.6,-0.1))
abline(h = median(daily_alpha_gbpusd), col = "orange", lty = 1, lwd = 2.5)
abline(h = quantile(x = daily_alpha_gbpusd, probs = 0.90), col = "black", 
       lty = 2, lwd = 2)
abline(h = quantile(x = daily_alpha_gbpusd, probs = 0.10), col = "black",
       lty = 2, lwd = 2)

plot(unique(as.Date(Quarter_Hour_BV_1min_usdcad_fcst$date.by_delta)),
     daily_alpha_usdcad, type = "p",pch = 16,cex = 0.6, col = col_usdcad,
     ylab = expression(alpha), xlab = expression("Time"), 
     main = expression("USD/CAD"), ylim = c(-0.6,-0.1))
abline(h = median(daily_alpha_usdcad), col = "orange", lty = 1, lwd = 2)
abline(h = quantile(x = daily_alpha_usdcad, probs = 0.90), col = "black", 
       lty = 2, lwd = 2)
abline(h = quantile(x = daily_alpha_usdcad, probs = 0.10), col = "black",
       lty = 2, lwd = 2)

plot(unique(as.Date(Quarter_Hour_BV_1min_usdjpy_fcst$date.by_delta)),
     daily_alpha_usdjpy, type = "p",pch = 16,cex = 0.6, col = col_usdjpy,
     ylab = expression(alpha), xlab = expression("Time"), 
     main = expression("USD/CAD"), ylim = c(-0.6,-0.1))
abline(h = median(daily_alpha_usdjpy), col = "orange", lty = 1, lwd = 2)
abline(h = quantile(x = daily_alpha_usdjpy, probs = 0.90), col = "black", 
       lty = 2, lwd = 2)
abline(h = quantile(x = daily_alpha_usdjpy, probs = 0.10), col = "black",
       lty = 2, lwd = 2)

mtext(expression(paste("Daily roughness index estimates in FX markets",sep = "")), outer = TRUE, cex = 1.5)

# Plotting of rolling windows 

par(mfrow=c(2,2),oma = c(0, 0, 2, 0), mai = c(0.7, 0.65, 0.3, 0.1), mar=c(4.1, 4.1, 3.1, 1.1))

# EURUSD
plot(tail(unique(as.Date(Quarter_Hour_BV_1min_eurusd_fcst$date.by_delta)),
          length(roll_alpha_eurusd))[76:(length(roll_alpha_eurusd)-75)], ylim = c(-0.5,-0.25),
     roll_alpha_eurusd[76:(length(roll_alpha_eurusd)-75)], type = "l",lwd = 2, col = col_eurusd, 
     ylab = expression(alpha), xlab = expression("Dates"), main = expression("EUR/USD"))

lines(tail(unique(as.Date(Quarter_Hour_BV_1min_eurusd_fcst$date.by_delta)),
           length(roll_alpha_eurusd))[76:(length(roll_alpha_eurusd)-75)],
      rollmean(x = roll_alpha_eurusd, k = 151), col = "#8338EC", lwd = 2)
abline(h = quantile(x = roll_alpha_eurusd[76:(length(roll_alpha_eurusd)-75)], probs = 0.5), col = "orange", lty = 2, lwd = 2)

abline(v = as.Date("2014-10-15"), lwd=2, lty=2) # Recesion in Greece
legend(x = as.Date("2014-10-15") - 250, y = -0.25, legend = c("Recession in Greece"), bty = "n", cex = 1, text.font = 2)

# GBPUSD
plot(tail(unique(as.Date(Quarter_Hour_BV_1min_gbpusd_fcst$date.by_delta)),
          length(roll_alpha_gbpusd))[76:(length(roll_alpha_gbpusd)-75)],ylim = c(-0.5,-0.25),
     roll_alpha_gbpusd[76:(length(roll_alpha_gbpusd)-75)], type = "l",lwd = 2, col = col_gbpusd, 
     ylab = expression(alpha), xlab = expression("Dates"), main = expression("GBP/USD"))

lines(tail(unique(as.Date(Quarter_Hour_BV_1min_gbpusd_fcst$date.by_delta)),
           length(roll_alpha_gbpusd))[76:(length(roll_alpha_gbpusd)-75)],
      rollmean(x = roll_alpha_gbpusd, k = 151), col = "#8338EC", lwd = 2)
abline(h = quantile(x = roll_alpha_gbpusd[76:length(roll_alpha_gbpusd)], probs = 0.5), col = "orange", lty = 2, lwd = 2)

abline(v = as.Date("2016-06-23"), lwd=2, lty=2) # Brexit referendum
abline(v = as.Date("2016-11-09"), lwd=2, lty=2) # U.S Presidential elections

legend(x = as.Date("2016-06-23") - 900, y = -0.25, legend = c("Brexit \nreferendum"), bty = "n", cex = 1, text.font = 2)
legend(x = as.Date("2016-11-09") - 220, y = -0.20, legend = c("U.S\npresidential\nelections"), bty = "n", cex = 1, text.font = 2)

# USDCAD
plot(tail(unique(as.Date(Quarter_Hour_BV_1min_usdcad_fcst$date.by_delta)),
          length(roll_alpha_usdcad))[76:(length(roll_alpha_usdcad)-75)], ylim = c(-0.5,-0.25),
     roll_alpha_usdcad[76:(length(roll_alpha_usdcad)-75)], type = "l",lwd = 2, col = col_usdcad, 
     ylab = expression(alpha), xlab = expression("Dates"), main = expression("USD/CAD"))

lines(tail(unique(as.Date(Quarter_Hour_BV_1min_usdcad_fcst$date.by_delta)),
           length(roll_alpha_usdcad))[76:(length(roll_alpha_usdcad)-75)],
      rollmean(x = roll_alpha_usdcad, k = 151), col = "#8338EC", lwd = 2)
abline(h = quantile(x = roll_alpha_usdcad[76:length(roll_alpha_usdcad)], probs = 0.5), col = "orange", lty = 2, lwd = 2)

abline(v = as.Date("2015-01-19"), lwd=2, lty=2) # Global drop of oil prices
abline(v = as.Date("2016-11-19"), lwd=2, lty=2) # U.S elections

legend(x = as.Date("2015-01-19") - 950, y = -0.25, legend = c("Global drop\nof oil prices"), bty = "n", cex = 1, text.font = 2)
legend(x = as.Date("2016-11-09") - 180, y = -0.20, legend = c("U.S\npresidential\nelections"), bty = "n", cex = 1, text.font = 2)

# USDJPY
plot(tail(unique(as.Date(Quarter_Hour_BV_1min_usdjpy_fcst$date.by_delta)),
          length(roll_alpha_usdjpy))[76:(length(roll_alpha_usdjpy)-75)],ylim = c(-0.5,-0.25),
     roll_alpha_usdjpy[76:(length(roll_alpha_usdjpy)-75)], type = "l",lwd = 2, col = col_usdjpy, 
     ylab = expression(alpha), xlab = expression("Dates"), main = expression("USD/JPY"))

lines(tail(unique(as.Date(Quarter_Hour_BV_1min_usdjpy_fcst$date.by_delta)),
           length(roll_alpha_usdjpy))[76:(length(roll_alpha_usdjpy)-75)],
      rollmean(x = roll_alpha_usdjpy, k = 151), col = "#8338EC", lwd = 2)
abline(h = quantile(x = roll_alpha_usdjpy[76:length(roll_alpha_usdjpy)], probs = 0.5), col = "orange", lty = 2, lwd = 2)

abline(v = as.Date("2013-01-19"), lwd=2, lty=2) # Start of abenomics
abline(v = as.Date("2014-10-31"), lwd=2, lty=2) # QE increase
abline(v = as.Date("2016-01-1"), lwd=2, lty=2) # Negative rates by BOJ

legend(x = as.Date("2013-01-19") - 950, y = -0.25, legend = c("Start of\nAbenomics"), bty = "n", cex = 1, text.font = 2)
legend(x = as.Date("2014-10-31") - 900, y = -0.25, legend = c("Increase of\nBOJ QE"), bty = "n", cex = 1, text.font = 2)
legend(x = as.Date("2016-01-1") - 250, y = -0.25, legend = c("Negative rates\nby BOJ"), bty = "n", cex = 1, text.font = 2)

mtext(expression(paste("Time variation of roughness index",sep = "")), outer = TRUE, cex = 1.5)
