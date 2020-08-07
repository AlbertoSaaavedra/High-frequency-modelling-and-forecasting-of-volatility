
# Periodicity plotting ----------------------------------------------------

setwd("C:/Users/Alberto Saavedra/OneDrive/Documentos/MSc in Statistics/Research project/Programing/Roughness parameter/Periodicity estimates")

load("per_eurusd2.Rout")
load("per_gbpusd2.Rout")
load("per_usdcad2.Rout")
load("per_usdjpy2.Rout")

col_ny <- rgb(red = col2rgb(col = "#96031A")[,1]["red"]/255,
              green = col2rgb(col = "#96031A")[,1]["green"]/255,
              blue = col2rgb(col = "#96031A")[,1]["blue"]/255,
              alpha = 0.20)

col_tokyo <- rgb(red = col2rgb(col = "#FAA916")[,1]["red"]/255,
                 green = col2rgb(col = "#FAA916")[,1]["green"]/255,
                 blue = col2rgb(col = "#FAA916")[,1]["blue"]/255,
                 alpha = 0.20)

col_london <- rgb(red = col2rgb(col = "#0B7A75")[,1]["red"]/255,
                  green = col2rgb(col = "#0B7A75")[,1]["green"]/255,
                  blue = col2rgb(col = "#0B7A75")[,1]["blue"]/255,
                  alpha = 0.20)

par(mfrow=c(2,2),oma = c(0, 0, 2, 0), mai = c(0.7, 0.67, 0.3, 0.1))


# 15 mins -----------------------------------------------------------------

times_15mins <- seq(from=as.POSIXct("2019-07-31 00:00", tz="UTC"),
                    to=as.POSIXct("2019-07-31 20:45", tz="UTC"),
                    by="15 mins")

times_tokyo <- seq(from=as.POSIXct("2019-07-31 00:00", tz="UTC"),
                   to=as.POSIXct("2019-07-31 09:00", tz="UTC"),
                   by="15 mins")

times_london <- seq(from=as.POSIXct("2019-07-31 07:00", tz="UTC"),
                    to=as.POSIXct("2019-07-31 15:00", tz="UTC"),
                    by="15 mins")

times_ny <- seq(from=as.POSIXct("2019-07-31 12:00", tz="UTC"),
                to=as.POSIXct("2019-07-31 20:45", tz="UTC"),
                by="15 mins")

# EURUSD
plot(x = times_15mins,
     y = per_eurusd$`from_5/2018_to_4/2019`$freq_15_mins$fi_tml, lty = 1, type="l", 
     ylim = c(0.4,
              2), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("EUR/USD"), col = "#FF1654", lwd=2)
lines(x = times_15mins, col = "#247BA0", lwd=2,
      y = per_eurusd$`from_5/2018_to_4/2019`$freq_15_mins$fi_ols, lty = 2)
lines(x = times_15mins, col = "#FF9F1C", lwd=2,
      y = per_eurusd$`from_5/2018_to_4/2019`$freq_15_mins$fi_wsd, lty = 5)

polygon(x=c(rep(head(times_tokyo,1),2),rep(tail(times_tokyo,1),2)),
        y=c(c(1.75,2),c(2,1.75)),
        density=NA, col=col_tokyo,
        border=NA)

polygon(x=c(rep(head(times_london,1),2),rep(tail(times_london,1),2)),
        y=c(c(1.75,2),c(2,1.75)),
        density=NA, col=col_london,
        border=NA)

polygon(x=c(rep(head(times_ny,1),2),rep(tail(times_ny,1),2)),
        y=c(c(1.75,2),c(2,1.75)),
        density=NA, col=col_ny,
        border=NA)

legend(x = head(times_tokyo,1), y = 2.225,text.font = 3,
       legend = c("Tokyo \n trading session"),bty = "n", cex=0.9)
legend(x = head(times_london,1) + 1800, y = 2.225,text.font = 3,
       legend = c("London \n trading session"),bty = "n", cex=0.9)
legend(x = head(times_ny,1) + 7000, y = 2.225,text.font = 3,
       legend = c("New York \n trading session"),bty = "n", cex=0.9)

# GBPUSD
plot(x = times_15mins,
     y = per_gbpusd$`from_5/2018_to_4/2019`$freq_15_mins$fi_tml, lty = 1, type="l", 
     ylim = c(0.4,
              2), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("GBP/USD"), col = "#FF1654", lwd=2)
lines(x = times_15mins, col = "#247BA0", lwd=2,
      y = per_gbpusd$`from_5/2018_to_4/2019`$freq_15_mins$fi_ols, lty = 2)
lines(x = times_15mins, col = "#FF9F1C", lwd=2,
      y = per_gbpusd$`from_5/2018_to_4/2019`$freq_15_mins$fi_wsd, lty = 5)

polygon(x=c(rep(head(times_tokyo,1),2),rep(tail(times_tokyo,1),2)),
        y=c(c(1.75,2),c(2,1.75)),
        density=NA, col=col_tokyo,
        border=NA)

polygon(x=c(rep(head(times_london,1),2),rep(tail(times_london,1),2)),
        y=c(c(1.75,2),c(2,1.75)),
        density=NA, col=col_london,
        border=NA)

polygon(x=c(rep(head(times_ny,1),2),rep(tail(times_ny,1),2)),
        y=c(c(1.75,2),c(2,1.75)),
        density=NA, col=col_ny,
        border=NA)

# USDCAD
plot(x = times_15mins,
     y = per_usdcad$`from_5/2018_to_4/2019`$freq_15_mins$fi_tml, lty = 1, type="l", 
     ylim = c(0.4,
              2), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("USD/CAD"), col = "#FF1654", lwd=2)
lines(x = times_15mins, col = "#247BA0", lwd=2,
      y = per_usdcad$`from_5/2018_to_4/2019`$freq_15_mins$fi_ols, lty = 2)
lines(x = times_15mins, col = "#FF9F1C", lwd=2,
      y = per_usdcad$`from_5/2018_to_4/2019`$freq_15_mins$fi_wsd, lty = 5)

polygon(x=c(rep(head(times_tokyo,1),2),rep(tail(times_tokyo,1),2)),
        y=c(c(1.75,2),c(2,1.75)),
        density=NA, col=col_tokyo,
        border=NA)

polygon(x=c(rep(head(times_london,1),2),rep(tail(times_london,1),2)),
        y=c(c(1.75,2),c(2,1.75)),
        density=NA, col=col_london,
        border=NA)

polygon(x=c(rep(head(times_ny,1),2),rep(tail(times_ny,1),2)),
        y=c(c(1.75,2),c(2,1.75)),
        density=NA, col=col_ny,
        border=NA)

legend("left", legend = c("TML","OLS","WSD"), lty = c(1,2,5), cex=0.9,
       lwd = rep(2,3), col = c("#FF1654","#247BA0","#FF9F1C"), bty = "n")

# USDJPY
plot(x = times_15mins,
     y = per_usdjpy$`from_5/2018_to_4/2019`$freq_15_mins$fi_tml, lty = 1, type="l", 
     ylim = c(0.4,
              2), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("USD/JPY"), col = "#FF1654", lwd=2)
lines(x = times_15mins, col = "#247BA0", lwd=2,
      y = per_usdjpy$`from_5/2018_to_4/2019`$freq_15_mins$fi_ols, lty = 2)
lines(x = times_15mins, col = "#FF9F1C", lwd=2,
      y = per_usdjpy$`from_5/2018_to_4/2019`$freq_15_mins$fi_wsd, lty = 5)

polygon(x=c(rep(head(times_tokyo,1),2),rep(tail(times_tokyo,1),2)),
        y=c(c(1.75,2),c(2,1.75)),
        density=NA, col=col_tokyo,
        border=NA)

polygon(x=c(rep(head(times_london,1),2),rep(tail(times_london,1),2)),
        y=c(c(1.75,2),c(2,1.75)),
        density=NA, col=col_london,
        border=NA)

polygon(x=c(rep(head(times_ny,1),2),rep(tail(times_ny,1),2)),
        y=c(c(1.75,2),c(2,1.75)),
        density=NA, col=col_ny,
        border=NA)

mtext(expression(paste("Intraday Periodicity Estimates: 15 mins freq.",sep = "")), outer = TRUE, cex = 1.5)


# 30 mins -----------------------------------------------------------------

times_30mins <- seq(from=as.POSIXct("2019-07-31 00:00", tz="UTC"),
                    to=as.POSIXct("2019-07-31 20:30", tz="UTC"),
                    by="30 mins")

# EURUSD
plot(x = times_30mins,
     y = per_eurusd$`from_5/2018_to_4/2019`$freq_30_mins$fi_tml, lty = 1, type="l", 
     ylim = c(min(per_eurusd$`from_5/2018_to_4/2019`$freq_30_mins[,-1]),
              max(per_eurusd$`from_5/2018_to_4/2019`$freq_30_mins[,-1])), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("EUR/USD"), col = "#FF1654", lwd=2)
lines(x = times_30mins, col = "#247BA0", lwd=2,
      y = per_eurusd$`from_5/2018_to_4/2019`$freq_30_mins$fi_ols, lty = 2)
lines(x = times_30mins, col = "#FF9F1C", lwd=2,
      y = per_eurusd$`from_5/2018_to_4/2019`$freq_30_mins$fi_wsd, lty = 5)

# GBPUSD
plot(x = times_30mins,
     y = per_gbpusd$`from_5/2018_to_4/2019`$freq_30_mins$fi_tml, lty = 1, type="l", 
     ylim = c(min(per_gbpusd$`from_5/2018_to_4/2019`$freq_30_mins[,-1]),
              max(per_gbpusd$`from_5/2018_to_4/2019`$freq_30_mins[,-1])), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("GBP/USD"), col = "#FF1654", lwd=2)
lines(x = times_30mins, col = "#247BA0", lwd=2,
      y = per_gbpusd$`from_5/2018_to_4/2019`$freq_30_mins$fi_ols, lty = 2)
lines(x = times_30mins, col = "#FF9F1C", lwd=2,
      y = per_gbpusd$`from_5/2018_to_4/2019`$freq_30_mins$fi_wsd, lty = 5)

# USDCAD
plot(x = times_30mins,
     y = per_usdcad$`from_5/2018_to_4/2019`$freq_30_mins$fi_tml, lty = 1, type="l", 
     ylim = c(min(per_usdcad$`from_5/2018_to_4/2019`$freq_30_mins[,-1]),
              max(per_usdcad$`from_5/2018_to_4/2019`$freq_30_mins[,-1])), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("USD/CAD"), col = "#FF1654", lwd=2)
lines(x = times_30mins, col = "#247BA0", lwd=2,
      y = per_usdcad$`from_5/2018_to_4/2019`$freq_30_mins$fi_ols, lty = 2)
lines(x = times_30mins, col = "#FF9F1C", lwd=2,
      y = per_usdcad$`from_5/2018_to_4/2019`$freq_30_mins$fi_wsd, lty = 5)

legend("topleft", legend = c("TML","OLS","WSD"), lty = c(1,2,5), 
       lwd = rep(2,3), col = c("#FF1654","#247BA0","#FF9F1C"), bty = "n")

# USDJPY
plot(x = times_30mins,
     y = per_usdjpy$`from_5/2018_to_4/2019`$freq_30_mins$fi_tml, lty = 1, type="l", 
     ylim = c(min(per_usdjpy$`from_5/2018_to_4/2019`$freq_30_mins[,-1]),
              max(per_usdjpy$`from_5/2018_to_4/2019`$freq_30_mins[,-1])), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("USD/JPY"), col = "#FF1654", lwd=2)
lines(x = times_30mins, col = "#247BA0", lwd=2,
      y = per_usdjpy$`from_5/2018_to_4/2019`$freq_30_mins$fi_ols, lty = 2)
lines(x = times_30mins, col = "#FF9F1C", lwd=2,
      y = per_usdjpy$`from_5/2018_to_4/2019`$freq_30_mins$fi_wsd, lty = 5)

mtext(expression(paste("Intraday Seasonality Estimates: 30 mins freq.",sep = "")), outer = TRUE, cex = 1.5)


# 60 mins -----------------------------------------------------------------

times_60mins <- seq(from=as.POSIXct("2019-07-31 00:00", tz="UTC"),
                    to=as.POSIXct("2019-07-31 20:00", tz="UTC"),
                    by="60 mins")

# EURUSD
plot(x = times_60mins,
     y = per_eurusd$`from_5/2018_to_4/2019`$freq_60_mins$fi_tml, lty = 1, type="l", 
     ylim = c(min(per_eurusd$`from_5/2018_to_4/2019`$freq_60_mins[,-1]),
              max(per_eurusd$`from_5/2018_to_4/2019`$freq_60_mins[,-1])), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("EUR/USD"), col = "#FF1654", lwd=2)
lines(x = times_60mins, col = "#247BA0", lwd=2,
      y = per_eurusd$`from_5/2018_to_4/2019`$freq_60_mins$fi_ols, lty = 2)
lines(x = times_60mins, col = "#FF9F1C", lwd=2,
      y = per_eurusd$`from_5/2018_to_4/2019`$freq_60_mins$fi_wsd, lty = 5)

# GBPUSD
plot(x = times_60mins,
     y = per_gbpusd$`from_5/2018_to_4/2019`$freq_60_mins$fi_tml, lty = 1, type="l", 
     ylim = c(min(per_gbpusd$`from_5/2018_to_4/2019`$freq_60_mins[,-1]),
              max(per_gbpusd$`from_5/2018_to_4/2019`$freq_60_mins[,-1])), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("GBP/USD"), col = "#FF1654", lwd=2)
lines(x = times_60mins, col = "#247BA0", lwd=2,
      y = per_gbpusd$`from_5/2018_to_4/2019`$freq_60_mins$fi_ols, lty = 2)
lines(x = times_60mins, col = "#FF9F1C", lwd=2,
      y = per_gbpusd$`from_5/2018_to_4/2019`$freq_60_mins$fi_wsd, lty = 5)

# USDCAD
plot(x = times_60mins,
     y = per_usdcad$`from_5/2018_to_4/2019`$freq_60_mins$fi_tml, lty = 1, type="l", 
     ylim = c(min(per_usdcad$`from_5/2018_to_4/2019`$freq_60_mins[,-1]),
              max(per_usdcad$`from_5/2018_to_4/2019`$freq_60_mins[,-1])), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("USD/CAD"), col = "#FF1654", lwd=2)
lines(x = times_60mins, col = "#247BA0", lwd=2,
      y = per_usdcad$`from_5/2018_to_4/2019`$freq_60_mins$fi_ols, lty = 2)
lines(x = times_60mins, col = "#FF9F1C", lwd=2,
      y = per_usdcad$`from_5/2018_to_4/2019`$freq_60_mins$fi_wsd, lty = 5)

legend("topleft", legend = c("TML","OLS","WSD"), lty = c(1,2,5), 
       lwd = rep(2,3), col = c("#FF1654","#247BA0","#FF9F1C"), bty = "n")

# USDJPY
plot(x = times_60mins,
     y = per_usdjpy$`from_5/2018_to_4/2019`$freq_60_mins$fi_tml, lty = 1, type="l", 
     ylim = c(min(per_usdjpy$`from_5/2018_to_4/2019`$freq_60_mins[,-1]),
              max(per_usdjpy$`from_5/2018_to_4/2019`$freq_60_mins[,-1])), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("USD/JPY"), col = "#FF1654", lwd=2)
lines(x = times_60mins, col = "#247BA0", lwd=2,
      y = per_usdjpy$`from_5/2018_to_4/2019`$freq_60_mins$fi_ols, lty = 2)
lines(x = times_60mins, col = "#FF9F1C", lwd=2,
      y = per_usdjpy$`from_5/2018_to_4/2019`$freq_60_mins$fi_wsd, lty = 5)

mtext(expression(paste("Intraday Seasonality Estimates: 60 mins freq.",sep = "")), outer = TRUE, cex = 1.5)


# 126 mins -----------------------------------------------------------------

times_126mins <- seq(from=as.POSIXct("2019-07-31 2:06", tz="UTC"),
                    to=as.POSIXct("2019-07-31 21:00:00", tz="UTC"),
                    by="126 mins")

# EURUSD
plot(x = times_126mins,
     y = per_eurusd$`from_5/2018_to_4/2019`$freq_126_mins$fi_tml, lty = 1, type="l", 
     ylim = c(min(per_eurusd$`from_5/2018_to_4/2019`$freq_126_mins[,-1]),
              max(per_eurusd$`from_5/2018_to_4/2019`$freq_126_mins[,-1])), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("EUR/USD"), col = "#FF1654", lwd=2)
lines(x = times_126mins, col = "#247BA0", lwd=2,
      y = per_eurusd$`from_5/2018_to_4/2019`$freq_126_mins$fi_ols, lty = 2)
lines(x = times_126mins, col = "#FF9F1C", lwd=2,
      y = per_eurusd$`from_5/2018_to_4/2019`$freq_126_mins$fi_wsd, lty = 5)

# GBPUSD
plot(x = times_126mins,
     y = per_gbpusd$`from_5/2018_to_4/2019`$freq_126_mins$fi_tml, lty = 1, type="l", 
     ylim = c(min(per_gbpusd$`from_5/2018_to_4/2019`$freq_126_mins[,-1]),
              max(per_gbpusd$`from_5/2018_to_4/2019`$freq_126_mins[,-1])), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("GBP/USD"), col = "#FF1654", lwd=2)
lines(x = times_126mins, col = "#247BA0", lwd=2,
      y = per_gbpusd$`from_5/2018_to_4/2019`$freq_126_mins$fi_ols, lty = 2)
lines(x = times_126mins, col = "#FF9F1C", lwd=2,
      y = per_gbpusd$`from_5/2018_to_4/2019`$freq_126_mins$fi_wsd, lty = 5)

# USDCAD
plot(x = times_126mins,
     y = per_usdcad$`from_5/2018_to_4/2019`$freq_126_mins$fi_tml, lty = 1, type="l", 
     ylim = c(min(per_usdcad$`from_5/2018_to_4/2019`$freq_126_mins[,-1]),
              max(per_usdcad$`from_5/2018_to_4/2019`$freq_126_mins[,-1])), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("USD/CAD"), col = "#FF1654", lwd=2)
lines(x = times_126mins, col = "#247BA0", lwd=2,
      y = per_usdcad$`from_5/2018_to_4/2019`$freq_126_mins$fi_ols, lty = 2)
lines(x = times_126mins, col = "#FF9F1C", lwd=2,
      y = per_usdcad$`from_5/2018_to_4/2019`$freq_126_mins$fi_wsd, lty = 5)

legend("topleft", legend = c("TML","OLS","WSD"), lty = c(1,2,5), 
       lwd = rep(2,3), col = c("#FF1654","#247BA0","#FF9F1C"), bty = "n")

# USDJPY
plot(x = times_126mins,
     y = per_usdjpy$`from_5/2018_to_4/2019`$freq_126_mins$fi_tml, lty = 1, type="l", 
     ylim = c(min(per_usdjpy$`from_5/2018_to_4/2019`$freq_126_mins[,-1]),
              max(per_usdjpy$`from_5/2018_to_4/2019`$freq_126_mins[,-1])), 
     ylab = expression("FFF estimate"), xlab = expression("Time of the day (GMT)"), 
     main = expression("USD/JPY"), col = "#FF1654", lwd=2)
lines(x = times_126mins, col = "#247BA0", lwd=2,
      y = per_usdjpy$`from_5/2018_to_4/2019`$freq_126_mins$fi_ols, lty = 2)
lines(x = times_126mins, col = "#FF9F1C", lwd=2,
      y = per_usdjpy$`from_5/2018_to_4/2019`$freq_126_mins$fi_wsd, lty = 5)

mtext(expression(paste("Intraday Seasonality Estimates: 126 mins freq.",sep = "")), outer = TRUE, cex = 1.5)
