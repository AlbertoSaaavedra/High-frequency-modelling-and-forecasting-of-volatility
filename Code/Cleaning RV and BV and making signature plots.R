
# Libraries ---------------------------------------------------------------

require(lubridate)
require(data.table)
require(Hmisc)
require(plyr)
require(fasttime)
require(caTools)

# Loading daily IV estimates and cleaning them ----------------------------

setwd("C:/Users/Alberto Saavedra/OneDrive/Documentos/MSc in Statistics/Research project/Programing/IV Est by sec (no shifting)/Bi-Hourly (21h) RV and BV data tables")

load("Bi_Hourly_BV_Est_1h_15_deltas.Rout")
load("Bi_Hourly_RV_Est_1h_15_deltas.Rout")
load("BV_Est_Bi_Hourly_extra_deltas.Rout")
load("RV_Est_Bi_Hourly_extra_deltas.Rout")

RV_Est_Bi_Hourly_all_deltas <- list()

for (j in 1:12) {
  RV_Est_Bi_Hourly_all_deltas[[j]] <- RV_Est_Bi_Hourly_extra_deltas[[j]]
  names(RV_Est_Bi_Hourly_all_deltas)[j] <- names(RV_Est_Bi_Hourly_extra_deltas)[j]
}

i <- 12
k <- 13
for (j in 1:15) {
  i <- i+1
  RV_Est_Bi_Hourly_all_deltas[[i]] <- Bi_Hourly_RV_Est_1h_15_deltas[[j]]
  names(RV_Est_Bi_Hourly_all_deltas)[i] <- names(Bi_Hourly_RV_Est_1h_15_deltas)[j]
  i <- i+1
  RV_Est_Bi_Hourly_all_deltas[[i]] <- RV_Est_Bi_Hourly_extra_deltas[[k]]
  names(RV_Est_Bi_Hourly_all_deltas)[i] <- names(RV_Est_Bi_Hourly_extra_deltas)[k]
  k <- k+1
}

BV_Est_Bi_Hourly_all_deltas <- list()

for (j in 1:12) {
  BV_Est_Bi_Hourly_all_deltas[[j]] <- BV_Est_Bi_Hourly_extra_deltas[[j]]
  names(BV_Est_Bi_Hourly_all_deltas)[j] <- names(BV_Est_Bi_Hourly_extra_deltas)[j]
}

i <- 12
k <- 13
for (j in 1:15) {
  i <- i+1
  BV_Est_Bi_Hourly_all_deltas[[i]] <- Bi_Hourly_BV_Est_1h_15_deltas[[j]]
  names(BV_Est_Bi_Hourly_all_deltas)[i] <- names(Bi_Hourly_BV_Est_1h_15_deltas)[j]
  i <- i+1
  BV_Est_Bi_Hourly_all_deltas[[i]] <- BV_Est_Bi_Hourly_extra_deltas[[k]]
  names(BV_Est_Bi_Hourly_all_deltas)[i] <- names(BV_Est_Bi_Hourly_extra_deltas)[k]
  k <- k+1
}

# Cleaning estimates from dates with no info ------------------------------

# Incomplete information --------------------------------------------------

# Loading lists of days to exclude for each currency (due to have incomplete information)

setwd("C:/Users/Alberto Saavedra/OneDrive/Documentos/MSc in Statistics/Research project/Programing/IV Est by sec (no shifting)/Days out per currency data tables")

load("wday_out_eurusd.Rout")
load("wday_out_gbpusd.Rout")
load("wday_out_usdcad.Rout")
load("wday_out_usdjpy.Rout")

days_out_inc_info_eurusd <- as.Date(wday_out_eurusd$first_ts_in_day)
days_out_inc_info_gbpusd <- as.Date(wday_out_gbpusd$first_ts_in_day)
days_out_inc_info_usdcad <- as.Date(wday_out_usdcad$first_ts_in_day)
days_out_inc_info_usdjpy <- as.Date(wday_out_usdjpy$first_ts_in_day)

days_out_inc_info_eurusd <- as.data.table(days_out_inc_info_eurusd)
days_out_inc_info_gbpusd <- as.data.table(days_out_inc_info_gbpusd)
days_out_inc_info_usdcad <- as.data.table(days_out_inc_info_usdcad)
days_out_inc_info_usdjpy <- as.data.table(days_out_inc_info_usdjpy)

colnames(days_out_inc_info_eurusd) <- "full_days_out_eurusd"
colnames(days_out_inc_info_gbpusd) <- "full_days_out_gbpusd"
colnames(days_out_inc_info_usdcad) <- "full_days_out_usdcad"
colnames(days_out_inc_info_usdjpy) <- "full_days_out_usdjpy"

# Completely missing days -------------------------------------------

setwd("C:/Users/Alberto Saavedra/OneDrive/Documentos/MSc in Statistics/Research project/Programing/IV Est by sec (no shifting)/Days with no information data tables")

load("full_days_out_eurusd.Rout")
load("full_days_out_gbpusd.Rout")
load("full_days_out_usdcad.Rout")
load("full_days_out_usdjpy.Rout")

full_days_out_eurusd <- rbind(full_days_out_eurusd,days_out_inc_info_eurusd)
full_days_out_gbpusd <- rbind(full_days_out_gbpusd,days_out_inc_info_gbpusd)
full_days_out_usdcad <- rbind(full_days_out_usdcad,days_out_inc_info_usdcad)
full_days_out_usdjpy <- rbind(full_days_out_usdjpy,days_out_inc_info_usdjpy)

# Excluding selected dates from RV and BV series --------------------------

delta <- seq(0,600,by = 15)
delta[1] <- 1
RV_Clean_eurusd <- list()
RV_Clean_gbpusd <- list()
RV_Clean_usdcad <- list()
RV_Clean_usdjpy <- list()

for (d in delta) {
  id <- paste("delta_",d,"_secs",sep = "")
  # Eliminating days with no information
  RV_Clean_eurusd[[id]] <- subset(x = RV_Est_Bi_Hourly_all_deltas[[id]],
                                  subset = !(as.Date(date.by_delta)  %in% full_days_out_eurusd$full_days_out_eurusd),
                                  select = c(date.by_delta,eurusd))
  RV_Clean_gbpusd[[id]] <- subset(x = RV_Est_Bi_Hourly_all_deltas[[id]],
                                  subset = !(as.Date(date.by_delta)  %in% full_days_out_gbpusd$full_days_out_gbpusd),
                                  select = c(date.by_delta,gbpusd))
  RV_Clean_usdcad[[id]] <- subset(x = RV_Est_Bi_Hourly_all_deltas[[id]],
                                  subset = !(as.Date(date.by_delta)  %in% full_days_out_usdcad$full_days_out_usdcad),
                                  select = c(date.by_delta,usdcad))
  RV_Clean_usdjpy[[id]] <- subset(x = RV_Est_Bi_Hourly_all_deltas[[id]],
                                  subset = !(as.Date(date.by_delta)  %in% full_days_out_usdjpy$full_days_out_usdjpy),
                                  select = c(date.by_delta,usdjpy))
  # Eliminating Chrsitmas
  RV_Clean_eurusd[[id]] <- subset(x = RV_Clean_eurusd[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 25))
  RV_Clean_gbpusd[[id]] <- subset(x = RV_Clean_gbpusd[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 25))
  RV_Clean_usdcad[[id]] <- subset(x = RV_Clean_usdcad[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 25))
  RV_Clean_usdjpy[[id]] <- subset(x = RV_Clean_usdjpy[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 25))
  # Eliminating last day of the year
  RV_Clean_eurusd[[id]] <- subset(x = RV_Clean_eurusd[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 31))
  RV_Clean_gbpusd[[id]] <- subset(x = RV_Clean_gbpusd[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 31))
  RV_Clean_usdcad[[id]] <- subset(x = RV_Clean_usdcad[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 31))
  RV_Clean_usdjpy[[id]] <- subset(x = RV_Clean_usdjpy[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 31))
  # Eliminating first day of the year
  RV_Clean_eurusd[[id]] <- subset(x = RV_Clean_eurusd[[id]],
                                  subset = !(month(date.by_delta) == 1 & day(date.by_delta) == 1))
  RV_Clean_gbpusd[[id]] <- subset(x = RV_Clean_gbpusd[[id]],
                                  subset = !(month(date.by_delta) == 1 & day(date.by_delta) == 1))
  RV_Clean_usdcad[[id]] <- subset(x = RV_Clean_usdcad[[id]],
                                  subset = !(month(date.by_delta) == 1 & day(date.by_delta) == 1))
  RV_Clean_usdjpy[[id]] <- subset(x = RV_Clean_usdjpy[[id]],
                                  subset = !(month(date.by_delta) == 1 & day(date.by_delta) == 1))
}

# BV

BV_Clean_eurusd <- list()
BV_Clean_gbpusd <- list()
BV_Clean_usdcad <- list()
BV_Clean_usdjpy <- list()

for (d in delta) {
  id <- paste("delta_",d,"_secs",sep = "")
  # Eliminating days with no information
  BV_Clean_eurusd[[id]] <- subset(x = BV_Est_Bi_Hourly_all_deltas[[id]],
                                  subset = !(as.Date(date.by_delta)  %in% full_days_out_eurusd$full_days_out_eurusd),
                                  select = c(date.by_delta,eurusd))
  BV_Clean_gbpusd[[id]] <- subset(x = BV_Est_Bi_Hourly_all_deltas[[id]],
                                  subset = !(as.Date(date.by_delta)  %in% full_days_out_gbpusd$full_days_out_gbpusd),
                                  select = c(date.by_delta,gbpusd))
  BV_Clean_usdcad[[id]] <- subset(x = BV_Est_Bi_Hourly_all_deltas[[id]],
                                  subset = !(as.Date(date.by_delta)  %in% full_days_out_usdcad$full_days_out_usdcad),
                                  select = c(date.by_delta,usdcad))
  BV_Clean_usdjpy[[id]] <- subset(x = BV_Est_Bi_Hourly_all_deltas[[id]],
                                  subset = !(as.Date(date.by_delta)  %in% full_days_out_usdjpy$full_days_out_usdjpy),
                                  select = c(date.by_delta,usdjpy))
  # Eliminating Chrsitmas
  BV_Clean_eurusd[[id]] <- subset(x = BV_Clean_eurusd[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 25))
  BV_Clean_gbpusd[[id]] <- subset(x = BV_Clean_gbpusd[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 25))
  BV_Clean_usdcad[[id]] <- subset(x = BV_Clean_usdcad[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 25))
  BV_Clean_usdjpy[[id]] <- subset(x = BV_Clean_usdjpy[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 25))
  # Eliminating last day of the year
  BV_Clean_eurusd[[id]] <- subset(x = BV_Clean_eurusd[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 31))
  BV_Clean_gbpusd[[id]] <- subset(x = BV_Clean_gbpusd[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 31))
  BV_Clean_usdcad[[id]] <- subset(x = BV_Clean_usdcad[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 31))
  BV_Clean_usdjpy[[id]] <- subset(x = BV_Clean_usdjpy[[id]],
                                  subset = !(month(date.by_delta) == 12 & day(date.by_delta) == 31))
  # Eliminating first day of the year
  BV_Clean_eurusd[[id]] <- subset(x = BV_Clean_eurusd[[id]],
                                  subset = !(month(date.by_delta) == 1 & day(date.by_delta) == 1))
  BV_Clean_gbpusd[[id]] <- subset(x = BV_Clean_gbpusd[[id]],
                                  subset = !(month(date.by_delta) == 1 & day(date.by_delta) == 1))
  BV_Clean_usdcad[[id]] <- subset(x = BV_Clean_usdcad[[id]],
                                  subset = !(month(date.by_delta) == 1 & day(date.by_delta) == 1))
  BV_Clean_usdjpy[[id]] <- subset(x = BV_Clean_usdjpy[[id]],
                                  subset = !(month(date.by_delta) == 1 & day(date.by_delta) == 1))
}

# Computing mean vals for RV and BV ---------------------------------------

# RV

RV_means_eurusd <- matrix(data = NA, nrow = length(delta),ncol = 1)
RV_means_gbpusd <- matrix(data = NA, nrow = length(delta),ncol = 1)
RV_means_usdcad <- matrix(data = NA, nrow = length(delta),ncol = 1)
RV_means_usdjpy <- matrix(data = NA, nrow = length(delta),ncol = 1)
for (k in 1:length(delta)) {
  RV_means_eurusd[k,] <- colMeans(subset(x = RV_Clean_eurusd[[k]],select = c(eurusd)))
  RV_means_gbpusd[k,] <- colMeans(subset(x = RV_Clean_gbpusd[[k]],select = c(gbpusd)))
  RV_means_usdcad[k,] <- colMeans(subset(x = RV_Clean_usdcad[[k]],select = c(usdcad)))
  RV_means_usdjpy[k,] <- colMeans(subset(x = RV_Clean_usdjpy[[k]],select = c(usdjpy)))
}

# BV

BV_means_eurusd <- matrix(data = NA, nrow = length(delta),ncol = 1)
BV_means_gbpusd <- matrix(data = NA, nrow = length(delta),ncol = 1)
BV_means_usdcad <- matrix(data = NA, nrow = length(delta),ncol = 1)
BV_means_usdjpy <- matrix(data = NA, nrow = length(delta),ncol = 1)
for (k in 1:length(delta)) {
  BV_means_eurusd[k,] <- colMeans(subset(x = BV_Clean_eurusd[[k]],select = c(eurusd)))
  BV_means_gbpusd[k,] <- colMeans(subset(x = BV_Clean_gbpusd[[k]],select = c(gbpusd)))
  BV_means_usdcad[k,] <- colMeans(subset(x = BV_Clean_usdcad[[k]],select = c(usdcad)))
  BV_means_usdjpy[k,] <- colMeans(subset(x = BV_Clean_usdjpy[[k]],select = c(usdjpy)))
}


# Signature plots ---------------------------------------------------------

# All currencies together -------------------------------------------------

old.par <- par(mar = c(0, 0, 0, 0))

par(mfrow=c(2,2),oma = c(0, 0, 2, 0), mai = c(0.7, 0.65, 0.3, 0.1))

col_eurusd <- "#1B998B"
col_gbpusd <- "#D1495B"
col_usdcad <- "#296EB4"
col_usdjpy <- "#848C8E"

## Realised Variance

### All values

# EURUSD

plot(delta,(RV_means_eurusd[,1]-mean(RV_means_eurusd[,1]))/mean(RV_means_eurusd[,1]),
     ylab = expression("Mean RV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#1B998B",cex.axis=0.8, main = expression("EUR/USD"))
grid()

# GBPUSD

plot(delta,(RV_means_gbpusd[,1]-mean(RV_means_gbpusd[,1]))/mean(RV_means_gbpusd[,1]),
     ylab = expression("Mean RV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#D1495B",cex.axis=0.8, main = expression("GBP/USD"))

grid()

# USDCAD

plot(delta,(RV_means_usdcad[,1]-mean(RV_means_usdcad[,1]))/mean(RV_means_usdcad[,1]),
     ylab = expression("Mean RV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#296EB4",cex.axis=0.8, main = expression("USD/CAD"))
grid()

# USDJPY

plot(delta,(RV_means_usdjpy[,1]-mean(RV_means_usdjpy[,1]))/mean(RV_means_usdjpy[,1]),
     ylab = expression("Mean RV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#848C8E",cex.axis=0.8, main = expression("USD/JPY"))
grid()

mtext(expression(paste(Delta,"-signature plots for Bi-hourly Realised Variance",sep = "")), outer = TRUE, cex = 1.5)

## Without outlier

# EURUSD

plot(delta[-1],(RV_means_eurusd[,1][-1]-mean(RV_means_eurusd[,1][-1]))/mean(RV_means_eurusd[,1][-1]),
     ylab = expression("Mean RV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#1B998B",cex.axis=0.8, main = expression("EUR/USD"))
grid()

# GBPUSD

plot(delta[-1],(RV_means_gbpusd[,1][-1]-mean(RV_means_gbpusd[,1][-1]))/mean(RV_means_gbpusd[,1][-1]),
     ylab = expression("Mean RV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#D1495B",cex.axis=0.8, main = expression("GBP/USD"))

grid()

# USDCAD

plot(delta[-1],(RV_means_usdcad[,1][-1]-mean(RV_means_usdcad[,1][-1]))/mean(RV_means_usdcad[,1][-1]),
     ylab = expression("Mean RV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#296EB4",cex.axis=0.8, main = expression("USD/CAD"))
grid()

# USDJPY

plot(delta[-1],(RV_means_usdjpy[,1][-1]-mean(RV_means_usdjpy[,1][-1]))/mean(RV_means_usdjpy[,1][-1]),
     ylab = expression("Mean RV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#848C8E",cex.axis=0.8, main = expression("USD/JPY"))
grid()

mtext(expression(paste(Delta,"-signature plots (without outlier) for Realised Variance",sep = "")), outer = TRUE, cex = 1.5)

### Bipower Variation

## All values

### All values

# EURUSD

plot(delta,(BV_means_eurusd[,1]-mean(BV_means_eurusd[,1]))/mean(BV_means_eurusd[,1]),
     ylab = expression("Mean BV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#1B998B",cex.axis=0.8, main = expression("EUR/USD"))
grid()

# GBPUSD

plot(delta,(BV_means_gbpusd[,1]-mean(BV_means_gbpusd[,1]))/mean(BV_means_gbpusd[,1]),
     ylab = expression("Mean BV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#D1495B",cex.axis=0.8, main = expression("GBP/USD"))

grid()

# USDCAD

plot(delta,(BV_means_usdcad[,1]-mean(BV_means_usdcad[,1]))/mean(BV_means_usdcad[,1]),
     ylab = expression("Mean BV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#296EB4",cex.axis=0.8, main = expression("USD/CAD"))
grid()

# USDJPY

plot(delta,(BV_means_usdjpy[,1]-mean(BV_means_usdjpy[,1]))/mean(BV_means_usdjpy[,1]),
     ylab = expression("Mean BV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#848C8E",cex.axis=0.8, main = expression("USD/JPY"))
grid()

mtext(expression(paste(Delta,"-signature plots for Bi-hourly Bipower Variation",sep = "")), outer = TRUE, cex = 1.5)

## Without outlier

# EURUSD

plot(delta[-1],(BV_means_eurusd[,1][-1]-mean(BV_means_eurusd[,1][-1]))/mean(BV_means_eurusd[,1][-1]),
     ylab = expression("Mean BV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#1B998B",cex.axis=0.8, main = expression("EUR/USD"))
grid()

# GBPUSD

plot(delta[-1],(BV_means_gbpusd[,1][-1]-mean(BV_means_gbpusd[,1][-1]))/mean(BV_means_gbpusd[,1][-1]),
     ylab = expression("Mean BV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#D1495B",cex.axis=0.8, main = expression("GBP/USD"))

grid()

# USDCAD

plot(delta[-1],(BV_means_usdcad[,1][-1]-mean(BV_means_usdcad[,1][-1]))/mean(BV_means_usdcad[,1][-1]),
     ylab = expression("Mean BV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#296EB4",cex.axis=0.8, main = expression("USD/CAD"))
grid()

# USDJPY

plot(delta[-1],(BV_means_usdjpy[,1][-1]-mean(BV_means_usdjpy[,1][-1]))/mean(BV_means_usdjpy[,1][-1]),
     ylab = expression("Mean BV (% change wrt overall pair mean)"), xlab = expression(paste(Delta," (in secs)",sep = " ")), 
     type = "o",lty=1, lwd=1,pch=19,col = "#848C8E",cex.axis=0.8, main = expression("USD/JPY"))
grid()

mtext(expression(paste(Delta,"-signature plots (without outlier) for Bipower Variation",sep = "")), outer = TRUE, cex = 1.5)

par(old.par)

# Saving clean squared volatility estimates

setwd("C:/Users/Alberto Saavedra/OneDrive/Documentos/MSc in Statistics/Research project/Programing/Roughness parameter/Clean volatility estimates/126 min")

Bi_Hourly_RV_2min_eurusd <- RV_Clean_eurusd[["delta_120_secs"]]
Bi_Hourly_RV_2min_gbpusd <- RV_Clean_gbpusd[["delta_120_secs"]]
Bi_Hourly_RV_2min_usdcad <- RV_Clean_usdcad[["delta_120_secs"]]
Bi_Hourly_RV_2min_usdjpy <- RV_Clean_usdjpy[["delta_120_secs"]]

save(Bi_Hourly_RV_2min_eurusd, file="Bi_Hourly_RV_2min_eurusd_clean.Rout")
save(Bi_Hourly_RV_2min_gbpusd, file="Bi_Hourly_RV_2min_gbpusd_clean.Rout")
save(Bi_Hourly_RV_2min_usdcad, file="Bi_Hourly_RV_2min_usdcad_clean.Rout")
save(Bi_Hourly_RV_2min_usdjpy, file="Bi_Hourly_RV_2min_usdjpy_clean.Rout")

Bi_Hourly_BV_2min_eurusd <- BV_Clean_eurusd[["delta_120_secs"]]
Bi_Hourly_BV_2min_gbpusd <- BV_Clean_gbpusd[["delta_120_secs"]]
Bi_Hourly_BV_2min_usdcad <- BV_Clean_usdcad[["delta_120_secs"]]
Bi_Hourly_BV_2min_usdjpy <- BV_Clean_usdjpy[["delta_120_secs"]]

save(Bi_Hourly_BV_2min_eurusd, file="Bi_Hourly_BV_2min_eurusd_clean.Rout")
save(Bi_Hourly_BV_2min_gbpusd, file="Bi_Hourly_BV_2min_gbpusd_clean.Rout")
save(Bi_Hourly_BV_2min_usdcad, file="Bi_Hourly_BV_2min_usdcad_clean.Rout")
save(Bi_Hourly_BV_2min_usdjpy, file="Bi_Hourly_BV_2min_usdjpy_clean.Rout")

setwd("C:/Users/Alberto Saavedra/OneDrive/Documentos/MSc in Statistics/Research project/Programing/IV Est by sec (no shifting)/Clean intraday volatility data tables (all deltas)/126 min")

RV_126min_all_deltas_Clean_eurusd <- RV_Clean_eurusd
BV_126min_all_deltas_Clean_eurusd <- BV_Clean_eurusd

save(RV_126min_all_deltas_Clean_eurusd, file="RV_126min_all_deltas_Clean_eurusd.Rout")
save(BV_126min_all_deltas_Clean_eurusd, file="BV_126min_all_deltas_Clean_eurusd.Rout")

RV_126min_all_deltas_Clean_gbpusd <- RV_Clean_gbpusd
BV_126min_all_deltas_Clean_gbpusd <- BV_Clean_gbpusd

save(RV_126min_all_deltas_Clean_gbpusd, file="RV_126min_all_deltas_Clean_gbpusd.Rout")
save(BV_126min_all_deltas_Clean_gbpusd, file="BV_126min_all_deltas_Clean_gbpusd.Rout")

RV_126min_all_deltas_Clean_usdcad <- RV_Clean_usdcad
BV_126min_all_deltas_Clean_usdcad <- BV_Clean_usdcad

save(RV_126min_all_deltas_Clean_usdcad, file="RV_126min_all_deltas_Clean_usdcad.Rout")
save(BV_126min_all_deltas_Clean_usdcad, file="BV_126min_all_deltas_Clean_usdcad.Rout")

RV_126min_all_deltas_Clean_usdjpy <- RV_Clean_usdjpy
BV_126min_all_deltas_Clean_usdjpy <- BV_Clean_usdjpy

save(RV_126min_all_deltas_Clean_usdjpy, file="RV_126min_all_deltas_Clean_usdjpy.Rout")
save(BV_126min_all_deltas_Clean_usdjpy, file="BV_126min_all_deltas_Clean_usdjpy.Rout")
