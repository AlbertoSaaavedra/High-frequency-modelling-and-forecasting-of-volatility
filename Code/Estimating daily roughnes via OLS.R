require(zoo)
require(tseries)
require(Hmisc)
require(MCS)
require(lubridate)
require(fasttime)
require(xtable)
# Daily estimates for roughness -------------------------------------------

setwd("C:/Users/Alberto Saavedra/OneDrive/Documentos/MSc in Statistics/Research project/Programing/Roughness parameter/Clean volatility estimates/Standardized/Daily")
load("Daily_BV_5min_eurusd_clean.Rout")
load("Daily_BV_5min_gbpusd_clean.Rout")
load("Daily_BV_5min_usdcad_clean.Rout")
load("Daily_BV_5min_usdjpy_clean.Rout")

# Estimating roughnes via OLS ---------------------------------------------

colors_names <- c("#247BA0","#FF1654","#FFCC6D","#70C1B3","#66BFFF","#B2DBBF")
bw <- 50
q_range <- c(0.5,1,1.5,2,3)

vrgm_eurusd <- matrix(data = NA, nrow = bw, ncol = length(q_range))
vrgm_gbpusd <- matrix(data = NA, nrow = bw, ncol = length(q_range))
vrgm_usdcad <- matrix(data = NA, nrow = bw, ncol = length(q_range))
vrgm_usdjpy <- matrix(data = NA, nrow = bw, ncol = length(q_range))
i <- 0
cols_names <- c()
for (q in q_range) {
  i <- i+1
  cols_names[i] <- paste("variogram_",q,sep = "")
  for (j in 1:bw) {
    vrgm_eurusd[j,i] <- mean(abs(Lag(log(sqrt(Daily_BV_5min_eurusd_clean$eurusd)),
                                     shift = j) - log(sqrt(Daily_BV_5min_eurusd_clean$eurusd)))^q,na.rm = TRUE)
    
    vrgm_gbpusd[j,i] <- mean(abs(Lag(log(sqrt(Daily_BV_5min_gbpusd_clean$gbpusd)),
                                     shift = j) - log(sqrt(Daily_BV_5min_gbpusd_clean$gbpusd)))^q,na.rm = TRUE)
    
    vrgm_usdcad[j,i] <- mean(abs(Lag(log(sqrt(Daily_BV_5min_usdcad_clean$usdcad)),
                                     shift = j) - log(sqrt(Daily_BV_5min_usdcad_clean$usdcad)))^q,na.rm = TRUE)
    
    vrgm_usdjpy[j,i] <- mean(abs(Lag(log(sqrt(Daily_BV_5min_usdjpy_clean$usdjpy)),
                                     shift = j) - log(sqrt(Daily_BV_5min_usdjpy_clean$usdjpy)))^q,na.rm = TRUE)
    
  }
}
colnames(vrgm_eurusd) <- cols_names
colnames(vrgm_gbpusd) <- cols_names
colnames(vrgm_usdcad) <- cols_names
colnames(vrgm_usdjpy) <- cols_names

test_eurusd <- data.frame(lag = 1:bw,vrgm_eurusd)
test_gbpusd <- data.frame(lag = 1:bw,vrgm_gbpusd)
test_usdcad <- data.frame(lag = 1:bw,vrgm_usdcad)
test_usdjpy <- data.frame(lag = 1:bw,vrgm_usdjpy)

ols_est_eurusd <- matrix(data = NA, nrow = (length(q_range)), ncol = 2)
ols_est_gbpusd <- matrix(data = NA, nrow = (length(q_range)), ncol = 2)
ols_est_usdcad <- matrix(data = NA, nrow = (length(q_range)), ncol = 2)
ols_est_usdjpy <- matrix(data = NA, nrow = (length(q_range)), ncol = 2)

for (q in 1:length(q_range)) {
  ols_est_eurusd[(q),] <- coef(lm(formula = log(test_eurusd[[(q+1)]]) ~ log(lag),data = test_eurusd))
  ols_est_gbpusd[(q),] <- coef(lm(formula = log(test_gbpusd[[(q+1)]]) ~ log(lag),data = test_gbpusd))
  ols_est_usdcad[(q),] <- coef(lm(formula = log(test_usdcad[[(q+1)]]) ~ log(lag),data = test_usdcad))
  ols_est_usdjpy[(q),] <- coef(lm(formula = log(test_usdjpy[[(q+1)]]) ~ log(lag),data = test_usdjpy))
}

alpha_eurusd <- (ols_est_eurusd[,2]-1)/2
alpha_gbpusd <- (ols_est_gbpusd[,2]-1)/2
alpha_usdcad <- (ols_est_usdcad[,2]-1)/2
alpha_usdjpy <- (ols_est_usdjpy[,2]-1)/2

H_eurusd <- alpha_eurusd + 0.5
H_gbpusd <- alpha_gbpusd + 0.5
H_usdcad <- alpha_usdcad + 0.5
H_usdjpy <- alpha_usdjpy + 0.5

par(mfrow=c(2,2),oma = c(0, 0, 2, 0), mai = c(0.7, 0.67, 0.3, 0.1))

plot(x = log(test_eurusd$lag), y = log(test_eurusd$variogram_0.5), type = "p", pch=16, cex=0.6,col=colors_names[1],
     ylim = c(min(log(test_eurusd[,2:length(q_range)]))-1,max(log(test_eurusd[,2:length(q_range)]))), 
     xlab = expression("log(h)"), ylab = expression(paste("log m(q, h)")), main = expression("EUR/USD"))
abline(a = ols_est_eurusd[1,1], b = ols_est_eurusd[1,2], col = "red")

for (q in 2:length(q_range)) {
  points(log(test_eurusd$lag),log(test_eurusd[[(q+1)]]), pch=16, cex=0.6,col=colors_names[q])
  abline(a = ols_est_eurusd[q,1], b = ols_est_eurusd[q,2], col = "red")
}

plot(x = log(test_gbpusd$lag), y = log(test_gbpusd$variogram_0.5), type = "p", pch=16, cex=0.6,col=colors_names[1],
     ylim = c(min(log(test_gbpusd[,2:length(q_range)]))-1,max(log(test_gbpusd[,2:length(q_range)]))), 
     xlab = expression("log(h)"), ylab = expression(paste("log m(q, h)")), main = expression("GBP/USD"))
abline(a = ols_est_gbpusd[1,1], b = ols_est_gbpusd[1,2], col = "red")

for (q in 2:length(q_range)) {
  points(log(test_gbpusd$lag),log(test_gbpusd[[(q+1)]]), pch=16, cex=0.6,col=colors_names[q])
  abline(a = ols_est_gbpusd[q,1], b = ols_est_gbpusd[q,2], col = "red")
}

plot(x = log(test_usdcad$lag), y = log(test_usdcad$variogram_0.5), type = "p", pch=16, cex=0.6,col=colors_names[1],
     ylim = c(min(log(test_usdcad[,2:length(q_range)]))-1,max(log(test_usdcad[,2:length(q_range)]))), 
     xlab = expression("log(h)"), ylab = expression(paste("log m(q, h)")), main = expression("USD/CAD"))
abline(a = ols_est_usdcad[1,1], b = ols_est_usdcad[1,2], col = "red")

for (q in 2:length(q_range)) {
  points(log(test_usdcad$lag),log(test_usdcad[[(q+1)]]), pch=16, cex=0.6,col=colors_names[q])
  abline(a = ols_est_usdcad[q,1], b = ols_est_usdcad[q,2], col = "red")
}

plot(x = log(test_usdjpy$lag), y = log(test_usdjpy$variogram_0.5), type = "p", pch=16, cex=0.6,col=colors_names[1],
     ylim = c(min(log(test_usdjpy[,2:length(q_range)]))-1,max(log(test_usdjpy[,2:length(q_range)]))), 
     xlab = expression("log(h)"), ylab = expression(paste("log m(q, h)")), main = expression("USD/JPY"))
abline(a = ols_est_usdjpy[1,1], b = ols_est_usdjpy[1,2], col = "red")

for (q in 2:length(q_range)) {
  points(log(test_usdjpy$lag),log(test_usdjpy[[(q+1)]]), pch=16, cex=0.6,col=colors_names[q])
  abline(a = ols_est_usdjpy[q,1], b = ols_est_usdjpy[q,2], col = "red")
}

legend("bottomright", legend = paste("q = ",q_range,sep = ""),col = colors_names[1:6], pch = 16, bty = "n", cex = 0.75)

mtext(expression(paste("Scaling property of moments of daily log-volatility",sep = "")), outer = TRUE, cex = 1.5)


alpha_eurusd <- ols_est_eurusd[,2]/q_range - 0.5
alpha_gbpusd <- ols_est_gbpusd[,2]/q_range - 0.5
alpha_usdcad <- ols_est_usdcad[,2]/q_range - 0.5
alpha_usdjpy <- ols_est_usdjpy[,2]/q_range - 0.5

alpha_table <- t(data.frame(alpha_eurusd,alpha_gbpusd,alpha_usdcad,alpha_usdjpy))
row.names(alpha_table) <- c("EUR/USD","GBP/USD","USD/CAD","USD/JPY")
colnames(alpha_table) <- c("q = 0.5","q = 1","q = 1.5","q = 2","q = 3")

xtable(alpha_table)
