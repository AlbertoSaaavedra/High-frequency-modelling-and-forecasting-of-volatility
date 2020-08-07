# Libraries ---------------------------------------------------------------

require(lubridate)
require(data.table)
require(Hmisc)
require(plyr)
require(fasttime)
require(caTools)

# Reading data

setwd("C:/Users/Alberto Saavedra/OneDrive/Documentos/MSc in Statistics/Research project/Programing/IV Est by sec (no shifting)/Daily (21h) RV and BV data tables")

# Delta to compute RV and BV (in seconds)
delta_prev <- seq(180,600,by = 30)
delta <- seq(0,600,by = 15)
delta[1] <- 1
delta <- delta[!(delta %in% delta_prev)]

dir_eurusd <- "D:/MSc in Statistical Finance/Research project/Data/TrueFX/EURUSD/EURUSD-CSV-by-second-(no shift and 21h days and 1st obs clean)"
dir_gbpusd <- "D:/MSc in Statistical Finance/Research project/Data/TrueFX/GBPUSD/GBPUSD-CSV-by-second-(no shift and 21h days and 1st obs clean)"
dir_usdcad <- "D:/MSc in Statistical Finance/Research project/Data/TrueFX/USDCAD/USDCAD-CSV-by-second-(no shift and 21h days and 1st obs clean)"
dir_usdjpy <- "D:/MSc in Statistical Finance/Research project/Data/TrueFX/USDJPY/USDJPY-CSV-by-second-(no shift and 21h days and 1st obs clean)"

dirs <- c(dir_eurusd,dir_gbpusd,dir_usdcad,dir_usdjpy)

files_eurusd <- list.files(dir_eurusd)
files_gbpusd <- list.files(dir_gbpusd)
files_usdcad <- list.files(dir_usdcad)
files_usdjpy <- list.files(dir_usdjpy)

files <- cbind(files_eurusd, files_gbpusd, files_usdcad, files_usdjpy)

result_rv <- list()
result_bv <- list()
RV <- list()
BV <- list()
g <- 0
pb = txtProgressBar(min = 0, max = length(delta), initial = 0)

# Total number of hours in the trading day under consideration
nhours <- 21
# Time spectrum, in minutes, over which integrated variance will be estimated
delta_iv <- 60

system.time({
  for (d in delta) {
    g <- g + 1
    for (i in 1:120) {
      # Reading the prices for 4 currencies
      for (j in 1:4) {
        aux <- fread(paste(dirs[j],"/",
                           files[i,j],sep = ""))
        if (j==1) {
          # Computing number of days in the month at hand
          ndays <- dim(aux)[1]/(nhours*60*60)
          # Dividing the total number of minutes in one trading day into the number of periods over which
          # we will estimate integrated variance
          iv_periods_per_day <- (nhours*60)/delta_iv
          # Defining the number of seconds whithin each period over which we will estimate integrated variance
          nsecs_per_iv_period <- delta_iv*60
          # Creating matrix to store log-prices of 4 ccys. Each row is
          # a second and each column a ccy
          prices <- matrix(data = NA, ncol = 4*ndays*iv_periods_per_day, nrow = nsecs_per_iv_period) 
        }
        # Indexes (corresponding to price measurements each second) of the star of each period over which we will
        # estimate integrated variance
        p_inde0 <- seq(1,by = nsecs_per_iv_period,length.out = iv_periods_per_day*ndays)
        # Generating sequences of the indexes in aux from the start to the end of each hour
        # info <- data.frame(start=p_inde0, len=rep(x = (60*60),length(p_inde0)))
        # p_inde <- sequence(info$len) + rep(info$start-1, info$len)
        # Filling all columns (hours) at once with the log-prices
        prices[,(ndays*iv_periods_per_day*(j-1)+1):(j*ndays*iv_periods_per_day)] <- aux[,log_midprice]
      }
      # Indexes to compute returns from prices
      inde <- seq(d,nsecs_per_iv_period,by = d)
      # Including extra index for first return
      if (head(inde,1) != 1) {
        inde <- c(1,inde)
      }
      # Auxiliar variables to store RV and BV for each currency on each day
      rv <- 0
      bv <- 0
      # Vector to store hourly RV estimates
      rv_prev <- c()
      bv_prev <- c()
      for (j in 1:ncol(prices)) {
        # Sum of squares of: price[inde+1] - price[inde]. The sum is for the whole column
        # thus we obtain daily estimates. TO GO INTRADAY, WE ONLY NEED MORE COLUMNS! AS
        # MANY AS INTERVALS TO COMPUTE RV and BV. The columns should be added in the prices
        # Matrix: instead of 1 per day to 1 per period of interest (eg 1 hour)
        returns <- (prices[inde,j][-1] - prices[inde,j][-length(prices[inde,j])])
        N <- length(returns)
        rv <- sum(returns^2)
        bv <- (N/(N-1)) * (pi/2) * sum(abs(returns) * abs(Lag(returns)),na.rm = TRUE)
        rv_prev <- c(rv_prev,rv)
        bv_prev <- c(bv_prev,bv)
      }
      # Indexes to separate the estimates of RV of each ccy: as many periods over which integrated
      # variance was estimated
      starts <- (ndays*iv_periods_per_day*(seq(1,4,length.out = 4)-1)+1)
      ends <- (1:4*ndays*iv_periods_per_day)
      # Storing monthly results for RV
      result_rv_prev <- data.table(
        date = aux[p_inde0,1],
        eurusd = rv_prev[starts[1]:ends[1]],
        gbpusd = rv_prev[starts[2]:ends[2]],
        usdcad = rv_prev[starts[3]:ends[3]],
        usdjpy = rv_prev[starts[4]:ends[4]]
      )
      # Storing monthly results for BV
      result_bv_prev <- data.table(
        date = aux[p_inde0,1],
        eurusd = bv_prev[starts[1]:ends[1]],
        gbpusd = bv_prev[starts[2]:ends[2]],
        usdcad = bv_prev[starts[3]:ends[3]],
        usdjpy = bv_prev[starts[4]:ends[4]]
      )
      # Storing the result of all the months for the current delta
      result_rv <- rbind(result_rv,result_rv_prev)
      result_bv <- rbind(result_bv,result_bv_prev)
    }
    # Storing the results for all the months for all the deltas
    id <- paste("delta_",d,"_secs",sep = "")
    RV[[id]] <- result_rv
    BV[[id]] <- result_bv
    # Re-starting variables to store results of all the months for each delta
    result_rv <- list()
    result_bv <- list()
    setTxtProgressBar(pb,g)
  }
})

RV_Est_1h_15_deltas <- RV
BV_Est_1h_15_deltas <- BV

RV_Est_1h_5min <- RV
BV_Est_1h_5min <- BV

setwd("C:/Users/Alberto Saavedra/OneDrive/Documentos/MSc in Statistics/Research project/Programing/IV Est by sec (no shifting)/Daily (21h) RV and BV data tables")

save(RV_Est_1h_15_deltas, file="RV_Est_1h_15_deltas.Rout")
save(BV_Est_1h_15_deltas, file="BV_Est_1h_15_deltas.Rout")

