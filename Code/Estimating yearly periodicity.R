# Libraries ---------------------------------------------------------------

require(lubridate)
require(data.table)
require(Hmisc)
require(plyr)
require(fasttime)
require(caTools)
require(xts)

# Computing intraweek periodicity -----------------------------------------

# Information needed to eliminate days with incomplete information from estimates

# Loading the data of a full year

setwd("D:/MSc in Statistical Finance/Research project/Data/TrueFX/EURUSD/EURUSD-CSV-by-second-(no shift and 21h days and 1st obs clean)")
dates <- list.files("D:/MSc in Statistical Finance/Research project/Data/TrueFX/EURUSD/EURUSD-CSV-by-second-(no shift and 21h days and 1st obs clean)")

# Loading lists of days to exclude for each currency (due to have incomplete information)
setwd("C:/Users/Alberto Saavedra/OneDrive/Documentos/MSc in Statistics/Research project/Programing/IV Est by sec (no shifting)/Days out per currency data tables")
load("wday_out_eurusd.Rout")

days_out_inc_info_eurusd <- as.Date(wday_out_eurusd$first_ts_in_day)
days_out_inc_info_eurusd <- as.data.table(days_out_inc_info_eurusd)
colnames(days_out_inc_info_eurusd) <- "full_days_out_eurusd"

# Completely missing days 

setwd("C:/Users/Alberto Saavedra/OneDrive/Documentos/MSc in Statistics/Research project/Programing/IV Est by sec (no shifting)/Days with no information data tables")
load("full_days_out_eurusd.Rout")
full_days_out_eurusd <- rbind(full_days_out_eurusd,days_out_inc_info_eurusd)

# Daily volatility estimates for the current currency

setwd("C:/Users/Alberto Saavedra/OneDrive/Documentos/MSc in Statistics/Research project/Programing/Roughness parameter/Clean volatility estimates/Daily")

load("Daily_BV_5min_eurusd.Rout")

# Initializing variable to store yearly information
test_files <- list()
per_eurusd <- list()

setwd("D:/MSc in Statistical Finance/Research project/Data/TrueFX/EURUSD/EURUSD-CSV-by-second-(no shift and 21h days and 1st obs clean)")

for (j in 1:10){
  # Initializing variable to store yearly information
  test_files <- list()
  start_of_year <- (12*(j-1)+1)
  end_of_year <- (12*j)
  for (k in start_of_year:end_of_year) {
    # Reading files of the corresponding year
    current_file <- fread(dates[k])
    test_files <- rbind(test_files,current_file)
  }
  # Converting dates to Posixct
  test_files$by_delta <- fastPOSIXct(test_files$by_delta,tz = "GMT")
  start_of_id2 <- paste(month(head(test_files$by_delta,1)),"/",year(head(test_files$by_delta,1)), sep = "")
  end_of_id2 <- paste(month(tail(test_files$by_delta,1)),"/",year(tail(test_files$by_delta,1)), sep = "")
  ### Eliminating prices on days with incomplete information
  
  # Eliminating days with no information
  test_files <- subset(x = test_files,
                       subset = !(as.Date(by_delta)  %in% full_days_out_eurusd$full_days_out_eurusd),
                       select = c(by_delta,log_midprice))
  # Eliminating Chrsitmas
  test_files <- subset(x = test_files,
                       subset = !(month(by_delta) == 12 & day(by_delta) == 25))
  # Eliminating last day of the year
  test_files <- subset(x = test_files,
                       subset = !(month(by_delta) == 12 & day(by_delta) == 31))
  # Eliminating first day of the year
  test_files <- subset(x = test_files,
                       subset = !(month(by_delta) == 1 & day(by_delta) == 1))
  per_aux2 <- list()
  for (f in c(15,30,60,126)){
    if (f==126) {
      per_aux <- intraday_periodicity(price_ts = test_files, 
                                      daily_vol_ts = Daily_BV_5min_eurusd_clean, 
                                      per_freq = f, P_c = 4, P_s = 3)
    }
    else{
      per_aux <- intraday_periodicity(price_ts = test_files, 
                                      daily_vol_ts = Daily_BV_5min_eurusd_clean, 
                                      per_freq = f)
    }
    id <- paste("freq_",f,"_mins",sep = "")
    per_aux2[[id]] <- per_aux
  }
  id2 <- paste("from_",start_of_id2,"_to_",end_of_id2,sep = "")
  per_eurusd[[id2]] <- per_aux2
}


setwd("C:/Users/Alberto Saavedra/OneDrive/Documentos/MSc in Statistics/Research project/Programing/Roughness parameter/Periodicity estimates")

save(per_eurusd, file = "per_eurusd.Rout")
