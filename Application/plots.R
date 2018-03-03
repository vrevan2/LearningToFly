#### Environment Setup ( Change This )####
setwd("D:\\Docs\\Documents\\Projects\\LearningToFly\\Application\\data") 

#### libraries  ####
library('lubridate')

#### Functions
adjustTime <- function (x) {
  formatC(x, width = 4, flag = "0")
}

toDateTime <- function (date, time, timezone) {
  result <- parse_date_time(paste(date, time), 
                            c("ymdHM", "%m-%d-%y %H%M", "%m/%d/%Y %H%M"), 
                            tz = timezone)
  return(result)
}

## Raw Data
df <- read.csv("On_Time_Performance_2017_IL.csv", header = FALSE)
colnames(df) <- as.character(read.table("OTP_Header.txt")[,1])
df <- df[sample(nrow(df), 10000), 1:64] #### REMOVE the sampling in this line later

## Airport Info
airports <- read.csv("airports.csv")
airports <- subset(airports, airports$IATA != "\\N")
rownames(airports) <- as.character(airports$IATA)

# Some time values should start with 0. e.g 328 should be 0328
df$CRSDepTime <- adjustTime(df$CRSDepTime)
df$CRSArrTime <- adjustTime(df$CRSArrTime)
df$DepTime <- adjustTime(df$DepTime)
df$ArrTime <- adjustTime(df$ArrTime)

df$DepDateTime <- mapply(toDateTime, df$FlightDate, df$DepTime, airports$Tz.database.time.zone[df$Origin])
df$ArrDateTime <- mapply(toDateTime, df$FlightDate, df$ArrTime, airports$Tz.database.time.zone[df$Dest])

# Departure Times

df$DepTime[nchar(df$DepTime)==3 & !is.na(df$DepTime)] <- paste0( "0",df$DepTime[nchar(df$DepTime)==3 & !is.na(df$DepTime)]) 
df$DepTime[nchar(df$DepTime)==2 & !is.na(df$DepTime)] <- paste0( "00",df$DepTime[nchar(df$DepTime)==2 & !is.na(df$DepTime)]) 
df$DepTime[nchar(df$DepTime)==1 & !is.na(df$DepTime)] <- paste0( "000",df$DepTime[nchar(df$DepTime)==1 & !is.na(df$DepTime)]) 

df$temp <- paste(df$FlightDate , df$DepTime)
df$DepDateTime <- strptime(df$temp, format="%m-%d-%Y %H%M")
df$DepDateTime[is.na(df$DepDateTime)] <- strptime(df$temp[is.na(df$DepDateTime)], format="%m/%d/%Y %H%M")

df$CRSDepHourofDay <- hour(ymd_hms(df$OriginDateTime))
df$CRSDepMonthofYear <- month(ymd_hms(df$OriginDateTime))
df$DepHourofDay <- hour(ymd_hms(df$DepDateTime))
df$DepMonthofYear <- month(ymd_hms(df$DepDateTime))
df$CRSArrHourofDay <- hour(ymd_hms(df$DestDateTime))
df$CRSArrMonthofYear <- month(ymd_hms(df$DestDateTime))
df$ArrHourofDay <- hour(ymd_hms(df$ArrDateTime))
df$ArrMonthofYear <- month(ymd_hms(df$ArrDateTime))


#### End of Global Variables Creation Section ####


#### New Variables/Columns Declared : (Add declared Variables here)

# df$DepDateTime, df$ArrDateTime -  Have NAs as the flights might be cancelled
# df$OriginDateTime, df$DestDateTime - Scheduled Times, Always present
# df$CRSArrHourofDay, ArrHourofDay , CRSDepHourofDay, DepHourofDay  - Range(0 - 23) , Non CRS ones have NA's due to same reason as above
# df$CRSArrMonthofYear, ArrMonthofYear, CRSDepMonthofYear,  CRSDepMonthofYear - Range(1,12) , Non CRS ones have NA's due to same reason as above 

####

#### Plot Names Declared #####
 

####





#### Plots ####

AirportName = "ORD"
d <- df[df$Origin == AirportName | df$Dest == AirportName,]
summary(d)

# Using aggregate according to the DayofMonth, HourofDay variables

#### End of Plots ####
