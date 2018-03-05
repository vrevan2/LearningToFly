library('lubridate')

#### Globals
defaultTz <- "America/Chicago"

#### Functions
adjustTime <- function (x) {
  formatC(x, width = 4, flag = "0")
}

toDateTime <- function (date, time, timezone) {
  result <- parse_date_time(paste(date, time), 
                            c("ymdHM", "%m-%d-%y %H%M", "%m/%d/%Y %H%M"), 
                            tz = timezone)
  result <- with_tz(result, tzone = defaultTz)
  return(result)
}

## Raw Data
df <- read.csv("data/On_Time_Performance_2017_IL.csv", header = FALSE)
colnames(df) <- as.character(read.table("data/OTP_Header.txt")[,1])
df <- df[sample(nrow(df), 1000), 1:64] #### REMOVE the sampling in this line later

## Airport Info
airports <- read.csv("data/airports_stations.csv")
rownames(airports) <- as.character(airports$IATA)

# Some time values should start with 0. e.g 328 should be 0328
df$CRSDepTime <- adjustTime(df$CRSDepTime)
df$CRSArrTime <- adjustTime(df$CRSArrTime)
df$DepTime <- adjustTime(df$DepTime)
df$ArrTime <- adjustTime(df$ArrTime)

# Get Date Time Objects for scheduled and actual Departure and Arrivals
df$CRSDepDateTime <- as_datetime(mapply(toDateTime, df$FlightDate, df$CRSDepTime, airports$AirportTimezone[df$Origin]), tz = defaultTz)
df$CRSArrDateTime <- as_datetime(mapply(toDateTime, df$FlightDate, df$CRSArrTime, airports$AirportTimezone[df$Dest]), tz = defaultTz)
df$DepDateTime <- as_datetime(mapply(toDateTime, df$FlightDate, df$DepTime, airports$AirportTimezone[df$Origin]), tz = defaultTz)
df$ArrDateTime <- as_datetime(mapply(toDateTime, df$FlightDate, df$ArrTime, airports$AirportTimezone[df$Dest]), tz = defaultTz)

# Bins
df$CRSDepHourofDay <- hour(df$CRSDepDateTime)
df$CRSDepMonthofYear <- month(df$CRSDepDateTime)
df$CRSArrHourofDay <- hour(df$CRSArrDateTime)
df$CRSArrMonthofYear <- month(df$CRSArrDateTime)
df$DepHourofDay <- hour(df$DepDateTime)
df$DepMonthofYear <- month(df$DepDateTime)
df$ArrHourofDay <- hour(df$ArrDateTime)
df$ArrMonthofYear <- month(df$ArrDateTime)

#### Plots ####

AirportName <- "ORD"
d <- df[df$Origin == AirportName | df$Dest == AirportName,]
summary(d)

# Using aggregate according to the DayofMonth, HourofDay variables

#### End of Plots ####
