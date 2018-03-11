library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyr)
library(leaflet)
library(jpeg)
library(grid)

library(lubridate)
library(reshape2)
library(ggplot2)
library(DT)

#### Globals
defaultTz <- "America/Chicago"

#### Functions
adjustTime <- function(x) {
  formatC(x, width = 4, flag = "0")
}

toDateTime <- function(date, time, timezone) {
  if(is.na(time) || time == '') {
    return(NA)
  }
  
  result <- parse_date_time(paste(date, time), 
                            c("ymdHM", "%m-%d-%y %H%M", "%m/%d/%Y %H%M"), 
                            tz = timezone)
  result <- with_tz(result, tzone = defaultTz)
  return(result)
}

## Raw Data
df <- read.csv("data/On_Time_Performance_2017_IL_sample.csv", header = FALSE)
#df <- read.csv("data/On_Time_Performance_2017_IL.csv", header = FALSE)

colnames(df) <- as.character(read.table("data/OTP_Header.txt")[,1])

#df<- df[df$Origin == "ORD" | df$Origin == "MDW" | df$Dest == "ORD" | df$Dest == "MDW",]


# df <- df[sample(nrow(df), 1000), 1:64] #### REMOVE the sampling in this line later
#df <- df[, 1:64] #### REMOVE the sampling in this line later

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

#Airlines Lookup
airlines <- read.csv("data/airlines.csv")

month = 1
Airport1 = 'ORD'
Airport2 = 'MDW'

#### End of Global Variables Creation Section ####


#### New Variables/Columns Declared : (Add declared Variables here)

# dataset$DepDateTime, dataset$ArrDateTime -  Have NAs as the flights might be cancelled
# dataset$OriginDateTime, dataset$DestDateTime - Scheduled Times, Always present
# dataset$CRSArrHourofDay, ArrHourofDay , CRSDepHourofDay, DepHourofDay  - Range(0 - 23) , Non CRS ones have NA's due to same reason as above
# dataset$CRSArrMonthofYear, ArrMonthofYear, CRSDepMonthofYear,  CRSDepMonthofYear - Range(1,12) , Non CRS ones have NA's due to same reason as above 

####

######## Grade B ########

library('plyr')
library(plotly)
library(gapminder)



### Airlines Arrival and Departure every Month
arrivalAPort1 <- data.frame(subset(df,  Dest == Airport1))
departureAPort1 <- data.frame(subset(df,  Origin == Airport1))



noArrAport1 <-count(arrivalAPort1 ,  c('ArrMonthofYear' , 'AirlineID'))
noDepAport1 <-count(departureAPort1 ,  c('DepMonthofYear' , 'AirlineID'))
colnames(noArrAport1)<-c("Month", "AirlineID" , "Freq" )
colnames(noDepAport1)<-c("Month", "AirlineID" , "Freq" )
APort1 <- merge(noArrAport1, noDepAport1, by = c("AirlineID","Month"))
APort1 <-merge(APort1,airlines, by = "AirlineID")


APort1 <- na.omit(APort1)
p <- gapminder %>%
  plot_ly(
    x = APort1$AirlineCode, 
    y = APort1$Freq.x, 
    frame = APort1$Month, 
    type = 'bar'  ) %>% 
    add_trace(y = APort1$Freq.y ) %>%
  layout( barmode = 'stack')

p

### Number of Arrivals and Departures Hour of Day


noArrAport1 <-count(arrivalAPort1 ,  c('ArrMonthofYear' , 'ArrHourofDay'))
noDepAport1 <-count(departureAPort1 ,  c('DepMonthofYear' , 'DepHourofDay'))
colnames(noArrAport1)<-c("Month", "Hour" , "Freq" )
colnames(noDepAport1)<-c("Month", "Hour" , "Freq" )

APort1 <- merge(noArrAport1, noDepAport1, by = c("Month" , "Hour"))


APort1 <- na.omit(APort1)
p <- gapminder %>%
  plot_ly(
    x = APort1$Hour, 
    y = APort1$Freq.x, 
    frame = APort1$Month, 
    type = 'bar'  ) %>% 
    add_trace(y = APort1$Freq.y ) %>%
  layout( barmode = 'stack')

p

#### 15 Common Destination Number over Month 


top15Dest <-count(departureAPort1 ,  c('Dest'))
top15Dest <-top15Dest[order(-top15Dest$freq) , ]
top15Dest <- head(top15Dest , 15)
colnames(top15Dest)<-c('Dest','Total')
top15Dest
top15DestData <- merge(departureAPort1, top15Dest , by = "Dest" , all.x = FALSE)

top15Dest <- count(top15DestData ,  c('Dest' , 'DepMonthofYear'))
colnames(top15Dest)<-c('Dest', 'Month' , 'Freq')

APort1 <- na.omit(top15Dest)
p <- gapminder %>%
  plot_ly(
    x = APort1$Dest, 
    y = APort1$Freq, 
    frame = APort1$Month, 
    type = 'bar'  )

p

### Type and Number of Delays for every Month

delaysdata  <- data.frame(subset(df,  Dest == Airport1 | Origin == Airport1))

delays_NAS <- delaysdata[delaysdata$NASDelay!=0 & !is.na(delaysdata$NASDelay),]
delays_NAS <- count(delays_NAS , c('DepMonthofYear'))
colnames(delays_NAS)<-c("Month" , "NAS")

delays_Security <- delaysdata[delaysdata$SecurityDelay!=0 & !is.na(delaysdata$SecurityDelay),]
delays_Security <- count(delays_Security , c('DepMonthofYear'))
colnames(delays_Security)<-c("Month" , "Security")

delays_Weather <- delaysdata[delaysdata$WeatherDelay!=0 & !is.na(delaysdata$WeatherDelay),]
delays_Weather <- count(delays_Weather , c('DepMonthofYear'))
colnames(delays_Weather)<-c("Month" , "Weather")

delays_Carrier <- delaysdata[delaysdata$CarrierDelay!=0 & !is.na(delaysdata$CarrierDelay),]
delays_Carrier <- count(delays_Carrier , c('DepMonthofYear'))
colnames(delays_Carrier)<-c("Month" , "Carrier")


delays_LateAircraftDelay <- delaysdata[delaysdata$LateAircraftDelay!=0 & !is.na(delaysdata$LateAircraftDelay),]
delays_LateAircraftDelay <- count(delays_LateAircraftDelay , c('DepMonthofYear'))
colnames(delays_LateAircraftDelay)<-c("Month" , "LAD")


delays<-  Reduce(function(x, y) merge(x, y, all=TRUE , by="Month"), list(delays_Carrier ,delays_LateAircraftDelay , delays_NAS, delays_Security ,delays_Weather))


delays[is.na(delays)] <- 0

p <- 
  plot_ly(
    x = delays$Month, 
    y = delays$NAS, 
    type = 'bar'  ) %>% 
  add_trace(y = delays$Security ) %>% 
  add_trace(y = delays$Carrier ) %>% 
  add_trace(y = delays$LAD ) %>% 
  add_trace(y = delays$Weather )  %>% 
   layout( barmode = 'stack')



p

######