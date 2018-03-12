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

## Raw Data
df <- read.csv("data/OTP_2017.csv")

## Airport Info
airports <- read.csv("data/airports_stations.csv")
rownames(airports) <- as.character(airports$IATA)

#Airlines Lookup
airlines <- read.csv("data/airlines.csv")
Airport2 = 'MDW'
Airport1 = 'ORD'
#### End of Global Variables Creation Section ####


#### New Variables/Columns Declared : (Add declared Variables here)

####

######## Grade B ########

library('plyr')
library(plotly)
library(gapminder)



### Airlines Arrival and Departure every Month
arrivalAPort1 <- data.frame(subset(df,  Dest == Airport1))
departureAPort1 <- data.frame(subset(df,  Origin == Airport1))



noArrAport1 <-count(arrivalAPort1 ,  c('Month' , 'AirlineID'))
noDepAport1 <-count(departureAPort1 ,  c('Month' , 'AirlineID'))
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


noArrAport1 <-count(arrivalAPort1 ,  c('Month' , 'ArrHour'))
noDepAport1 <-count(departureAPort1 ,  c('Month' , 'DepHour'))
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

top15Dest <- count(top15DestData ,  c('Dest' , 'Month'))
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
delays_NAS <- count(delays_NAS , c('Month'))
colnames(delays_NAS)<-c("Month" , "NAS")

delays_Security <- delaysdata[delaysdata$SecurityDelay!=0 & !is.na(delaysdata$SecurityDelay),]
delays_Security <- count(delays_Security , c('Month'))
colnames(delays_Security)<-c("Month" , "Security")

delays_Weather <- delaysdata[delaysdata$WeatherDelay!=0 & !is.na(delaysdata$WeatherDelay),]
delays_Weather <- count(delays_Weather , c('Month'))
colnames(delays_Weather)<-c("Month" , "Weather")

delays_Carrier <- delaysdata[delaysdata$CarrierDelay!=0 & !is.na(delaysdata$CarrierDelay),]
delays_Carrier <- count(delays_Carrier , c('Month'))
colnames(delays_Carrier)<-c("Month" , "Carrier")


delays_LateAircraftDelay <- delaysdata[delaysdata$LateAircraftDelay!=0 & !is.na(delaysdata$LateAircraftDelay),]
delays_LateAircraftDelay <- count(delays_LateAircraftDelay , c('Month'))
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