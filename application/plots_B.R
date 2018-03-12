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
Airport2 = 'MDW' ## User Selectable Variable
Airport1 = 'ORD' ## User Selectable Variable
dow_i = '' ## User Selectable Variable
date = ''## User Selectable Variable
delay_type = '' ## User Selectable Variable
AID = 0 ## User Selectable Variable
#### End of Global Variables Creation Section ####


#### New Variables/Columns Declared : (Add declared Variables here)

####

######## Grade B ########

library('plyr')
library(plotly)
library(gapminder)



### Airlines Arrival and Departure every Month
arrivalAPort1 <- subset(df,  Dest == Airport1)
departureAPort1 <- subset(df,  Origin == Airport1)



noArrAport1 <-count(arrivalAPort1 ,  c('Month' , 'AirlineID'))
noDepAport1 <-count(departureAPort1 ,  c('Month' , 'AirlineID'))
colnames(noArrAport1)<-c("Month", "AirlineID" , "Freq" )
colnames(noDepAport1)<-c("Month", "AirlineID" , "Freq" )
APort1 <- merge(noArrAport1, noDepAport1, by = c("AirlineID","Month") , all.x=TRUE , all.y =TRUE)
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

APort1 <- merge(noArrAport1, noDepAport1, by = c("Month" , "Hour") ,   all.x=TRUE , all.y =TRUE )



APort1[is.na(APort1)]<-0

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
top15DestData <- merge(departureAPort1, top15Dest , by = "Dest" , all = FALSE)

top15Dest <- count(top15DestData ,  c('Dest' , 'Month'))
colnames(top15Dest)<-c('Dest', 'Month' , 'Freq')

top15Dest<-na.omit(top15Dest)

p <- gapminder %>%
  plot_ly(
    x = top15Dest$Dest, 
    y = top15Dest$Freq, 
    frame = top15Dest$Month, 
    type = 'bar'  )

p

### Type and Number of Delays for every Month

delaysdata  <- subset(df,  Dest == Airport1 | Origin == Airport1)

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

###### Plots A ######

### Top 50 Lists ###
top50pairs <-count(arrivalAPort1 , c('Origin'))

top50 <- top50pairs[order(-top50pairs$freq),]

top50pairs <- head(top50, 50)

top50aiports<-unique(as.character(top50pairs$Origin))

top50pairs <-count(departureAPort1, c('Dest'))

top50 <- top50pairs[order(-top50pairs$freq),]

top50pairs <- head(top50, 50)

top50aiports<-c( top50aiports , as.character(top50pairs$Dest))

Airport2 ="BOS" ## User Selectable Variable


pair_dep <-  subset( arrivalAPort1 , Origin ==Airport2)
pair_arr <- subset( departureAPort1,  Dest == Airport2)

noArrAport1 <-count(pair_arr ,  c('Month' , 'ArrHour'))
noDepAport1 <-count(pair_dep ,  c('Month' , 'DepHour'))
colnames(noArrAport1)<-c("Month", "Hour" , "Freq" )
colnames(noDepAport1)<-c("Month", "Hour" , "Freq" )

APort1 <- merge(noArrAport1, noDepAport1, by = c("Month" , "Hour") , all = TRUE)


APort1[is.na(APort1)]<-0

p <- gapminder %>%
  plot_ly(
    x = APort1$Hour, 
    y = APort1$Freq.x, 
    frame = APort1$Month, 
    type = 'bar'  ) %>% 
    add_trace(y = APort1$Freq.y ) %>%
  layout( barmode = 'stack')

p



## Airline number of departures hour month
AID =  19790  ## User Selectable Variable

airline_arr <- subset(arrivalAPort1 , AirlineID == AID)
airline_dep <- subset(departureAPort1 , AirlineID == AID)


noArrAport1 <-count(airline_arr ,  c('Month' , 'ArrHour'))
noDepAport1 <-count(airline_dep ,  c('Month' , 'DepHour'))
colnames(noArrAport1)<-c("Month", "Hour" , "Freq" )
colnames(noDepAport1)<-c("Month", "Hour" , "Freq" )

APort1 <- merge(noArrAport1, noDepAport1, by = c("Month" , "Hour") ,   all.x=TRUE , all.y =TRUE )



APort1[is.na(APort1)]<-0

p <- gapminder %>%
  plot_ly(
    x = APort1$Hour, 
    y = APort1$Freq.x, 
    frame = APort1$Month, 
    type = 'bar'  ) %>% 
    add_trace(y = APort1$Freq.y ) %>%
  layout( barmode = 'stack')

p

### allow a user to pick a date in 2017 for more detail on the 24 hour breakdown of that day 
### (how many departures and arrivals per hour, how many delays per hour)

date = ymd(as.Date("2017-12-25")) ## User Selectable Variable
day_i = day(date)
month_i = month(date)


user_arr <-subset(arrivalAPort1 , DayofMonth== day_i &Month == month_i)
user_dep <- subset(departureAPort1,  DayofMonth== day_i &Month == month_i)
noArrAport1 <-count(user_arr ,  c('ArrHour'))
noDepAport1 <-count(user_dep ,  c( 'DepHour'))
colnames(noArrAport1)<-c("Hour" , "Freq" )
colnames(noDepAport1)<-c("Hour" , "Freq" )

APort1 <- merge(noArrAport1, noDepAport1, by = c("Hour") ,   all.x=TRUE , all.y =TRUE )



APort1[is.na(APort1)]<-0

p <- 
  plot_ly(
    x = APort1$Hour, 
    y = APort1$Freq.x,
    type = 'bar'  ) %>% 
    add_trace(y = APort1$Freq.y ) %>%
  layout( barmode = 'stack')

p


### allow a user to pick a type of delay 

###for more info on how it changes over the 24 hours of the day and the 12 months of the year

delay_type = "NAS"  ## User Selectable Variable

delaysdata  <- subset(df,  Dest == Airport1 | Origin == Airport1)

delays_NAS <- delaysdata[delaysdata$NASDelay!=0 & !is.na(delaysdata$NASDelay),]
delays_NAS <- count(delays_NAS , c('Month' , 'ArrHour'))
colnames(delays_NAS)<-c('Month' , 'ArrHour', "NAS")

delays_Security <- delaysdata[delaysdata$SecurityDelay!=0 & !is.na(delaysdata$SecurityDelay),]
delays_Security <- count(delays_Security , c('Month' , 'ArrHour'))
colnames(delays_Security)<-c('Month' , 'ArrHour', "Security")

delays_Weather <- delaysdata[delaysdata$WeatherDelay!=0 & !is.na(delaysdata$WeatherDelay),]
delays_Weather <- count(delays_Weather , c('Month' , 'ArrHour'))
colnames(delays_Weather)<-c('Month' , 'ArrHour', "Weather")

delays_Carrier <- delaysdata[delaysdata$CarrierDelay!=0 & !is.na(delaysdata$CarrierDelay),]
delays_Carrier <- count(delays_Carrier , c('Month' , 'ArrHour'))
colnames(delays_Carrier)<-c('Month' , 'ArrHour', "Carrier")


delays_LateAircraftDelay <- delaysdata[delaysdata$LateAircraftDelay!=0 & !is.na(delaysdata$LateAircraftDelay),]
delays_LateAircraftDelay <- count(delays_LateAircraftDelay , c('Month' , 'ArrHour'))
colnames(delays_LateAircraftDelay)<-c('Month' , 'ArrHour', "LAD")


delays<-  Reduce(function(x, y) merge(x, y, all=TRUE , by=c('Month' , 'ArrHour')), list(delays_Carrier ,delays_LateAircraftDelay , delays_NAS, delays_Security ,delays_Weather))


delays[is.na(delays)] <- 0

p <- gapminder %>%
  plot_ly(
    x = delays$ArrHour, 
    y = delays[delay_type][,0:1], 
    frame = delays$Month, 
    type = 'bar'  )
p


#allow a user to pick a day of the week for more detail on the 
#24 hour breakdown of that day of the week across the year (how many departures and arrivals per hour,
# how many delays per hour)


dow_i = 1 ## User Selectable Variable


user_arr <-subset(arrivalAPort1 , DayOfWeek== dow_i )
user_dep <- subset(departureAPort1,  DayOfWeek== dow_i )


noArrAport1 <-count(user_arr ,  c('Month' , 'ArrHour'))
noDepAport1 <-count(user_dep ,  c('Month' , 'DepHour'))
colnames(noArrAport1)<-c("Month", "Hour" , "Freq" )
colnames(noDepAport1)<-c("Month", "Hour" , "Freq" )

APort1 <- merge(noArrAport1, noDepAport1, by = c("Month" , "Hour") ,   all.x=TRUE , all.y =TRUE )



APort1[is.na(APort1)]<-0

p <- gapminder %>%
  plot_ly(
    x = APort1$Hour, 
    y = APort1$Freq.x, 
    frame = APort1$Month, 
    type = 'bar'  ) %>% 
    add_trace(y = APort1$Freq.y ) %>%
  layout( barmode = 'stack')

p

