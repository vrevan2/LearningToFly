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

weather <- read.csv("data/weather_data.csv")


library(plyr)



# Generate cancellations data file
pairs <- count(df[df$CancellationCode!="", ] , c('Month', 'DayofMonth', 'Origin' , 'Dest' , "OriginState", "DestState"  ))

# Generate Total flights data file
#pairs <- count(df[df$CancellationCode!="", ] , c('Month', 'DayofMonth', 'Origin' , 'Dest' , "OriginState", "DestState"  ))

colnames(pairs)<- c('Month', 'DayofMonth', 'Origin' , 'Dest' , "OriginState", "DestState" ,'Freq')

head(pairs)

airports_map <- airports[, c('IATA','AirportLatitude','AirportLongitude','StationID')]

pairs<- merge(pairs, airports_map , by.x = 'Origin'  , by.y=  'IATA' , all.y= FALSE)

head(pairs)

colnames(pairs)<- c('Origin','Month', 'DayofMonth', 'Dest', "OriginState", "DestState"  ,'Freq' ,'Olong' ,'Olat' , 'OStationID')

pairs<- merge(pairs, airports_map , by.x = 'Dest'  , by.y=  'IATA' , all.y= FALSE )

colnames(pairs)<- c( 'Dest' ,'Origin' , 'Month', 'DayofMonth', "OriginState", "DestState",'Freq' ,'Olat' ,'Olong' , 'OStationID' , 'Dlat' ,'Dlong' , 'DStationID')



weather$Date <- as.character(weather$Date)
weather$DayofMonth<-as.numeric(substr(weather$Date , 7,8))
weather$Month<-as.numeric(substr(weather$Date , 5,6))
colnames(weather)<-c("StationID", "Date","OPRCP","OSNOW","OSNWD","OTMAX","OTMIN","OTAVG","DayofMonth","Month")

weatherAP<-merge(pairs,weather, by.x=c("OStationID" ,"Month" , "DayofMonth") , by.y = c("StationID" ,"Month" , "DayofMonth") , all.x = TRUE , all.y = FALSE)


colnames(weather)<-c("StationID", "Date","DPRCP","DSNOW","DSNWD","DTMAX","DTMIN","DTAVG","DayofMonth","Month")

weatherAP<-merge(weatherAP,weather, by.x=c("DStationID" ,"Month" , "DayofMonth") , by.y = c("StationID" ,"Month" , "DayofMonth") , all.x = TRUE , all.y = FALSE)

weatherfinal<- subset(weatherAP , select = c( "Month" ,"DayofMonth","Dest","Origin", "OriginState", "DestState","Freq","Olat", "Olong","Dlat","Dlong","OPRCP","OSNOW","OSNWD","OTMAX",     
                       "OTMIN","OTAVG","DPRCP","DSNOW","DSNWD","DTMAX","DTMIN","DTAVG"))

# For cancelled Flights data
write.csv(weatherfinal, "mapweathercan.csv")
# Total Flights Data
#write.csv(weatherfinal, "mapweather.csv")




