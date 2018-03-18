library(shiny)
library(shinydashboard)

library(plotly)
library(tidyr)
library(DT)


library('leaflet')
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

weather <- read.csv("data/mapweather.csv")


dayofmonth=1
month = 12
originstate = "IL"

  
weather_sub <- subset(weather ,  Month == 12 & DayofMonth == 1 & OriginState == "IL" )





origins <- unique(weather_sub[,c('Olat' , 'Olong' , "OPRCP" ,"OSNOW", "OSNWD","OTMAX","OTMIN","OTAVG" , "Origin" )])
dests <- unique(weather_sub[,c('Dlat' , 'Dlong' , "DPRCP" ,"DSNOW", "DSNWD","DTMAX","DTMIN","DTAVG","Dest"  )])



x<- leaflet() %>%  addTiles() 

for(i in 1:nrow(weather_sub)){
    x <- addPolylines(x, lat = as.numeric(weather_sub[i, c('Olat','Dlat' )]), 
                               lng = as.numeric(weather_sub[i, c('Olong', 'Dlong')]) , label = paste(as.character(weather_sub[i,c('Freq')]) ," : " ,weather_sub[i,c('Origin')]) ,weight = weather_sub[i,c('Freq')]/15 )
}
x<-addCircles(x,lng =origins$Olong,
             lat =origins$Olat,
             radius = 1, 
             color = "red", 
             fillColor = "red",
             popup = paste0("<b>IATA : " , origins$Origin,
                      "</b><br/>" , "<b>PRCP</b> : " , origins$OPRCP,
                      "<br/>" ,"<b>SNOW </b>: " , origins$OSNOW,
                      "<br/>" ,"<b>SNWD </b>: " , origins$OSNWD,
                      "<br/>" ,"<b>TMAX </b>: " , origins$OTMAX,
                      "<br/>" ,"<b>TMIN </b>: " , origins$OTMIN,
                      "<br/>" ,"<b>TAVG </b>: " , origins$OTAVG
                       ))
x<-addCircles(x,lng =dests$Dlong,
             lat =dests$Dlat,
             radius = 1, 
             color = "red", 
             fillColor = "red",
             popup = paste0("<b>IATA : " , dests$Dest,
                      "</b><br/>" , "<b>PRCP :</b> " , dests$DPRCP,
                      "<br/>" ,"<b>SNOW :</b> " , dests$DSNOW,
                      "<br/>" ,"<b>SNWD : </b>" , dests$DSNWD,
                      "<br/>" ,"<b>TMAX : </b>" , dests$DTMAX,
                      "<br/>" ,"<b>TMIN : </b>" , dests$DTMIN,
                      "<br/>" ,"<b>TAVG : </b>" , dests$DTAVG
                       ))
x

