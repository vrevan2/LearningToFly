library(shiny)
library(shinydashboard)

library(plotly)
library(tidyr)
library(leaflet)
library(jpeg)
library(grid)

library(lubridate)
library(reshape2)
library(DT)


library('maps')
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


all_states <- map_data("state")
#plot all states with ggplot
p <- ggplot() + 
      geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey80" )+ 

             
       geom_segment(aes(x = weather_sub$Olong, y = weather_sub$Olat, xend = weather_sub$Dlong,
         yend = weather_sub$Dlat  ) , colour = "rgba(255,0,0,0.2)", data = weather_sub)+ 
      
        geom_point( data=origins, aes(x=Olong, y=Olat, size = 0.00001 ) , colour = "rgba(0,0,255,0.2)")+  

        geom_point( data=dests, aes(x=Dlong, y=Dlat, size = 0.00001 ) , colour = "rgba(0,255,0,0.2)")+  
        
      
          scale_fill_distiller(palette = "Spectral", na.value = "red")+ 
         coord_map()+ 
         labs(x = NULL, y = NULL)+ 
         theme_bw()+ 
         theme(panel.border = element_blank(), panel.grid = element_blank(),
                 axis.ticks = element_blank(), axis.text = element_blank())

p <- ggplotly(p)

p$x$data[[2]]$text<- paste("Flights :" , weather_sub$Freq )
p$x$data[[1]]$hoverinfo<-"none"
p$x$data[[3]]$text<- paste0("IATA : " , origins$Origin,
                      "<br>" , "PRCP : " , origins$OPRCP,
                      "<br>" ,"SNOW : " , origins$OSNOW,
                      "<br>" ,"SNWD : " , origins$OSNWD,
                      "<br>" ,"TMAX : " , origins$OTMAX,
                      "<br>" ,"TMIN : " , origins$OTMIN,
                      "<br>" ,"TAVG : " , origins$OTAVG
                       )
p$x$data[[4]]$text<- paste0("IATA : " , dests$Dest,
                      "<br>" , "PRCP : " , dests$DPRCP,
                      "<br>" ,"SNOW : " , dests$DSNOW,
                      "<br>" ,"SNWD : " , dests$DSNWD,
                      "<br>" ,"TMAX : " , dests$DTMAX,
                      "<br>" ,"TMIN : " , dests$DTMIN,
                      "<br>" ,"TAVG : " , dests$DTAVG
                       )

p

