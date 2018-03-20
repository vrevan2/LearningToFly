canellations<-read.csv("data/mapweathercan.csv")
weather<-read.csv("data/mapweather.csv")

colnames(canellations)<- c("X","Month","DayofMonth","Dest","Origin","OriginState","DestState","NoOfCancellations","Olat","Olong","Dlat","Dlong", "OPRCP","OSNOW",      
                             "OSNWD","OTMAX","OTMIN","OTAVG","DPRCP","DSNOW","DSNWD","DTMAX","DTMIN","DTAVG")

colnames(weather)<- c("X","Month","DayofMonth","Dest","Origin","OriginState","DestState","NoOfFlights","Olat","Olong","Dlat","Dlong", "OPRCP","OSNOW",      
                           "OSNWD","OTMAX","OTMIN","OTAVG","DPRCP","DSNOW","DSNWD","DTMAX","DTMIN","DTAVG")
data <- merge(canellations,weather , by = c("Month","DayofMonth","Dest","Origin","OriginState","DestState","Olat","Olong","Dlat","Dlong", "OPRCP","OSNOW",      
                                            "OSNWD","OTMAX","OTMIN","OTAVG","DPRCP","DSNOW","DSNWD","DTMAX","DTMIN","DTAVG")
              ,all.y = TRUE)

nrow(data)
data$X.x<-NULL
data$X.y<-NULL
write.csv(data, "mapweatherfull.csv")