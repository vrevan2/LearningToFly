library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyr)
library(leaflet)
library(jpeg)
library(grid)
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(DT)

#### Globals
defaultTz <- "America/Chicago"
hourFormat <- 12
month <- 0
Airport1 <- 'ORD'
Airport2 <- 'MDW'
temperatureFormat <- 'F'

daysOfWeek <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
hours <- if(hourFormat == 24) c(0:23) else c(paste(c(0:11), "am"), "Noon", paste(c(1:11), "pm"))

temperature <- function(x) {
  return (if (temperatureFormat == 'F') x else (x - 32) * (5 / 9))
}

## Raw Data
df <- read.csv("data/OTP_2017.csv")

## Airport Info
airports <- read.csv("data/airports_stations.csv")
rownames(airports) <- as.character(airports$IATA)

#Airlines Lookup
airlines <- read.csv("data/airlines.csv")

#### End of Global Variables Creation Section ####

######## Grade C (a) ########

### Heatmaps ###
getHourDayHeatMap <- function(sourceData, hourColname) {
  baseData <- expand.grid(0:23, 1:7)
  colnames(baseData) <- c('Hour', 'DayOfWeek')
  
  data <- merge(baseData, sourceData, # get values for all permutations of hour and dayofweek
                by.x = c('Hour', 'DayOfWeek'), 
                by.y = c(hourColname, 'DayOfWeek'),
                all = TRUE)
  data[which(is.na(data[,3]), arr.ind=TRUE), 3] <- 0 # update NAs to 0
  data <- arrange(data, Hour, DayOfWeek) # order by hour, dayofweek
  data <- matrix(data$n, nrow = 7, ncol = 24, dimnames = list(daysOfWeek, hours) ) # convert to matrix
  return(plot_ly(x = hours, y = daysOfWeek, z = data, type = "heatmap")) # plot
}

getHourMonthHeatMap <- function(sourceData, hourColname) {
  baseData <- expand.grid(0:23, 1:12)
  colnames(baseData) <- c('Hour', 'Month')
  
  data <- merge(baseData, sourceData, # get values for all permutations of hour and dayofweek
                by.x = c('Hour', 'Month'), 
                by.y = c(hourColname, 'Month'),
                all = TRUE)
  data[which(is.na(data[,3]), arr.ind=TRUE), 3] <- 0 # update NAs to 0
  data <- arrange(data, Hour, Month) # order by hour, dayofweek
  data <- matrix(data$n, nrow = 12, ncol = 24, dimnames = list(months, hours) ) # convert to matrix
  return(plot_ly(x = hours, y = months, z = data, type = "heatmap")) # plot
}

##  by Day of week vs. Time of day
departureAirport1 <- subset(df, (if (month == 0) TRUE else Month == month) 
                            & Origin == Airport1 & CancellationCode == '')
plot <- getHourDayHeatMap(count(departureAirport1, DepHour, DayOfWeek), 'DepHour') # Departures

arrivalAirport1 <- subset(df, (if (month == 0) TRUE else Month == month) 
                          & Dest == Airport1 & CancellationCode == '')
plot <- getHourDayHeatMap(count(arrivalAirport1, ArrHour, DayOfWeek), 'ArrHour') # Arrivals

delays <- subset(df, (if (month == 0) TRUE else Month == month) 
                 & (Origin == Airport1 | Dest == Airport1)
                 & (CarrierDelay > 0 | WeatherDelay > 0 | NASDelay > 0 
                    | SecurityDelay > 0 | LateAircraftDelay > 0))
plot <- getHourDayHeatMap(count(delays, ArrHour, DayOfWeek), 'ArrHour') # Delays

cancellations <- subset(df, (if (month == 0) TRUE else Month == month) 
                        & (Origin == Airport1 | Dest == Airport1) & CancellationCode != '')
plot <- getHourDayHeatMap(count(cancellations, CRSDepHour, DayOfWeek), 'CRSDepHour') # Delays


##  by Month of year vs. Time of day
departureAirport1 <- subset(df, Origin == Airport1 & CancellationCode == '')
plot <- getHourMonthHeatMap(count(departureAirport1, DepHour, Month), 'DepHour') # Departures

arrivalAirport1 <- subset(df, Dest == Airport1 & CancellationCode == '')
plot <- getHourMonthHeatMap(count(arrivalAirport1, ArrHour, Month), 'ArrHour') # Arrivals

delays <- subset(df, (Origin == Airport1 | Dest == Airport1)
                 & (CarrierDelay > 0 | WeatherDelay > 0 | NASDelay > 0 
                    | SecurityDelay > 0 | LateAircraftDelay > 0))
plot <- getHourMonthHeatMap(count(delays, ArrHour, Month), 'ArrHour') # Delays

cancellations <- subset(df, (Origin == Airport1 | Dest == Airport1) & CancellationCode != '')
plot <- getHourMonthHeatMap(count(cancellations, CRSDepHour, Month), 'CRSDepHour') # Delays



arrivalCountsByAirlineId <- count(arrivalAirport1, arrivalAirport1$AirlineID)
arrivalAPort1 <- data.frame(table(arrivalAirport1$AirlineID))
colnames(arrivalAPort1) <- c('AirlineID','NoOfArrivals')
departureAPort1 <- data.frame(table(departureAirport1$AirlineID))
colnames(departureAPort1) <- c('AirlineID','NoOfDepartures')

APort1 <- merge(arrivalAPort1, departureAPort1, by = "AirlineID")
APort1$Airport <- 'ORD'
melt_APort1 <- melt(APort1, id.vars = 'AirlineID', measure.vars = c('NoOfArrivals', 'NoOfDepartures'))

arrivalAPort2 <- data.frame(subset(df, Month == 1 & Dest == 'MDW'))
departureAPort2 <- data.frame(subset(df, Month == 1 & Origin == 'MDW'))

arrivalAPort2 <- data.frame(table(arrivalAPort2$AirlineID))
colnames(arrivalAPort2) <- c('AirlineID','NoOfArrivals')
departureAPort2 <- data.frame(table(departureAPort2$AirlineID))
colnames(departureAPort2) <- c('AirlineID','NoOfDepartures')

APort2 <- merge(arrivalAPort2, departureAPort2, by = 'AirlineID')
APort2$Airport <- 'MDW'
melt_APort2 <- melt(APort2, id.vars = 'AirlineID', measure.vars = c('NoOfArrivals', 'NoOfDepartures'))

combineAirports <- rbind(APort1,APort2)
combineAirports <- merge(x = combineAirports, y = airlines, by = "AirlineID", all.x = TRUE)
melt_combineAirports <- melt(combineAirports, id.vars = c('AirlineID','Airport','AirlineName','AirlineCode'), measure.vars = c('NoOfArrivals', 'NoOfDepartures'))

tableFlightsByAirline <- merge(x = APort2, y = APort1, by = 'AirlineID', all = TRUE)
tableFlightsByAirline <- merge(x = tableFlightsByAirline, y = airlines, by = "AirlineID", all.x = TRUE)
newtableFlightsByAirline <- tableFlightsByAirline[c(9,8,2,3,5,6)]

sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Airline Code'),
      th(rowspan = 2, 'Airline Names'),
      th(colspan = 2, 'Midway'),
      th(colspan = 2, 'OHare')
    ),
    tr(
      lapply(rep(c('Arrivals', 'Departures'), 2), th)
    )
  )
))


### 15 most common arrival and destination airports ####
commonAirports <- data.frame("Month" = df$Month,
                             "Dest" = df$Dest, 
                             "Origin" = df$Origin)

# Most common 15 Destination Airports
commonDestinationAirportsAPort1 <- data.frame(subset(commonAirports, Month == 1 & Origin == 'ORD'))
commonDestinationAirportsAPort1 <- data.frame(table(commonDestinationAirportsAPort1$Dest))
commonDestinationAirportsAPort1 <- commonDestinationAirportsAPort1[order(-commonDestinationAirportsAPort1$Freq),]
common15DestAPort1 <- head(commonDestinationAirportsAPort1,15)
common15DestAPort1$Origin <- 'ORD'
colnames(common15DestAPort1) <- c('IATA','NoOfFlights','Origin')
common15DestAPort1 <- merge(x = common15DestAPort1, y = airports[,c('AirportName','IATA')], by = 'IATA', all.x = TRUE)
common15DestAPort1 <- common15DestAPort1[c(1,4,2,3)]

commonDestinationAirportsAPort2 <- data.frame(subset(commonAirports, Month == 1 & Origin == 'MDW'))
commonDestinationAirportsAPort2 <- data.frame(table(commonDestinationAirportsAPort2$Dest))
commonDestinationAirportsAPort2 <- commonDestinationAirportsAPort2[order(-commonDestinationAirportsAPort2$Freq),]
common15DestAPort2 <- head(commonDestinationAirportsAPort2,15)
common15DestAPort2$Origin <- 'MDW'
colnames(common15DestAPort2) <- c('IATA','NoOfFlights','Origin')
common15DestAPort2 <- merge(x = common15DestAPort2, y = airports[,c('AirportName','IATA')], by = 'IATA', all.x = TRUE)
common15DestAPort2 <- common15DestAPort2[c(1,4,2,3)]

combineCommonDestAirports <- rbind(common15DestAPort1, common15DestAPort2)

# Most common 15 Arrival Airports
commonOriginAirportsAPort1 <- data.frame(subset(commonAirports, Month == 1 & Dest == 'ORD'))
commonOriginAirportsAPort1 <- data.frame(table(commonOriginAirportsAPort1$Origin))
commonOriginAirportsAPort1 <- commonOriginAirportsAPort1[order(-commonOriginAirportsAPort1$Freq),]
common15OriginAPort1 <- head(commonOriginAirportsAPort1,15)
common15OriginAPort1$Origin <- 'ORD'
colnames(common15OriginAPort1) <- c('IATA','NoOfFlights','Destination')
common15OriginAPort1 <- merge(x = common15OriginAPort1, y = airports[,c('AirportName','IATA')], by = 'IATA', all.x = TRUE)
common15OriginAPort1 <- common15OriginAPort1[c(1,4,2,3)]

commonOriginAirportsAPort2 <- data.frame(subset(commonAirports, Month == 1 & Dest == 'MDW'))
commonOriginAirportsAPort2 <- data.frame(table(commonOriginAirportsAPort2$Origin))
commonOriginAirportsAPort2 <- commonOriginAirportsAPort2[order(-commonOriginAirportsAPort2$Freq),]
common15OriginAPort2 <- head(commonOriginAirportsAPort2,15)
common15OriginAPort2$Origin <- 'MDW'
colnames(common15OriginAPort2) <- c('IATA','NoOfFlights','Destination')
common15OriginAPort2 <- merge(x = common15OriginAPort2, y = airports[,c('AirportName','IATA')], by = 'IATA', all.x = TRUE)
common15OriginAPort2 <- common15OriginAPort2[c(1,4,2,3)]

combineCommonOriginAirports <- rbind(common15OriginAPort1, common15OriginAPort2)

#### Arrivals and departures for hour of the day across a month
dfMonth<-df[df$Month == month,]

dfMonthHoD<- dfMonth[dfMonth$Dest == Airport1 | dfMonth$Origin == Airport1| dfMonth$Dest == Airport2 | dfMonth$Origin == Airport2,]
dfMonthHoD<-dfMonthHoD[,c('ArrHour', 'DepHour', 'Dest', 'Origin')]
dfMonthHoD$airport <- ''
dfMonthHoD<- na.omit(dfMonthHoD)
dfMonthHoD[dfMonthHoD$Origin == Airport1 | dfMonthHoD$Dest == Airport1,]$airport <- 1
dfMonthHoD[dfMonthHoD$Origin == Airport2 | dfMonthHoD$Dest == Airport2,]$airport <- 2
dfMonthHoD<-data.frame('Arrivals' = dfMonthHoD$ArrHour, 'Departures' = dfMonthHoD$DepHour, 'airport' = dfMonthHoD$airport)
dfMonthHoDMelt<-melt(dfMonthHoD, id='airport')
dfMonthHoDMelt$airport <- ifelse(dfMonthHoDMelt$airport == 1, Airport1, Airport2)

#for table
Airport2Dep <- df %>% filter(Origin == Airport2 & Month == month) %>% count('hour' = DepHour)
Airport1Dep <- df %>% filter(Origin == Airport1 & Month == month) %>% count('hour' = DepHour)
Airport1Arr <- df %>% filter(Dest == Airport1 & Month == month) %>% count('hour' = ArrHour)
Airport2Arr <- df %>% filter(Dest == Airport2 & Month == month) %>% count('hour' = ArrHour)
Airport1ArrDep<-merge(Airport1Arr, Airport1Dep, by = 'hour', all = TRUE, suffixes = c('Arr', 'Dep'))
Airport2ArrDep<-merge(Airport2Arr, Airport2Dep, by = 'hour', all = TRUE, suffixes = c('Arr', 'Dep'))
tableArrDepByHour<-merge(Airport1ArrDep, Airport2ArrDep, by = 'hour', all = TRUE)

sketchHour = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Hour of the Day'),
      th(colspan = 2, 'Midway'),
      th(colspan = 2, 'OHare')
    ),
    tr(
      lapply(rep(c('Arrivals', 'Departures'), 2), th)
    )
  )
))

### number of arrivals and departures for day of the week across a month
dfMonthDoW<- dfMonth[dfMonth$Dest == Airport1 | dfMonth$Origin == Airport1| dfMonth$Dest == Airport2 | dfMonth$Origin == Airport2,]
dfMonthDoW <- dfMonthDoW[,c('DayOfWeek', 'Dest', 'Origin')]
colnames(dfMonthDoW)<-c('Day', 'Arr', 'Dep')
dfMonthDoW$Arr<-sapply(dfMonthDoW$Arr, function(x) if(is.na(x)) x=0 else if(x == Airport1) x=1 else if (x==Airport2) x=2 else x=0)
dfMonthDoW$Dep<-sapply(dfMonthDoW$Dep, function(x) if(is.na(x)) x=0 else if(x == Airport1) x=1 else if (x==Airport2) x=2 else x=0)
dfMonthDoWMelt <- melt(dfMonthDoW, id='Day')
dfMonthDoWMelt<-dfMonthDoWMelt[!((dfMonthDoWMelt$variable == 'Arr' & dfMonthDoWMelt$value==0) | (dfMonthDoWMelt$variable == 'Dep' & dfMonthDoWMelt$value==0)),]
dfMonthDoWMelt$cat<-''
dfMonthDoWMelt[dfMonthDoWMelt$value == 1,]$cat <- Airport1
dfMonthDoWMelt[dfMonthDoWMelt$value == 2,]$cat <- Airport2

##for table
A2DepDay <- df %>% filter(Origin == Airport2 & Month == month) %>% count('Day' = DayOfWeek)
A1DepDay <- df %>% filter(Origin == Airport1 & Month == month) %>% count('Day' = DayOfWeek)
A1ArrDay <- df %>% filter(Dest == Airport1 & Month == month) %>% count('Day' = DayOfWeek)
A2ArrDay <- df %>% filter(Dest == Airport2 & Month == month) %>% count('Day' = DayOfWeek)
A1ArrDepDay<-merge(A1ArrDay, A1DepDay, by = 'Day', all = TRUE, suffixes = c('Arr', 'Dep'))
A2ArrDepDay<-merge(A2ArrDay, A2DepDay, by = 'Day', all = TRUE, suffixes = c('Arr', 'Dep'))
tableArrDepByDay<-merge(A1ArrDepDay, A2ArrDepDay, by = 'Day', all = TRUE)

sketchDay = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Day of the Week'),
      th(colspan = 2, 'Midway'),
      th(colspan = 2, 'OHare')
    ),
    tr(
      lapply(rep(c('Arrivals', 'Departures'), 2), th)
    )
  )
))

### number of delays for hour of the day across a month

dfdelay<-dfMonth[(dfMonth$Dest == Airport1 | dfMonth$Dest == Airport2) & dfMonth$ArrDelay > 0,]
#dfdelay<-dfdelay[,c('Dest','ArrDelay', 'CRSArrHour')]


############## layout ################
ui <- dashboardPage(
  dashboardHeader(title = 'Learning To Fly'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Flight Data', tabName = 'flightData'),
      menuItem('Top Charts', tabName = 'topCharts'),
      menuItem('About', tabName = 'about')
    )#sidebarMenu
  ),#dashboardSidebar
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),#tags$head
    tabItems(
      tabItem("flightData",
              fluidRow(
                box(
                  radioButtons("filter","Filter By:",
                               choices = c("Airlines", "Hours of Day","Days of Week","Delays"),
                               selected = "Airlines", inline = TRUE)
                )
                ),
                fluidRow(
                  conditionalPanel(
                    condition = "input.filter == 'Airlines'",
                    box(title = "Plot showing number of Arrivals and Departures by Airlines for the month of January", width = 6,
                        plotOutput("bar_airlines")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.filter == 'Airlines'",
                    box(title = "Table of Number of Arrivals and Departures by Airlines for the month of January", width = 6,
                        dataTableOutput("table_airlines")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.filter == 'Hours of Day'",
                    box(title = "Plot showing Number of Arrivals and Departures for each Hour of the Day for the month of January", width = 6,
                        plotOutput("bar_hour")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.filter == 'Hours of Day'",
                    box(title = "Table of Number of Arrivals and Departures for each Hour of the Day for the month of January", width = 6,
                        dataTableOutput("table_hour")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.filter == 'Days of Week'",
                    box(title = "Number of Arrivals and Departures for each Day of the Week for the month of January", width = 6,
                        plotOutput("bar_day")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.filter == 'Days of Week'",
                    box(title = "Number of Arrivals and Departures for each Day of the Week for the month of January", width = 6,
                        dataTableOutput("table_day")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.filter == 'Delays'",
                    box(title = "Number of Delays for each hour of the day for the month of January", width = 6,
                        plotOutput("bar_delay")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.filter == 'Delays'",
                    box(title = "Number of Delays for each hour of the day for the month of January", width = 6,
                        dataTableOutput("table_delay")
                    )
                  )

              )#fluidRow
      ),#tabItem
      tabItem("topCharts",
              fluidRow(
                tabBox(width = 12,
                  tabPanel("Plots",
                        plotOutput("common_originPorts"),
                        plotOutput("common_destinationPorts")
                    ),
                  tabPanel("Table: Most Common 15 Origin Airports",
                           fluidRow(
                             column( dataTableOutput("table_commonOriginAPort1"),width = 6),
                             column( dataTableOutput("table_commonOriginAPort2"),width = 6)
                             )
                      ),
                  tabPanel("Table: Most Common 15 Destination Airports",
                           fluidRow(
                             column( dataTableOutput("table_commonDestinationAPort1"),width = 6),
                             column( dataTableOutput("table_commonDestinationAPort2"),width = 6)
                           )
                  )
                   )
                )
              
      ),#tabItem
      tabItem("about",
        htmlOutput("about_project")
      )#tabItem
    )#tabItems
  )#dashboardBody
)#dashboardPage

###### server ########
server <- function(input, output){
  
  ############# themes for plots ################
  barplot_theme <- theme(legend.position = c(0.2, 0.9), legend.title = element_blank())
  
  ############ render plots #####################
  # bar chart number of flights(arrivals and departures) by airlines
  output$bar_airlines <- renderPlot({
    ggplot(data = melt_combineAirports, aes(x = Airport, y = value, fill = variable)) + geom_bar(stat = "identity") + facet_grid(~AirlineCode) +
      barplot_theme + labs(x = 'Airports', y = 'Number of Flights') + scale_fill_manual(labels = c("Arrivals", "Departures"), values = c('#7a8da8','#c6adaa'))
  })

  # table number of flights(arrivals and departures) by airlines
  output$table_airlines <- DT::renderDataTable(
    DT::datatable({
      newtableFlightsByAirline
    },
    container = sketch,
    rownames = FALSE,
    options = list(paging = FALSE, searching = FALSE, dom = 't',order = list(list(0,'asc')))
    )
  )
  
  # 15 most common origin airports
  output$common_originPorts <- renderPlot({
    ggplot(data = combineCommonDestAirports , aes(x = Origin, y = NoOfFlights, fill = Origin)) + geom_bar(stat = "identity") + facet_grid(~IATA) +
      barplot_theme + labs(title = 'Most common 15 Origin Airports for Midway and O\'Hare International Airports',x = '', y = 'Number of Flights') + 
      theme(legend.position = c(0.1, 0.9), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      scale_fill_manual(labels = c("Midway (MDW)", "OHare (ORD)"), values = c('#5f7390','#b2908b'))
  })
  
  # 15 most common destination airports
  output$common_destinationPorts <- renderPlot({
    ggplot(data = combineCommonOriginAirports , aes(x = Destination, y = NoOfFlights, fill = Destination)) + geom_bar(stat = "identity") + facet_grid(~IATA) +
      barplot_theme + labs(title = 'Most common 15 Destination Airports for Midway and O\'Hare International Airports',x = '', y = 'Number of Flights') + 
      theme(legend.position = c(0.1, 0.9),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      scale_fill_manual(labels = c("Midway (MDW)", "OHare (ORD)"), values = c('#5f7390','#b2908b'))
  })
  
  # 15 most common origin airports for Airport1(ORD)
  output$table_commonOriginAPort1 <- DT::renderDataTable(
    DT::datatable({
      common15OriginAPort1
    },
    caption = 'Most common 15 Origin Airports for O\'Hare International Airport',
    options = list(paging = FALSE, searching = FALSE, dom = 't', searching = FALSE,order = list(list(1,'asc')),columnDefs = list(list(visible = FALSE, targets = c(0,4)))))
  )
  
  # 15 most common origin airports for Airport2(MDW)
  output$table_commonOriginAPort2 <- DT::renderDataTable(
    DT::datatable({
      common15OriginAPort2
    },
    caption = 'Most common 15 Origin Airports for Midway International Airport',
    options = list(paging = FALSE, searching = FALSE, dom = 't', searching = FALSE,order = list(list(1,'asc')),columnDefs = list(list(visible = FALSE, targets = c(0,4)))))
  )
  
  # 15 most common destination airports for Airport1(ORD)
  output$table_commonDestinationAPort1 <- DT::renderDataTable(
    DT::datatable({
      common15DestAPort1
    },
    caption = 'Most common 15 Destination Airports for O\'Hare International Airport',
    options = list(paging = FALSE, searching = FALSE, dom = 't', searching = FALSE,order = list(list(1,'asc')),columnDefs = list(list(visible = FALSE, targets = c(0,4)))))
  )
  
  # 15 most common destination airports for Airport2(MDW)
  output$table_commonDestinationAPort2 <- DT::renderDataTable(
    DT::datatable({
      common15DestAPort2
    },
    caption = 'Most common 15 Destination Airports for Midway International Airport',
    options = list(paging = FALSE, searching = FALSE, dom = 't', searching = FALSE,order = list(list(1,'asc')),columnDefs = list(list(visible = FALSE, targets = c(0,4)))))
  )
  
  # arrivals and departures by hour of the day
  output$bar_hour<- renderPlot({
    ggplot(dfMonthHoDMelt, aes(x = airport, fill = variable)) + 
      geom_bar(stat = 'count', position = 'stack') + facet_grid(~ value) + 
      scale_fill_manual(labels=c('Arrivals', 'Departures'), values = c('#7a8da8', '#c6adaa')) +
      labs(x='Hour of the Day', y='Count')
  })
  
  # arrivals and departures by day of the week
  output$bar_day<- renderPlot({
    ggplot(dfMonthDoWMelt, aes(x = cat, fill = variable)) + geom_bar(stat = 'count', position = 'stack') + facet_grid(~ Day) + scale_fill_manual(labels=c('Arrivals', 'Departures'), values = c('#7a8da8', '#c6adaa')) +
      labs(x='Day of the Week', y='Count')
  })
  
  output$bar_delay<- renderPlot({
    ggplot(dfdelay, aes(x = Dest, fill = Dest)) + geom_bar(stat = 'count') + facet_grid(~ CRSArrHour) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
      labs(x='Hour of the Day', y='Count')
  })
  
  # table number of flights(arrivals and departures) by hour of the day
  output$table_hour <- DT::renderDataTable(
    DT::datatable({
      tableArrDepByHour
    },
    container = sketchHour,
    rownames = FALSE,
    options = list(paging = FALSE, searching = FALSE, dom = 't',order = list(list(0,'asc')))
    )
  )
  # table number of flights(arrivals and departures) by day of the week
  output$table_day <- DT::renderDataTable(
    DT::datatable({
      tableArrDepByDay
    },
    container = sketchDay,
    rownames = FALSE,
    options = list(paging = FALSE, searching = FALSE, dom = 't',order = list(list(0,'asc')))
    )
  )
  
  # displaying text for the about section
  output$about_project <- renderUI({
    about <- "<h4><b>About: </b><br/>This application shows the interactive visualizations of Airline flights in Illinois for 2017. The user can visualize
    the number of flights (arrivals and departures) of an airport filtered by airlines, hour of day, day of week or number of delays.
    Also, the user can look at the statistics of the most common arrivals and destination airports.</h4><br/>"
    libraries <- "<b>Libraries used: </b> <br/> shiny, shinydashboard, ggplot2, DT, lubridate and reshape2<br/><br/>"
    sources <- "<b>Data Sources:</b> <br/> <a href=\"https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time\" target=\"_blank\" >Airline On-Time Performance Data</a> from the 
      Bureau of Transportation Statisitcs has been used for producing the visualizations.<br/><br/>"
    details <- "<b>For more details:</b> <br/> <a href = \"https://vrevan2.github.io/LearningToFly/index.html\" target=\"_blank\">Project Details</a> <br/><br/> "
    team <- " <h4><b>Team: R You Shiny</b></h4> 
      Amey Barapatre <br/>  Sai Phaltankar <br/> Vivek R. Shivaprabhu <br/> Jaspreet Kaur Sohal <br/>"
    HTML(paste(about, libraries, sources, details,team))
  })
}#server

shinyApp(ui = ui, server = server)
