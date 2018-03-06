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
colnames(df) <- as.character(read.table("data/OTP_Header.txt")[,1])
# df <- df[sample(nrow(df), 1000), 1:64] #### REMOVE the sampling in this line later
df <- df[, 1:64] #### REMOVE the sampling in this line later

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

#### End of Global Variables Creation Section ####


#### New Variables/Columns Declared : (Add declared Variables here)

# dataset$DepDateTime, dataset$ArrDateTime -  Have NAs as the flights might be cancelled
# dataset$OriginDateTime, dataset$DestDateTime - Scheduled Times, Always present
# dataset$CRSArrHourofDay, ArrHourofDay , CRSDepHourofDay, DepHourofDay  - Range(0 - 23) , Non CRS ones have NA's due to same reason as above
# dataset$CRSArrMonthofYear, ArrMonthofYear, CRSDepMonthofYear,  CRSDepMonthofYear - Range(1,12) , Non CRS ones have NA's due to same reason as above 

####

######## Grade C (a) ########

noArrDept <- data.frame("ArrMonthofYear" = df$ArrMonthofYear,"DeptMonthofYear" = df$DepMonthofYear,
                        "AirlineID" = df$AirlineID, "Dest" = df$Dest, "Origin" = df$Origin)

arrivalAPort1 <- data.frame(subset(noArrDept, ArrMonthofYear == 1 & Dest == 'ORD'))
departureAPort1 <- data.frame(subset(noArrDept, DeptMonthofYear == 1 & Origin == 'ORD'))

arrivalAPort1 <- data.frame(table(arrivalAPort1$AirlineID))
colnames(arrivalAPort1) <- c('AirlineID','NoOfArrivals')
departureAPort1 <- data.frame(table(departureAPort1$AirlineID))
colnames(departureAPort1) <- c('AirlineID','NoOfDepartures')

APort1 <- merge(arrivalAPort1, departureAPort1, by = "AirlineID")
APort1$Airport <- 'ORD'
melt_APort1 <- melt(APort1, id.vars = 'AirlineID', measure.vars = c('NoOfArrivals', 'NoOfDepartures'))

arrivalAPort2 <- data.frame(subset(noArrDept, ArrMonthofYear == 1 & Dest == 'MDW'))
departureAPort2 <- data.frame(subset(noArrDept, ArrMonthofYear == 1 & Origin == 'MDW'))

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
commonAirports <- data.frame("ArrMonthofYear" = df$ArrMonthofYear,"DeptMonthofYear" = df$DepMonthofYear,
                             "Dest" = df$Dest, "Origin" = df$Origin)

# Most common 15 Destination Airports
commonDestinationAirportsAPort1 <- data.frame(subset(commonAirports, ArrMonthofYear == 1 & Origin == 'ORD'))
commonDestinationAirportsAPort1 <- data.frame(table(commonDestinationAirportsAPort1$Dest))
commonDestinationAirportsAPort1 <- commonDestinationAirportsAPort1[order(-commonDestinationAirportsAPort1$Freq),]
common15DestAPort1 <- head(commonDestinationAirportsAPort1,15)
common15DestAPort1$Origin <- 'ORD'
colnames(common15DestAPort1) <- c('IATA','NoOfFlights','Origin')
common15DestAPort1 <- merge(x = common15DestAPort1, y = airports[,c('AirportName','IATA')], by = 'IATA', all.x = TRUE)
common15DestAPort1 <- common15DestAPort1[c(1,4,2,3)]

commonDestinationAirportsAPort2 <- data.frame(subset(commonAirports, ArrMonthofYear == 1 & Origin == 'MDW'))
commonDestinationAirportsAPort2 <- data.frame(table(commonDestinationAirportsAPort2$Dest))
commonDestinationAirportsAPort2 <- commonDestinationAirportsAPort2[order(-commonDestinationAirportsAPort2$Freq),]
common15DestAPort2 <- head(commonDestinationAirportsAPort2,15)
common15DestAPort2$Origin <- 'MDW'
colnames(common15DestAPort2) <- c('IATA','NoOfFlights','Origin')
common15DestAPort2 <- merge(x = common15DestAPort2, y = airports[,c('AirportName','IATA')], by = 'IATA', all.x = TRUE)
common15DestAPort2 <- common15DestAPort2[c(1,4,2,3)]

combineCommonDestAirports <- rbind(common15DestAPort1, common15DestAPort2)

# Most common 15 Arrival Airports
commonOriginAirportsAPort1 <- data.frame(subset(commonAirports, ArrMonthofYear == 1 & Dest == 'ORD'))
commonOriginAirportsAPort1 <- data.frame(table(commonOriginAirportsAPort1$Origin))
commonOriginAirportsAPort1 <- commonOriginAirportsAPort1[order(-commonOriginAirportsAPort1$Freq),]
common15OriginAPort1 <- head(commonOriginAirportsAPort1,15)
common15OriginAPort1$Origin <- 'ORD'
colnames(common15OriginAPort1) <- c('IATA','NoOfFlights','Destination')
common15OriginAPort1 <- merge(x = common15OriginAPort1, y = airports[,c('AirportName','IATA')], by = 'IATA', all.x = TRUE)
common15OriginAPort1 <- common15OriginAPort1[c(1,4,2,3)]

commonOriginAirportsAPort2 <- data.frame(subset(commonAirports, ArrMonthofYear == 1 & Dest == 'MDW'))
commonOriginAirportsAPort2 <- data.frame(table(commonOriginAirportsAPort2$Origin))
commonOriginAirportsAPort2 <- commonOriginAirportsAPort2[order(-commonOriginAirportsAPort2$Freq),]
common15OriginAPort2 <- head(commonOriginAirportsAPort2,15)
common15OriginAPort2$Origin <- 'MDW'
colnames(common15OriginAPort2) <- c('IATA','NoOfFlights','Destination')
common15OriginAPort2 <- merge(x = common15OriginAPort2, y = airports[,c('AirportName','IATA')], by = 'IATA', all.x = TRUE)
common15OriginAPort2 <- common15OriginAPort2[c(1,4,2,3)]

combineCommonOriginAirports <- rbind(common15OriginAPort1, common15OriginAPort2)


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

















