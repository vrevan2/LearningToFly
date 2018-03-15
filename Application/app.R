library(shiny)
library(shinydashboard)
library(plotly)
library(plyr)
library(reshape2) ##??
library(DT)

#### Globals
defaultTz <- "America/Chicago"
is24Hour <- FALSE
isMetric <- FALSE

daysOfWeek <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
daysOfWeekDf <- data.frame(daysOfWeek, c(1:7))
colnames(daysOfWeekDf) <- c("DayName", "DayNumber")

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
monthsDf <- data.frame(months, c(1:12))
colnames(monthsDf) <- c("MonthName", "MonthNumber")

times <- c(paste(c(0:4), 'to', c(1:5), 'hr'), '5 hr or more')
timesDf <- data.frame(times, c(0:5))
colnames(timesDf) <- c("TimeName", "TimeNumber")

hours <- function() {
  return(if(is24Hour) c(0:23) else c(paste(c(0:11), "am"), "Noon", paste(c(1:11), "pm")))
}
hoursDf <- function() {
  hdf <- data.frame(hours(), c(0:23))
  colnames(hdf) <- c("HourName", "HourNumber")
  return(hdf)
}

distanceGroups <- function(){
  return(if(isMetric)
    c(paste(c(0:7) * 400, 'to', c(1:8) * 400 - 1, "km"), '3200+ km')
    else
      c(paste(c(0:7) * 250, 'to', c(1:8) * 250 - 1, "miles"), '2000+ miles'))
}
distanceGroupDf <- function() {
  dgdf <- data.frame(distanceGroups(), c(0:8))
  colnames(dgdf) <- c('DistanceName', 'DistanceNumber')
  return(dgdf)
}

temperature <- function(x) {
  return (if (temperatureFormat == 'F') x else (x - 32) * (5 / 9))
}

tableHeaderArrDep <- function(airport1, airport2, tableName) {
  return(htmltools::withTags(table(
    class = 'display', thead(
      tr(
        th(rowspan = 2, tableName),
        th(colspan = 2, airport1),
        th(colspan = 2, airport2)
      ), tr(
        lapply(rep(c('Arrivals', 'Departures'), 2), th)
      ))
  )))
}

tableHeaderTwoAirports <- function(airport1, airport2, tableName) {
  return(htmltools::withTags(table(
    class = 'display', thead(
      tr(
        th(tableName),
        th(airport1),
        th(airport2)
      ))
  )))
}

tableHeaderOneAirport <- function(airport1, airport2, tableName) {
  return(htmltools::withTags(table(
    class = 'display', thead(
      tr(
        th(tableName),
        th(airport1),
        th(airport2)
      ))
  )))
}

# Airline Info
airlines <- read.csv("data/airlines.csv", na.strings = '-')
rownames(airlines) <- as.character(airlines$AirlineCode)

# Airport Info
airports <- read.csv("data/airports_stations.csv")
rownames(airports) <- as.character(airports$IATA)

# Flight Data
flights <- read.csv("data/OTP_2017.csv")
flights$DistanceGroup <- flights$DistanceGroup - 1 # convert 1:n to 0:n-1
flights$CRSElapsedTimeGroup[flights$CRSElapsedTimeGroup > 5] <- 5 # bin times more than 5 hrs into one group
flights$DistanceGroup[flights$DistanceGroup > 8] <- 8 # bin times more than 2000 miles into one group
cancellations <- flights[flights$CancellationCode != '',]
flights <- flights[flights$CancellationCode == '',]

# Choices for Airport Dropdown
airportList <- as.character(read.csv("data/illinois_airports.csv")$IATA)
airportList <- airportList[order(airportList)]
names(airportList) <- sapply(airportList, function(x) paste(x, '-', airports[x == airports$IATA,]$AirportName))

# Layout
ui <- dashboardPage(
  dashboardHeader(title = 'R you Shiny'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Home', tabName = 'home'),
      menuItem('Flight Data', tabName = 'flightData'),
      menuItem('Compare Airports', tabName = 'compareAirports'),
      menuItem('Single Airports', tabName = 'singleAirport'),
      menuItem('States', tabName = 'states')
    )),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabItems(
      tabItem("home", includeHTML("home.html")),
      tabItem(
        "flightData",
        fluidRow(column(width = 12, h2('Number of Arrivals and Departures'))),
        fluidRow(
          column(width = 4, selectInput("flightDataAirport1", "Airport 1", airportList, width = '100%', selected = 'ORD')),
          column(width = 4, selectInput("flightDataAirport2", "Airport 2", airportList, width = '100%', selected = 'MDW')),
          column(width = 1, checkboxInput("flightDataCompare", "Compare")),
          column(width = 1, checkboxInput("flightDataStacked", "Stacked_Bars")),
          column(width = 1, offset = 1, actionButton("arrivalDepartureData", "Show Data"))),
        fluidRow(tabBox(
          id = "flightDataTabs",
          width = 12,
          tabPanel(
            "Number of Flights",
            radioButtons("noOfFlightsBy", "Plot by", inline = TRUE, 
                         c("Airline" = "airline", "Hour" = "hour", "Day of the Week" = "day", "Flight Time" = "time", "Distance" = "distance")),
            plotlyOutput("flightDataNumberOfFlights")
          ),
          tabPanel(
            "Number of Delays",
            selectInput("top15", "Type", 
                        c("All Stacked", "All Combined", "Carrier Delay", "Weather Delay", "NAS Delay","Security Delay", "Late Aircraft Delay"),
                        width = '50%', selected = 'All Stacked'),
            plotlyOutput("flightDataNumberOfDelays")
          )
        ))
      ),#tabItem
      tabItem("compareAirports",
              fluidRow(column(width = 12, h2('Top 15'))),
              fluidRow(
                column(width = 4, selectInput("flightDataAirport1", "Airport 1", airportList, width = '100%', selected = 'ORD')),
                column(width = 4, selectInput("flightDataAirport2", "Airport 2", airportList, width = '100%', selected = 'MDW')),
                column(width = 1, checkboxInput("compareFlightData", "Compare")),
                column(width = 1, checkboxInput("stackedFlightData", "Stacked")),
                column(width = 1, offset = 1, actionButton("arrivalDepartureData", "Show Data"))),
              fluidRow(
                box(width = 12)
              )
      ),#tabItem
      tabItem("singleAirport",
              fluidRow(column(width = 12, h2('Single Airports'))),
              fluidRow(
                column(width = 4, selectInput("flightDataAirport1", "Source Airport", airportList, width = '100%', selected = 'ORD')),
                column(width = 4,  radioButtons("rb", "Select by", inline = TRUE, c("Top50","Airlines", "Date","Day")))
              ),
              fluidRow(
                column(width = 3, selectInput("top50", "Top50", c(), width = '100%')),
                column(width = 3, selectInput("airlinesBreakdown", "Airlines Breakdown", c(), width = '100%')),
                column(width = 3, dateInput("dateBreakdown", "Date Breakdown",value = "2017-01-01", min = "2017-01-01", max = "2017-12-31", width = '100%')),
                column(width = 3, selectInput("dayBreakdown", "Day Breakdown", 
                                              c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), 
                                              width = '100%'))
              ),
              fluidRow(
                box(width = 12)
              )
      ),#tabItem
      tabItem("states",
              fluidRow(
                box(title = "Flights to and from different states within US", width = 12)
              )
      )#tabItem 
)))

flightDataNoOfFlights <- function(pref, airport, stacked) {
  byGroup <- c("Month", switch(pref,
                               "airline" = 'AirlineID',
                               "hour" = 'ArrHour',
                               "day" = 'DayOfWeek',
                               "time" = 'CRSElapsedTimeGroup',
                               "distance" = 'DistanceGroup'))
  arrival1 <- count(subset(flights, Dest == airport), byGroup)
  colnames(arrival1) <- c(byGroup, "Frequency")
  departure1 <- count(subset(flights, Origin == airport), byGroup)
  colnames(departure1) <- c(byGroup, "Frequency")
  arrDep1 <- merge(arrival1, departure1, by = byGroup, all.x = TRUE, all.y = TRUE)
  arrDep1 <- merge(arrDep1, monthsDf, by.x = "Month",  by.y = "MonthNumber")
  arrDep1$MonthName <- ordered(arrDep1$MonthName, months)
  if(pref == 'airline') {
    arrDep1 <- merge(arrDep1, airlines, by = "AirlineID", all.x = TRUE)
  }
  if(pref == 'hour') {
    arrDep1 <- merge(arrDep1, hoursDf(), by.x = 'ArrHour', by.y = 'HourNumber')
    arrDep1$HourName <- ordered(arrDep1$HourName, hours())
  }
  if(pref == 'day') {
    arrDep1 <- merge(arrDep1, daysOfWeekDf, by.x = 'DayOfWeek', by.y = 'DayNumber')
    arrDep1$DayName <- ordered(arrDep1$DayName, daysOfWeek)
  }
  if(pref == 'time') {
    arrDep1 <- merge(arrDep1, timesDf, by.x = 'CRSElapsedTimeGroup', by.y = 'TimeNumber')
    arrDep1$TimeName <- ordered(arrDep1$TimeName, times)
  }
  if(pref == 'distance') {
    arrDep1 <- merge(arrDep1, distanceGroupDf(), by.x = 'DistanceGroup', by.y = 'DistanceNumber')
    arrDep1$DistanceName <- ordered(arrDep1$DistanceName, distanceGroups())
  }
  arrDep1[is.na(arrDep1)] <- 0
  
  txt <- switch(pref,
                "airline" = as.character(arrDep1$AirlineName),
                "hour" = arrDep1$HourName,
                "day" = arrDep1$DayName,
                "time" = arrDep1$TimeName,
                "distance" = arrDep1$DistanceName)
  
  return (plot_ly(
    x = switch(pref,
               "airline" = as.character(arrDep1$AirlineCode),
               "hour" = arrDep1$HourName,
               "day" = arrDep1$DayName,
               "time" = arrDep1$TimeName,
               "distance" = arrDep1$DistanceName), 
    y = arrDep1$Frequency.x, 
    frame = arrDep1$MonthName,
    text = paste(txt, '\nArrivals:', arrDep1$Frequency.x),
    hoverinfo = 'text',
    name = 'Arrivals',
    type = 'bar') %>%
    add_trace(y = arrDep1$Frequency.y,
              text = paste(txt, '\nDepartures:', arrDep1$Frequency.y),
              hoverinfo = 'text',
              name = 'Departures') %>%
    layout(barmode = if(stacked) 'stack'))
}

# server
server <- function(input, output){
  
  output$flightDataNumberOfFlights <- renderPlotly({
    subplot(
      flightDataNoOfFlights(input$noOfFlightsBy, input$flightDataAirport1, input$flightDataStacked),
      flightDataNoOfFlights(input$noOfFlightsBy, input$flightDataAirport2, input$flightDataStacked),
      shareY = input$flightDataCompare
    ) %>%
    layout(title = paste(input$flightDataAirport1, '- vs. -', input$flightDataAirport2))
  })
}

shinyApp(ui = ui, server = server)
