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
        fluidRow(column(width = 12, h2('Number of Arrivals, Departures and Delays'))),
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
  arrival <- count(subset(flights, Dest == airport), byGroup)
  colnames(arrival) <- c(byGroup, "Frequency")
  departure <- count(subset(flights, Origin == airport), byGroup)
  colnames(departure) <- c(byGroup, "Frequency")
  arrDep <- merge(arrival, departure, by = byGroup, all.x = TRUE, all.y = TRUE)
  arrDep <- merge(arrDep, monthsDf, by.x = "Month",  by.y = "MonthNumber")
  arrDep$MonthName <- ordered(arrDep$MonthName, months)
  if(pref == 'airline') {
    arrDep <- merge(arrDep, airlines, by = "AirlineID", all.x = TRUE)
  }
  if(pref == 'hour') {
    arrDep <- merge(arrDep, hoursDf(), by.x = 'ArrHour', by.y = 'HourNumber')
    arrDep$HourName <- ordered(arrDep$HourName, hours())
  }
  if(pref == 'day') {
    arrDep <- merge(arrDep, daysOfWeekDf, by.x = 'DayOfWeek', by.y = 'DayNumber')
    arrDep$DayName <- ordered(arrDep$DayName, daysOfWeek)
  }
  if(pref == 'time') {
    arrDep <- merge(arrDep, timesDf, by.x = 'CRSElapsedTimeGroup', by.y = 'TimeNumber')
    arrDep$TimeName <- ordered(arrDep$TimeName, times)
  }
  if(pref == 'distance') {
    arrDep <- merge(arrDep, distanceGroupDf(), by.x = 'DistanceGroup', by.y = 'DistanceNumber')
    arrDep$DistanceName <- ordered(arrDep$DistanceName, distanceGroups())
  }
  arrDep[is.na(arrDep)] <- 0
  
  txt <- switch(pref,
                "airline" = as.character(arrDep$AirlineName),
                "hour" = arrDep$HourName,
                "day" = arrDep$DayName,
                "time" = arrDep$TimeName,
                "distance" = arrDep$DistanceName)
  
  return (plot_ly(
    x = switch(pref,
               "airline" = as.character(arrDep$AirlineCode),
               "hour" = arrDep$HourName,
               "day" = arrDep$DayName,
               "time" = arrDep$TimeName,
               "distance" = arrDep$DistanceName), 
    y = arrDep$Frequency.x, 
    frame = arrDep$MonthName,
    text = paste(txt, '\nArrivals:', arrDep$Frequency.x),
    hoverinfo = 'text',
    name = 'Arrivals',
    type = 'bar') %>%
      add_trace(y = arrDep$Frequency.y,
                text = paste(txt, '\nDepartures:', arrDep$Frequency.y),
                hoverinfo = 'text',
                name = 'Departures') %>%
      layout(barmode = if(stacked) 'stack'))
}

flightDataNoOfDelays <- function(airport, stacked) {
  groupBy <- c('Month', 'ArrHour')

  delaysdata <- subset(flights, Dest == airport)
  delays_TotalFlights <- count(delaysdata, groupBy)
  colnames(delays_TotalFlights)<-c(groupBy, "Total")
  
  delays_NAS <- delaysdata[delaysdata$NASDelay!=0 & !is.na(delaysdata$NASDelay),]
  delays_NAS <- count(delays_NAS, groupBy)
  colnames(delays_NAS)<-c(groupBy, "NAS")
  
  delays_Security <- delaysdata[delaysdata$SecurityDelay!=0 & !is.na(delaysdata$SecurityDelay),]
  delays_Security <- count(delays_Security, groupBy)
  colnames(delays_Security)<-c(groupBy, "Security")
  
  delays_Weather <- delaysdata[delaysdata$WeatherDelay!=0 & !is.na(delaysdata$WeatherDelay),]
  delays_Weather <- count(delays_Weather, groupBy)
  colnames(delays_Weather)<-c(groupBy, "Weather")
  
  delays_Carrier <- delaysdata[delaysdata$CarrierDelay!=0 & !is.na(delaysdata$CarrierDelay),]
  delays_Carrier <- count(delays_Carrier, groupBy)
  colnames(delays_Carrier)<-c(groupBy, "Carrier")
  
  delays_LateAircraftDelay <- delaysdata[delaysdata$LateAircraftDelay!=0 & !is.na(delaysdata$LateAircraftDelay),]
  delays_LateAircraftDelay <- count(delays_LateAircraftDelay, groupBy)
  colnames(delays_LateAircraftDelay)<-c(groupBy, "LAD")
  
  delays_Combined <- delaysdata[
    (delaysdata$NASDelay!=0 & !is.na(delaysdata$NASDelay))
    |(delaysdata$SecurityDelay!=0 & !is.na(delaysdata$SecurityDelay))
    |(delaysdata$WeatherDelay!=0 & !is.na(delaysdata$WeatherDelay))
    |(delaysdata$CarrierDelay!=0 & !is.na(delaysdata$CarrierDelay))
    |(delaysdata$LateAircraftDelay!=0 & !is.na(delaysdata$LateAircraftDelay)),]
  delays_Combined <- count(delays_Combined, groupBy)
  colnames(delays_Combined)<-c(groupBy, "Combined")
  
  delays <- Reduce(function(x, y) merge(x, y, all=TRUE, by=groupBy), 
                   list(delays_Carrier, delays_LateAircraftDelay, delays_NAS, 
                        delays_Security, delays_Weather, delays_TotalFlights, delays_Combined))
  delays$Percent <- delays$Combined * 100.00 / delays$Total

  delays <- merge(delays, monthsDf, by.x = "Month",  by.y = "MonthNumber")
  delays$MonthName <- ordered(delays$MonthName, months)
  
  delays <- merge(delays, hoursDf(), by.x = 'ArrHour', by.y = 'HourNumber')
  delays$HourName <- ordered(delays$HourName, hours())

  delays[is.na(delays)] <- 0

  return(plot_ly(
    x = delays$HourName, 
    y = delays$NAS, 
    name = 'National Airspace System',
    frame = delays$MonthName, 
    type = 'bar'  ) %>% 
      add_trace(y = delays$Security,
                name = 'Security' ) %>% 
      add_trace(y = delays$Carrier,
                name = 'Carrier' ) %>% 
      add_trace(y = delays$LAD,
                name = 'Late Aircraft' ) %>% 
      add_trace(y = delays$Weather,
                name = 'Weather' ) %>%
      add_trace(
          y = delays$Percent,
          name = 'Percentage',
          type = 'scatter',
          mode = 'lines+markers',
          line = list(color = 'black')
          #yaxis = 'y2'
          ) %>%
      layout(
        barmode = if(stacked) 'stack'
        #yaxis2 = list(
        #  range = c(0, 120),
        #  overlaying = 'y',
        #  side = 'right',
        #  showline = FALSE,
        #  zeroline = FALSE,
        #  showgrid = FALSE,
        #  showticklabels = FALSE
        #)
      ))
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

  output$flightDataNumberOfDelays <- renderPlotly({
    subplot(
      flightDataNoOfDelays(input$flightDataAirport1, input$flightDataStacked),
      flightDataNoOfDelays(input$flightDataAirport2, input$flightDataStacked),
      shareY = input$flightDataCompare
    ) %>%
      layout(title = paste(input$flightDataAirport1, '- vs. -', input$flightDataAirport2))
  })
}

shinyApp(ui = ui, server = server)
