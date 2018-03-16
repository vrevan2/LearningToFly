library(shiny)
library(shinydashboard)
library(plotly)
library(plyr)
library(reshape2) ##??
library(DT)
library(shinyjs)
library(rgdal)
library(rgeos)
library(ggplot2)
library(maptools)
library(mapproj)

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
  return(if (is24Hour) c(0:23) else c(paste(c(0:11), "am"), "Noon", paste(c(1:11), "pm")))
}
hoursDf <- function() {
  hdf <- data.frame(hours(), c(0:23))
  colnames(hdf) <- c("HourName", "HourNumber")
  return(hdf)
}

distanceGroups <- function() {
  return(if (isMetric)
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
  return(htmltools::withTags(table(class = 'display', thead(
    tr(
      th(rowspan = 2, tableName),
      th(colspan = 2, airport1),
      th(colspan = 2, airport2)
    ), tr(lapply(rep(
      c('Arrivals', 'Departures'), 2
    ), th))
  ))))
}

tableHeaderTwoAirports <- function(airport1, airport2, tableName) {
  return(htmltools::withTags(table(class = 'display', thead(tr(
    th(tableName),
    th(airport1),
    th(airport2)
  )))))
}

tableHeaderOneAirport <- function(airport1, airport2, tableName) {
  return(htmltools::withTags(table(class = 'display', thead(tr(
    th(tableName),
    th(airport1),
    th(airport2)
  )))))
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

# flight By State
flightsFromIL <- count(subset(flights, OriginState == 'IL'), vars = 'DestState')
flightsFromIL$Percent <- as.numeric(format(round(
  flightsFromIL$freq * 100 / sum(flightsFromIL$freq), 2), nsmall = 2))
colnames(flightsFromIL) <- c('State', 'Flights', 'Percent')
flightsFromIL$Flights <- format(flightsFromIL$Flights, big.mark=",", scientific=FALSE)

flightsToIL <- count(subset(flights, DestState == 'IL'), vars = 'OriginState')
flightsToIL$Percent <- as.numeric(format(round(
  flightsToIL$freq * 100 / sum(flightsToIL$freq), 2), nsmall = 2))
colnames(flightsFromIL) <- c('State', 'Flights', 'Percent')
flightsToIL$Flights <- format(flightsToIL$Flights, big.mark=",", scientific=FALSE)

#Load the map
us <- readOGR(dsn = "data/us_states_hexgrid.geojson", layer = "us_states_hexgrid")
centers <- cbind.data.frame(data.frame(gCentroid(us, byid=TRUE), id=us@data$iso3166_2))
us_map <- fortify(us, region="iso3166_2")

# Layout
ui <- dashboardPage(
  dashboardHeader(title = 'R you Shiny'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Home', tabName = 'home'),
      menuItem('Compare 2 Airports', tabName = 'flightData'),
      menuItem('Deep Dive', tabName = 'deepDive'),
      menuItem('States', tabName = 'states')
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabItems(
      tabItem("home", includeHTML("home.html")),
      tabItem(
        "flightData", 
        fluidRow(column(width = 12, h2('Number of Flights, Delays and Top 15 Destinations'))),
        fluidRow(
          column(width = 4, selectInput("flightDataAirport1", "Airport 1", airportList, width = '100%', selected = 'ORD')),
          column(width = 4, selectInput("flightDataAirport2", "Airport 2", airportList, width = '100%', selected = 'MDW')),
          column(width = 1, checkboxInput("flightDataCompare", "Compare")),
          column(width = 1, checkboxInput("flightDataStacked", "Stacked_Bars")),
          column(width = 1, actionButton("flightDataTable", "Show Data"))
        ),
        fluidRow(tabBox(
          id = "flightDataTabs",
          width = 12,
          tabPanel(
            "Number of Flights",
            radioButtons("noOfFlightsBy", "Plot by", inline = TRUE, c("Airline" = "airline", "Hour" = "hour", "Day of the Week" = "day", "Flight Time" = "time", "Distance" = "distance")),
            plotlyOutput("flightDataNumberOfFlights", height = "60vh")),
          tabPanel(
            "Number of Delays",
            plotlyOutput("flightDataNumberOfDelays", height = "60vh")),
          tabPanel(
            "Top 15 Destinations",
            plotlyOutput("flightDataTop15Destinations", height = "60vh"))
        ))
      ),
      tabItem(
        "deepDive",
        fluidRow(column(width = 12, h2('Deep Dive for an Airport'))),
        fluidRow(
          column(width = 4, selectInput("flightDataAirport1", "Source Airport", airportList, width = '100%', selected = 'ORD')),
          column(width = 4, radioButtons("breakdownRadioButton", "Select by", inline = TRUE, c("Target Airport", "Airline", "Date", "Day of the Week"))),
          column(width = 4, 
                 selectInput("airportBreakdown", "Target Airport", c(), width = '100%'), 
                 selectInput("airlineBreakdown", "Airline", c(), width = '100%'),
                 dateInput("dateBreakdown", "Date", value = "2017-01-01", min = "2017-01-01", max = "2017-12-31", width = '100%'),
                 selectInput("dayBreakdown", "Day of the Week", daysOfWeek, width = '100%'))),
        fluidRow(box(width = 12))
      ),
      tabItem(
        "states",
        fluidRow(
          box(title = "Flights from IL to different states within US", width = 12, plotlyOutput('mapFlightsFromIL')),
          box(title = "Flights to IL from different states within US", width = 12, plotlyOutput('mapFlightsToIL'))
        )
      )
)))

flightDataNoOfFlights <- function(pref, airport, stacked) {
  byGroup <- c("Month", switch(
    pref,
    "airline" = 'AirlineID',
    "hour" = 'ArrHour',
    "day" = 'DayOfWeek',
    "time" = 'CRSElapsedTimeGroup',
    "distance" = 'DistanceGroup'
  ))
  
  arrival <- count(subset(flights, Dest == airport), byGroup)
  colnames(arrival) <- c(byGroup, "Frequency")
  departure <- count(subset(flights, Origin == airport), byGroup)
  colnames(departure) <- c(byGroup, "Frequency")
  arrDep <- merge(arrival, departure, by = byGroup, all.x = TRUE, all.y = TRUE)
  arrDep <- merge(arrDep, monthsDf, by.x = "Month",  by.y = "MonthNumber")
  arrDep$MonthName <- ordered(arrDep$MonthName, months)
  if (pref == 'airline') {
    arrDep <- merge(arrDep, airlines, by = "AirlineID", all.x = TRUE)
  }
  if (pref == 'hour') {
    arrDep <- merge(arrDep, hoursDf(), by.x = 'ArrHour', by.y = 'HourNumber')
    arrDep$HourName <- ordered(arrDep$HourName, hours())
  }
  if (pref == 'day') {
    arrDep <- merge(arrDep, daysOfWeekDf, by.x = 'DayOfWeek', by.y = 'DayNumber')
    arrDep$DayName <- ordered(arrDep$DayName, daysOfWeek)
  }
  if (pref == 'time') {
    arrDep <- merge(arrDep, timesDf, by.x = 'CRSElapsedTimeGroup', by.y = 'TimeNumber')
    arrDep$TimeName <- ordered(arrDep$TimeName, times)
  }
  if (pref == 'distance') {
    arrDep <- merge(arrDep, distanceGroupDf(), by.x = 'DistanceGroup', by.y = 'DistanceNumber')
    arrDep$DistanceName <- ordered(arrDep$DistanceName, distanceGroups())
  }
  arrDep[is.na(arrDep)] <- 0
  
  xCol <- switch(
    pref,
    "airline" = as.character(arrDep$AirlineCode),
    "hour" = arrDep$HourName,
    "day" = arrDep$DayName,
    "time" = arrDep$TimeName,
    "distance" = arrDep$DistanceName
  )
  
  txt <- switch(
    pref,
    "airline" = as.character(arrDep$AirlineName),
    "hour" = arrDep$HourName,
    "day" = arrDep$DayName,
    "time" = arrDep$TimeName,
    "distance" = arrDep$DistanceName
  )
  
  return (
    plot_ly(
      x = xCol,
      y = arrDep$Frequency.x,
      frame = arrDep$MonthName,
      text = paste(txt, '\nArrivals:', arrDep$Frequency.x),
      hoverinfo = 'text',
      name = paste(airport, 'Arrivals'),
      type = 'bar'
    ) %>%
      add_trace(
        y = arrDep$Frequency.y,
        text = paste(txt, '\nDepartures:', arrDep$Frequency.y),
        hoverinfo = 'text',
        name = paste(airport, 'Departures')
      ) %>%
      layout(
        barmode = if (stacked) 'stack',
        hovermode = 'compare')
  )
}

flightDataNoOfDelays <- function(airport, stacked) {
  groupBy <- c('Month', 'ArrHour')
  
  delaysdata <- subset(flights, Dest == airport)
  delays_TotalFlights <- count(delaysdata, groupBy)
  colnames(delays_TotalFlights) <- c(groupBy, "Total")
  
  delays_NAS <- delaysdata[delaysdata$NASDelay != 0 & !is.na(delaysdata$NASDelay),]
  delays_NAS <- count(delays_NAS, groupBy)
  colnames(delays_NAS) <- c(groupBy, "NAS")
  
  delays_Security <- delaysdata[delaysdata$SecurityDelay != 0 & !is.na(delaysdata$SecurityDelay),]
  delays_Security <- count(delays_Security, groupBy)
  colnames(delays_Security) <- c(groupBy, "Security")
  
  delays_Weather <- delaysdata[delaysdata$WeatherDelay != 0 & !is.na(delaysdata$WeatherDelay),]
  delays_Weather <- count(delays_Weather, groupBy)
  colnames(delays_Weather) <- c(groupBy, "Weather")
  
  delays_Carrier <- delaysdata[delaysdata$CarrierDelay != 0 & !is.na(delaysdata$CarrierDelay),]
  delays_Carrier <- count(delays_Carrier, groupBy)
  colnames(delays_Carrier) <- c(groupBy, "Carrier")
  
  delays_LateAircraftDelay <- delaysdata[delaysdata$LateAircraftDelay != 0 & !is.na(delaysdata$LateAircraftDelay),]
  delays_LateAircraftDelay <- count(delays_LateAircraftDelay, groupBy)
  colnames(delays_LateAircraftDelay) <- c(groupBy, "LAD")
  
  delays_Combined <- delaysdata[(delaysdata$NASDelay != 0 & !is.na(delaysdata$NASDelay))
                                | (delaysdata$SecurityDelay != 0 & !is.na(delaysdata$SecurityDelay))
                                | (delaysdata$WeatherDelay != 0 & !is.na(delaysdata$WeatherDelay))
                                | (delaysdata$CarrierDelay != 0 & !is.na(delaysdata$CarrierDelay))
                                | (delaysdata$LateAircraftDelay != 0 & !is.na(delaysdata$LateAircraftDelay)),]
  delays_Combined <- count(delays_Combined, groupBy)
  colnames(delays_Combined) <- c(groupBy, "Combined")
  
  delays <- Reduce( function(x, y) merge(x, y, all = TRUE, by = groupBy),
      list(delays_Carrier, delays_LateAircraftDelay, delays_NAS, delays_Security,
        delays_Weather, delays_TotalFlights, delays_Combined))
  delays$Percent <- delays$Combined * 100.00 / delays$Total
  
  delays <- merge(delays, monthsDf, by.x = "Month",  by.y = "MonthNumber")
  delays$MonthName <- ordered(delays$MonthName, months)
  
  delays <- merge(delays, hoursDf(), by.x = 'ArrHour', by.y = 'HourNumber')
  delays$HourName <- ordered(delays$HourName, hours())
  
  delays[is.na(delays)] <- 0
  
  return(
    plot_ly(
      x = delays$HourName,
      y = delays$NAS,
      name = paste(airport, 'National Airspace System'),
      frame = delays$MonthName,
      type = 'bar'
    ) %>%
      add_trace(y = delays$Security, name = paste(airport, 'Security')) %>%
      add_trace(y = delays$Carrier, name = paste(airport, 'Carrier')) %>%
      add_trace(y = delays$LAD, name = paste(airport, 'Late Aircraft')) %>%
      add_trace(y = delays$Weather, name = paste(airport, 'Weather')) %>%
      add_trace(
        y = delays$Percent,
        name = paste(airport, 'Percentage'),
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = 'black')
        #yaxis = 'y2'
      ) %>%
      layout(barmode = if (stacked) 'stack',
        hovermode = 'compare'
        #yaxis2 = list(
        #  range = c(0, 120),
        #  overlaying = 'y',
        #  side = 'right',
        #  showline = FALSE,
        #  zeroline = FALSE,
        #  showgrid = FALSE,
        #  showticklabels = FALSE
        #)))))
      ))
}

top15Airports <- function(airport, stacked) {
  airportData <- flights[flights$Origin == airport | flights$Dest == airport, ]
  airportData$DestinationAirport[airportData$Origin == airport] <- as.character(airportData$Dest[airportData$Origin == airport])
  airportData$DestinationAirport[airportData$Dest == airport] <- as.character(airportData$Origin[airportData$Dest == airport])
  top15Airports <- count(airportData, 'DestinationAirport') %>% arrange(desc(freq)) %>% head(15)
  top15AirportData <- subset(airportData, DestinationAirport %in% top15Airports$DestinationAirport)
  
  groupBy <- c('Month', 'DestinationAirport')
  counts <- count(top15AirportData, groupBy) # Total Flights
  counts <- merge(counts, count(top15AirportData[airportData$Dest == airport, ], groupBy), by = groupBy, all = TRUE) # Arrivals
  counts <- merge(counts, count(top15AirportData[airportData$Origin == airport, ], groupBy), by = groupBy, all = TRUE) # Departures
  colnames(counts) <- c(groupBy, 'Total_Flights', 'Arrivals', 'Departures')
  counts <- merge(counts, airports, by.x = 'DestinationAirport', by.y = 'IATA')
  counts <- merge(counts, monthsDf, by.x = "Month",  by.y = "MonthNumber")
  counts$MonthName <- ordered(counts$MonthName, months)
  
  return(plot_ly(
    x = counts$DestinationAirport,
    y = counts$Arrivals,
    frame = counts$MonthName,
    text = paste(counts$AirportName, '\nArrivals:', counts$Arrivals),
    hoverinfo = 'text',
    name = paste('Arrivals to', airport),
    type = 'bar'
  ) %>%
    add_trace(
      y = counts$Departures,
      text = paste(counts$AirportName, '\nDepartures:', counts$Departures),
      hoverinfo = 'text',
      name = paste('Departures from', airport)
    ) %>%
    layout(
      barmode = if (stacked) 'stack',
      hovermode = 'compare'))
}

getMap <- function(mapData, useOriginState) {
  return(ggplot()
         + geom_text(data = centers,
                     aes(label = id, x = x, y = y),
                     color = "black", size = 4)
         + geom_map(data = mapData, map = us_map, alpha = 0.5,
                    aes(map_id = State, fill = Percent, data = Flights))
         + scale_fill_distiller(palette = "Spectral", na.value = "red")
         + coord_map()
         + coord_fixed(ratio = 10)
         + labs(x = NULL, y = NULL)
         + theme_bw()
         + theme(panel.border = element_blank(), panel.grid = element_blank(),
                 axis.ticks = element_blank(), axis.text = element_blank()))
}

getTop50Airports <- function(srcAirport) {
  return(merge(flights[flights$Origin == srcAirport,] %>% count(c('Dest')), 
               flights[flights$Dest == srcAirport,] %>% count(c('Origin')), 
               by.x = 'Dest', by.y = 'Origin', all = TRUE) %>% 
           mutate(total = freq.x + freq.y) %>% 
           arrange(desc(total)) %>% 
           head(50) %>% 
           'colnames<-'(c("Airport", "Dep", "Arr", "total")))
}

# server
server <- function(input, output) {
  observe({
    hide("airportBreakdown")
    hide("airlineBreakdown")
    hide("dateBreakdown")
    hide("dayBreakdown")
    
    shinyjs::show(switch(
      input$breakdownRadioButton,
      "Target Airport" = "airportBreakdown",
      "Airline" = "airlineBreakdown",
      "Date" = "dateBreakdown",
      "Day of the Week" = "dayBreakdown"
    ))
  })
  
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
  
  output$flightDataTop15Destinations <- renderPlotly({
    subplot(
      top15Airports(input$flightDataAirport1, input$flightDataStacked),
      top15Airports(input$flightDataAirport2, input$flightDataStacked),
      shareY = input$flightDataCompare
    ) %>%
      layout(title = paste(input$flightDataAirport1, '- vs. -', input$flightDataAirport2))
  })
  
  output$mapFlightsFromIL <- renderPlotly({ getMap(flightsFromIL) })
  output$mapFlightsToIL <- renderPlotly({ getMap(flightsToIL) })
}

shinyApp(ui = ui, server = server)
