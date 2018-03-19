library(shiny)
library(shinydashboard)
library(plotly)
library(plyr)
library(reshape2)
library(DT)
library(shinyjs)
library(rgdal)
library(rgeos)
library(leaflet)

# Colors
arrivalColor <- '#6a819d'
departureColor <- '#e76e48'
colorScale <- c('#A7CAF2', '#262E38')

plotLabelSize <- 12
plotMarginTop <- 40
plotMarginLeft <- 10
plotMarginRight <- 10
plotMarginBottom <- 10

# Map
zoomLevel <- 5
lineOpacity <- 6
lineWeight <- 2
zoomLevelStates <- 3

daysOfWeek <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
daysOfWeekDropDown <- c(1:7)
names(daysOfWeekDropDown) <- daysOfWeek
daysOfWeekDf <- data.frame(daysOfWeek, c(1:7))
colnames(daysOfWeekDf) <- c('DayName', 'DayNumber')

months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
monthsDf <- data.frame(months, c(1:12))
colnames(monthsDf) <- c('MonthName', 'MonthNumber')

times <- c(paste(c(0:4), 'to', c(1:5), 'hr'), '5 hr or more')
timesDf <- data.frame(times, c(0:5))
colnames(timesDf) <- c('TimeName', 'TimeNumber')

hours <- function(is24Hour) {
  return(if (is24Hour) paste(c(0:23), 'hr') else c('Midnight', paste(c(1:11), 'am'), 'Noon', paste(c(1:11), 'pm')))
}
hoursDf <- function(is24Hour) {
  hdf <- data.frame(hours(is24Hour), c(0:23))
  colnames(hdf) <- c('HourName', 'HourNumber')
  return(hdf)
}

distanceGroups <- function(isMetric) {
  values <- (if (isMetric)
    c(paste('~', c(1:8) * 400, 'km'), '> 3200 km')
    else
      c(paste('~', c(1:8) * 250, 'mi'), '> 2000 mi'))
  
  values[1] <- paste('0', values[1])
  return(values)
}
distanceGroupDf <- function(isMetric) {
  dgdf <- data.frame(distanceGroups(isMetric), c(0:8))
  colnames(dgdf) <- c('DistanceName', 'DistanceNumber')
  return(dgdf)
}

temperature <- function(x, inMetric) {
  return (if (inMetric) 
      paste(round(x / 10.0, 1), '&deg;C')
    else 
      paste(round(((x / 10.0) * 9 / 5) + 32, 1), '&deg;F'))
}

distance <- function(x, inMetric) {
  return (if (inMetric)
      paste(round(x, 0), 'mm')
    else
      paste(round(x * 0.039370, 1), 'in'))
}

tableHeaderTwoAirports <- function(airport1, airport2, tableName, tabletype) {
  colName<-switch(
    tableName,
    'airline' = 'Airline',
    'hour' = 'Hour', # Use ArrHour or DepHour appropriately
    'day' = 'Day',
    'time' = 'Flight Time',
    'distance' = 'Distance',
    'top15' = 'Rank'
  )
  subheaders<-switch(
    tabletype,
    'noOfFlights' = c('Arrivals', 'Departures'),
    'delays' = c('Carrier', 'Late Arr.', 'NAS', 'Security', 'Weather', 'Total', 'Percent' ),
    'top15' = c('Airport','Arrivals', 'Departures')
  )
  return(htmltools::withTags(table(class = 'display', thead(class = "center",
    tr(
      th(rowspan = 2, colName),
      th(colspan = as.numeric(length(subheaders)), airport1),
      th(colspan = as.numeric(length(subheaders)), airport2)
    ), tr(lapply(rep(
      subheaders, 2
    ), th))
  ))))
}

tableHeaderOneAirport <- function(airport1, airport2, tableName) {
  return(htmltools::withTags(table(class = 'display', thead(tr(
    th(tableName),
    th(airport1),
    th(airport2)
  )))))
}

# Airline Info
airlines <- read.csv('data/airlines.csv', na.strings = '-')
rownames(airlines) <- as.character(airlines$AirlineCode)

# List of airlines for use in dropdowns
getAirlineCodePlusNames <- function(airlineCodes) {
  airlineCodes <- as.character(airlineCodes)
  airlineList <- airlineCodes[order(airlineCodes)]
  names(airlineList) <- sapply(airlineList, function(x) paste(x, '-', airlines[x == airlines$AirlineCode, ]$AirlineName))
  return(airlineList)
}

# Top Airlines for a Source Airport
getTopAirlines <- function(airport, noOfAirlines) {
  airportData <- flights[flights$Origin == airport | flights$Dest == airport, ]
  topAirlines <- count(airportData, 'AirlineID') %>% arrange(desc(freq)) %>% head(noOfAirlines) %>% merge(airlines, by = 'AirlineID')
  return(topAirlines$AirlineCode)
}

# Airport Info
airports <- read.csv('data/airports_stations.csv')
rownames(airports) <- as.character(airports$IATA)

# List of airports for use in dropdowns
getAirportCodePlusNames <- function(airportCodes) {
  airportCodes <- as.character(airportCodes)
  airportList <- airportCodes[order(airportCodes)]
  names(airportList) <- sapply(airportList, function(x) paste(x, '-', airports[x == airports$IATA, ]$AirportName))
  return(airportList)
}

# Choices for Airport Dropdown
airportList <- getAirportCodePlusNames(read.csv('data/illinois_airports.csv')$IATA)

# Top Destinations for a Source Airport
getTopDestinations <- function(airport, noOfDestinations, getFlightData) {
  airportData <- flights[flights$Origin == airport | flights$Dest == airport, ]
  airportData$DestinationAirport[airportData$Origin == airport] <- as.character(airportData$Dest[airportData$Origin == airport])
  airportData$DestinationAirport[airportData$Dest == airport] <- as.character(airportData$Origin[airportData$Dest == airport])
  topAirports <- count(airportData, 'DestinationAirport') %>% arrange(desc(freq)) %>% head(noOfDestinations)

  if(getFlightData)
    return(subset(airportData, DestinationAirport %in% topAirports$DestinationAirport))
  else
    return(topAirports)
}

# Flight Data
flights <- read.csv('data/OTP_2017.csv')
flights$DistanceGroup <- flights$DistanceGroup - 1 # convert 1:n to 0:n-1
flights$CRSElapsedTimeGroup[flights$CRSElapsedTimeGroup > 5] <- 5 # bin times more than 5 hrs into one group
flights$DistanceGroup[flights$DistanceGroup > 8] <- 8 # bin times more than 2000 miles into one group
cancellations <- flights[flights$CancellationCode != '', ]
flights <- flights[flights$CancellationCode == '', ]

# Weather Data
weather <- read.csv("data/mapweatherfull.csv")
originState = "IL"

# Flights By State
flightsFromIL <- count(subset(flights, OriginState == 'IL'), vars = 'DestState')
colnames(flightsFromIL) <- c('iso3166_2', 'Departures')
flightsToIL <- count(subset(flights, DestState == 'IL'), vars = 'OriginState')
colnames(flightsToIL) <- c('iso3166_2', 'Arrivals')
flightsIL <- merge(flightsToIL, flightsFromIL, by = 'iso3166_2', all = T)
flightsIL <- flightsIL %>% mutate('percentArr' = Arrivals * 100 / sum(flightsIL$Arrivals), percentDep = Departures * 100 / sum(flightsIL$Departures), total = Arrivals+Departures)
flightsIL$Arrivals <- format(flightsIL$Arrivals, big.mark = ',', scientific = FALSE)
flightsIL$Departures <- format(flightsIL$Departures, big.mark = ',', scientific = FALSE)

# Layout
ui <- function() {
  dashboardPage(
  dashboardHeader(title = 'Learning to Fly'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Home', tabName = 'preferences'),
      menuItem('Compare Airports', tabName = 'flightData'),
      menuItem('Deep Dive', tabName = 'deepDive'),
      menuItem('States', tabName = 'states'),
      menuItem(
        'Preferences',
        radioButtons('measurements', 'Measurements', inline = TRUE, c('Imperial', 'Metric')),
        radioButtons('timeFormat', 'Time Format', inline = TRUE, c('12 hr', '24 hr')),
        br(),
        startExpanded = TRUE)
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
      tags$script('
        var dimension = [0, 0];
        $(document).on("shiny:connected", function(e) {
          dimension[0] = window.innerWidth;
          dimension[1] = window.innerHeight;
          Shiny.onInputChange("dimension", dimension);
        });
        $(window).resize(function(e) {
          dimension[0] = window.innerWidth;
          dimension[1] = window.innerHeight;
          Shiny.onInputChange("dimension", dimension);
        });'
    )),
    tabItems(
      tabItem('insights', includeHTML('insights.html')),
      tabItem(
        'preferences',
        fluidRow(
          column(width = 4, h2('About'),
            p('Interactive Visualization of Airline Flights in Illinois'),
            a('Project Page', href = 'https://vrevan2.github.io/LearningToFly/', target = '_blank'),
            h2('Team "R you Shiny"'),
            p('Amey Barapatre, Jaspreet Kaur Sohal, Sai Phaltankar and Vivek R. Shivaprabhu')
        ))
      ),
      tabItem(
        'flightData',
        fluidRow(column(width = 12, h2('Number of Flights, Delays and Top 15 Destinations in 2017'))),
        fluidRow(
          column(width = 4, selectInput('flightDataAirport1', 'Airport 1', airportList, width = '100%', selected = 'ORD')),
          column(width = 4, selectInput('flightDataAirport2', 'Airport 2', airportList, width = '100%', selected = 'MDW')),
          column(width = 1, checkboxInput('flightDataStacked', 'Stacked_Bars')),
          column(width = 1, checkboxInput('flightDataCompare', 'Compare')),
          column(width = 1, actionButton('flightDataTable', 'Toggle Data'))
        ),
        fluidRow(tabBox(
          id = 'flightDataTabs',
          width = 12,
          tabPanel(
            'Number of Flights',
            radioButtons('noOfFlightsBy', 'Plot by', inline = TRUE, c('Airline' = 'airline', 'Hour' = 'hour', 'Day of the Week' = 'day', 'Flight Time' = 'time', 'Distance' = 'distance')),
            selectInput('monthBreakdownFlights', 'Month', months, width = '50%'),
            plotlyOutput('flightDataNumberOfFlights', height = '60vh'),
            DT::dataTableOutput("flightDataNumberOfFlightsTable")
            ),
          tabPanel(
            'Number of Delays',
            plotlyOutput('flightDataNumberOfDelays', height = '70vh'),
            selectInput('monthBreakdownDelays', 'Month', months, width = '50%'),
            DT::dataTableOutput("flightDataNumberOfDelaysTable")),
          tabPanel(
            'Top 15 Destinations',
            plotlyOutput('flightDataTop15Destinations', height = '70vh'),
            selectInput('monthBreakdownTop15', 'Month', months, width = '50%'),
            DT::dataTableOutput("flightDataTop15DestinationsTable"))
          
        ))
      ),
      tabItem(
        'deepDive',
        fluidRow(column(width = 12, h2('Deep Dive for an Airport in 2017'))),
        fluidRow(
          column(width = 3, selectInput('breakdownSourceAirport', 'Source Airport', airportList, width = '100%', selected = 'ORD')),
          column(width = 4, radioButtons('breakdownRadioButton', 'Select by', inline = TRUE, 
            c('Date' = 'date', 'Target Airport' = 'airport', 'Airline' = 'airline', 'Day of the Week' = 'day'))),
          column(width = 3,
            selectInput('airportBreakdown', 'Target Airport', c(), width = '100%'),
            selectInput('airlineBreakdown', 'Airline', c(), width = '100%'),
            dateInput('dateBreakdown', 'Date', value = as.Date(format(Sys.Date(), '2017-%m-%d')), min = '2017-01-01', max = '2017-12-31', width = '100%'),

            selectInput('dayBreakdown', 'Day of the Week', daysOfWeekDropDown, width = '100%')),
          column(width = 2, radioButtons('breakdownPlotType', 'Plot Type', inline = TRUE, choiceValues = c('map', 'graph'), choiceNames = c('Map', 'Graph')))
        ),
        fluidRow(id = 'deepDiveGraph', box(width = 12, title = 'Number of Flights', plotlyOutput('deepDivePlots', height = '60vh'))),
        fluidRow(id = 'deepDiveMap', box(width = 12,
          radioButtons('deepDiveMapDirection', 'Show flights', c('Leaving IL' = 'departures', 'Entering IL' = 'arrivals'), inline = TRUE),
          p('Click on airports (circles) to view weather for the day. Hover over the flight-paths (lines) to view number of flights and cancellations.'),
          leafletOutput('deepDiveLeaflet')))
      ),
      tabItem(
        'states',
        fluidRow(
          box(title = 'Flights between IL and different states within US in 2017', leafletOutput('mapFlightsToFromIL'), width = 12)
        )
      )
)))
}

flightDataNoOfFlightsPlot <- function(pref, airport, stacked, is24Hour, isMetric) {
  arrDep <- flightDataNoOfFlightsDataFrame(pref, airport, is24Hour, isMetric)

  xCol <- switch(
    pref,
    'airline' = as.character(arrDep$AirlineCode),
    'hour' = arrDep$HourName,
    'day' = arrDep$DayName,
    'time' = arrDep$TimeName,
    'distance' = arrDep$DistanceName
  )

  txt <- switch(
    pref,
    'airline' = as.character(arrDep$AirlineName),
    'hour' = arrDep$HourName,
    'day' = arrDep$DayName,
    'time' = arrDep$TimeName,
    'distance' = arrDep$DistanceName
  )

  return (
    plot_ly(
      x = xCol,
      y = arrDep$Frequency.x,
      frame = arrDep$MonthName,
      text = paste(txt, '\nArrivals:', arrDep$Frequency.x),
      hoverinfo = 'text',
      name = paste(airport, 'Arrivals'),
      type = 'bar',
      marker = list(color = arrivalColor)
    ) %>%
      add_trace(
        y = arrDep$Frequency.y,
        text = paste(txt, '\nDepartures:', arrDep$Frequency.y),
        hoverinfo = 'text',
        name = paste(airport, 'Departures'),
        marker = list(color = departureColor)
      ) %>%

      layout(title = airport, barmode = if (stacked) 'stack', hovermode = 'compare', font = list(size = plotLabelSize), margin = list(t = plotMarginTop, b = plotMarginBottom),
             yaxis = list(range = c(0, max(max(arrDep$Frequency.x), max(arrDep$Frequency.y)))))
  )
}

flightDataNoOfFlightsDataFrame <- function(pref, airport, is24Hour, isMetric) {
  byGroup <- c('Month', switch(
    pref,
    'airline' = 'AirlineID',
    'hour' = 'Hour', # Use ArrHour or DepHour appropriately
    'day' = 'DayOfWeek',
    'time' = 'CRSElapsedTimeGroup',
    'distance' = 'DistanceGroup'
  ))
  
  arrival <- count(subset(flights, Dest == airport), if(pref == 'hour') c('Month', 'ArrHour') else byGroup)
  colnames(arrival) <- c(byGroup, 'Frequency')
  
  departure <- count(subset(flights, Origin == airport), if(pref == 'hour') c('Month', 'DepHour') else byGroup)
  colnames(departure) <- c(byGroup, 'Frequency')
  
  arrDep <- merge(arrival, departure, by = byGroup, all = TRUE)
  
  if (pref == 'airline') {
    uniques <- expand.grid(unique(arrDep$AirlineID), c(1:12))
    cols <- c('AirlineID', 'Month')
    colnames(uniques) <- cols
    arrDep <- merge(arrDep, uniques, by = cols, all = TRUE)
    arrDep <-
      merge(arrDep, airlines, by = 'AirlineID', all.x = TRUE)
  } else
  if (pref == 'hour') {
    uniques <- expand.grid(c(0:23), c(1:12))
    cols <- c('Hour', 'Month')
    colnames(uniques) <- cols
    arrDep <- merge(arrDep, uniques, by = cols, all = TRUE)
    arrDep <-
      merge(
        arrDep,
        hoursDf(is24Hour),
        by.x = 'Hour',
        by.y = 'HourNumber',
        all = TRUE
      )
    arrDep$HourName <- ordered(arrDep$HourName, hours(is24Hour))
  } else
  if (pref == 'day') {
    uniques <- expand.grid(c(1:7), c(1:12))
    cols <- c('DayOfWeek', 'Month')
    colnames(uniques) <- cols
    arrDep <- merge(arrDep, uniques, by = cols, all = TRUE)
    arrDep <-
      merge(arrDep,
            daysOfWeekDf,
            by.x = 'DayOfWeek',
            by.y = 'DayNumber',
            all = TRUE)
    arrDep$DayName <- ordered(arrDep$DayName, daysOfWeek)
  } else
  if (pref == 'time') {
    uniques <- expand.grid(c(0:5), c(1:12))
    cols <- c('CRSElapsedTimeGroup', 'Month')
    colnames(uniques) <- cols
    arrDep <- merge(arrDep, uniques, by = cols, all = TRUE)
    arrDep <-
      merge(arrDep,
            timesDf,
            by.x = 'CRSElapsedTimeGroup',
            by.y = 'TimeNumber',
            all = TRUE)
    arrDep$TimeName <- ordered(arrDep$TimeName, times)
  } else
  if (pref == 'distance') {
    uniques <- expand.grid(c(0:8), c(1:12))
    cols <- c('DistanceGroup', 'Month')
    colnames(uniques) <- cols
    arrDep <- merge(arrDep, uniques, by = cols, all = TRUE)
    arrDep <-
      merge(
        arrDep,
        distanceGroupDf(isMetric),
        by.x = 'DistanceGroup',
        by.y = 'DistanceNumber',
        all = TRUE
      )
    arrDep$DistanceName <-
      ordered(arrDep$DistanceName, distanceGroups(isMetric))
  }
  arrDep[is.na(arrDep)] <- 0
  arrDep <- merge(arrDep, monthsDf, by.x = 'Month', by.y = 'MonthNumber', all = TRUE)
  arrDep$MonthName <- ordered(arrDep$MonthName, months)
  return(arrDep)
}

flightDataNoOfFlightsTable <- function(airport1, airport2, pref, month, is24Hour, isMetric) {
  colName<-switch(
    pref,
    'airline' = 'AirlineCode',
    'hour' = 'HourName', # Use ArrHour or DepHour appropriately
    'day' = 'DayName',
    'time' = 'TimeName',
    'distance' = 'DistanceName'
  )
  monthNo <- as.numeric(subset(monthsDf, MonthName == month)$MonthNumber)
  
  arrDep1 <- flightDataNoOfFlightsDataFrame(pref, airport1, is24Hour, isMetric)
  arrDep2 <- flightDataNoOfFlightsDataFrame(pref, airport2, is24Hour, isMetric)
  
  arrDep <- merge(arrDep1, arrDep2, by = c(colName, 'Month'), all = T)
  if(pref == 'airline') {
    arrDep$AirlineCode <- names(getAirlineCodePlusNames(arrDep$AirlineCode))
  }
  arrDep <- arrDep[,c(colName, 'Frequency.x.x', 'Frequency.y.x', 'Frequency.x.y', 'Frequency.y.y', 'Month')]
  arrDep[is.na(arrDep)] <- 0
  return(subset(arrDep[arrDep$Month == monthNo,], select = -c(Month)))
}

flightDataNoOfDelaysPlot <- function(airport, stacked, is24Hour) {
  delays<- flightDataNoOfDelaysDataFrame(airport, is24Hour)
  return(
    plot_ly(
      x = delays$HourName, 
      y = delays$NAS, 
      name = paste(airport, 'National Airspace System'), 
      frame = delays$MonthName, 
      type = 'bar',
      marker = list(color = '#4B7C8C')
    ) %>%
      add_trace(y = delays$Security, name = paste(airport, 'Security'), marker = list(color = '#002d58')) %>%
      add_trace(y = delays$Carrier, name = paste(airport, 'Carrier'), marker = list(color = '#EAC949')) %>%
      add_trace(y = delays$LAD, name = paste(airport, 'Late Aircraft'), marker = list(color = '#E47F7B')) %>%
      add_trace(y = delays$Weather, name = paste(airport, 'Weather'), marker = list(color = '#84BCBE')) %>%
      add_trace(
        y = delays$Percent, 
        name = paste(airport, 'Percentage'), 
        type = 'scatter', 
        mode = 'lines+markers', 
        line = list(color = 'black'),
        marker = list(color = 'black')
        #yaxis = 'y2'
      ) %>%
      layout(barmode = if (stacked) 'stack', hovermode = 'compare', font = list(size = plotLabelSize), margin = list(t = plotMarginTop, b = plotMarginBottom)
        #yaxis2 = list(
        #  range = c(0, 120),
        #  overlaying = 'y',
        #  side = 'right',
        #  showline = FALSE,
        #  zeroline = FALSE,
        #  showgrid = FALSE,
        #  showticklabels = FALSE
        #)))))
      ) %>%
      animation_opts(4000, transition = 3000, easing = "elastic")
    )
}

flightDataNoOfDelaysDataFrame<- function(airport, is24Hour) {
  groupBy <- c('Month', 'ArrHour')
  
  delaysdata <- subset(flights, Dest == airport)
  delays_TotalFlights <- count(delaysdata, groupBy)
  colnames(delays_TotalFlights) <- c(groupBy, 'Total')
  
  delays_NAS <- delaysdata[delaysdata$NASDelay != 0 & !is.na(delaysdata$NASDelay), ]
  delays_NAS <- count(delays_NAS, groupBy)
  colnames(delays_NAS) <- c(groupBy, 'NAS')
  
  delays_Security <- delaysdata[delaysdata$SecurityDelay != 0 & !is.na(delaysdata$SecurityDelay), ]
  delays_Security <- count(delays_Security, groupBy)
  colnames(delays_Security) <- c(groupBy, 'Security')
  
  delays_Weather <- delaysdata[delaysdata$WeatherDelay != 0 & !is.na(delaysdata$WeatherDelay), ]
  delays_Weather <- count(delays_Weather, groupBy)
  colnames(delays_Weather) <- c(groupBy, 'Weather')
  
  delays_Carrier <- delaysdata[delaysdata$CarrierDelay != 0 & !is.na(delaysdata$CarrierDelay), ]
  delays_Carrier <- count(delays_Carrier, groupBy)
  colnames(delays_Carrier) <- c(groupBy, 'Carrier')
  
  delays_LateAircraftDelay <- delaysdata[delaysdata$LateAircraftDelay != 0 & !is.na(delaysdata$LateAircraftDelay), ]
  delays_LateAircraftDelay <- count(delays_LateAircraftDelay, groupBy)
  colnames(delays_LateAircraftDelay) <- c(groupBy, 'LAD')
  
  delays_Combined <- delaysdata[(delaysdata$NASDelay != 0 & !is.na(delaysdata$NASDelay))
                                | (delaysdata$SecurityDelay != 0 & !is.na(delaysdata$SecurityDelay))
                                | (delaysdata$WeatherDelay != 0 & !is.na(delaysdata$WeatherDelay))
                                | (delaysdata$CarrierDelay != 0 & !is.na(delaysdata$CarrierDelay))
                                | (delaysdata$LateAircraftDelay != 0 & !is.na(delaysdata$LateAircraftDelay)), ]
  delays_Combined <- count(delays_Combined, groupBy)
  colnames(delays_Combined) <- c(groupBy, 'Combined')
  
  delays <- Reduce( function(x, y) merge(x, y, by = groupBy, all = TRUE),
                    list(delays_Carrier, delays_LateAircraftDelay, delays_NAS, delays_Security,
                         delays_Weather, delays_TotalFlights, delays_Combined))
  delays$Percent <- delays$Combined * 100.00 / delays$Total
  
  uniques <- expand.grid(c(0:23), c(1:12))
  cols <- c('ArrHour', 'Month')
  colnames(uniques) <- cols
  delays <- merge(delays, uniques, by = cols, all = TRUE)
  
  delays <- merge(delays, monthsDf, by.x = 'Month', by.y = 'MonthNumber', all = TRUE)
  delays$MonthName <- ordered(delays$MonthName, months)
  
  delays <- merge(delays, hoursDf(is24Hour), by.x = 'ArrHour', by.y = 'HourNumber', all = TRUE)
  delays$HourName <- ordered(delays$HourName, hours(is24Hour))
  
  delays[is.na(delays)] <- 0
  
  return(delays)
}

flightDataNoOfDelaysTable <- function(airport1, airport2, month, is24Hour) {
  monthNo <- as.numeric(subset(monthsDf, MonthName == month)$MonthNumber)
  
  delays1<- flightDataNoOfDelaysDataFrame(airport1, is24Hour)
  delays1<-subset(delays1[delays1$Month == monthNo,], select = -c(Month))
  delays1<-arrange(delays1, ArrHour)
  delays1<-delays1[,c(11,2,3,4,5,6,8,9)]
  delays1$Percent<-as.numeric(format(delays1$Percent, digits=2, nsmall=2))
  
  delays2<- flightDataNoOfDelaysDataFrame(airport2, is24Hour)
  delays2<-subset(delays2[delays2$Month == monthNo,], select = -c(Month))
  delays2<-arrange(delays2, ArrHour)
  delays2<-delays2[,c(11,2,3,4,5,6,8,9)]
  delays2$Percent<-as.numeric(format(delays2$Percent, digits=2, nsmall=2))
  
  delays<-merge(delays1, delays2, by='HourName', sort = F)
  # colnames(delays)<- c('Hour', 'Carrier Delays', 'Late Arrival Delays', 'National Airspace Sys. Delays', 'Security Delays', 'Weather Delays', 'Total Delays', 'Total Flights', 'Percent')
  return(delays)
}

top15AirportsPlot <- function(airport, stacked) {
  counts<-top15AirportsDataFrame(airport)

  return(plot_ly(
    x = counts$DestinationAirport, 
    y = counts$Arrivals, 
    frame = counts$MonthName, 
    text = paste(counts$AirportName, '\nArrivals:', counts$Arrivals), 
    hoverinfo = 'text', 
    name = paste('Arrivals to', airport), 
    type = 'bar',
    marker = list(color = arrivalColor)
  ) %>%
    add_trace(
      y = counts$Departures, 
      text = paste(counts$AirportName, '\nDepartures:', counts$Departures), 
      hoverinfo = 'text', 
      name = paste('Departures from', airport),
      marker = list(color = departureColor)
    ) %>%
    layout(barmode = if (stacked) 'stack', hovermode = 'compare',font = list(size = plotLabelSize), margin = list(t = plotMarginTop, b = plotMarginBottom))
  )
}

top15AirportsDataFrame <- function(airport) {
  top15AirportData <- getTopDestinations(airport, 15, getFlightData = TRUE)
  groupBy <- c('Month', 'DestinationAirport')
  counts <- count(top15AirportData, groupBy) # Total Flights
  
  uniques <- expand.grid(unique(top15AirportData$DestinationAirport), c(1:12))
  cols <- c('DestinationAirport', 'Month')
  colnames(uniques) <- cols
  counts <- merge(counts, uniques, by = cols, all = TRUE)
  
  counts <- merge(counts, count(top15AirportData[top15AirportData$Dest == airport, ], groupBy), by = groupBy, all = TRUE) # Arrivals
  counts <- merge(counts, count(top15AirportData[top15AirportData$Origin == airport, ], groupBy), by = groupBy, all = TRUE) # Departures
  colnames(counts) <- c(groupBy, 'Total_Flights', 'Arrivals', 'Departures')
  counts <- merge(counts, airports, by.x = 'DestinationAirport', by.y = 'IATA', all.x = TRUE)
  counts <- merge(counts, monthsDf, by.x = 'Month', by.y = 'MonthNumber', all = TRUE)
  counts$MonthName <- ordered(counts$MonthName, months)
  
  counts[is.na(counts)] <- 0
  return(counts)
}

top15AirportsTable <- function(airport1, airport2, month) {
  counts1<-top15AirportsDataFrame(airport1)
  counts2<-top15AirportsDataFrame(airport2)
  monthNo <- as.numeric(subset(monthsDf, MonthName == month)$MonthNumber)
  counts1<-subset(counts1[counts1$Month == monthNo,], select = -c(Month))
  counts2<-subset(counts2[counts2$Month == monthNo,], select = -c(Month))
  
  counts1<-counts1[order(-counts1$Total_Flights),]
  counts2<-counts2[order(-counts2$Total_Flights),]
  counts1<-counts1[,c(6,3,4)]
  counts2<-counts2[,c(6,3,4)]
  counts1$rn<-as.numeric(rownames(counts1))
  counts2$rn<-as.numeric(rownames(counts2))
  counts<-merge(counts1, counts2, by='rn', all = TRUE)
  # Rank<-1:length(counts)
  # counts<-cbind(Rank, counts)
  return(counts)
  
}

deepDiveMap <- function(dateValue, showDepartures, inMetric) {
  weatherData <- subset(weather, Month == as.numeric(format(dateValue, "%m")) & DayofMonth == as.numeric(format(dateValue, "%d")))
  if(showDepartures)
    weatherData <- weatherData[weatherData$OriginState == originState, ]
  else
    weatherData <- weatherData[weatherData$DestState == originState, ]

  origins <- unique(weatherData[,c('Olat', 'Olong', "OPRCP", "OSNOW", "OSNWD", "OTMAX", "OTMIN", "OTAVG", "Origin" )])
  dests <- unique(weatherData[,c('Dlat', 'Dlong', "DPRCP", "DSNOW", "DSNWD", "DTMAX", "DTMIN", "DTAVG", "Dest"  )])
  
  x <- leaflet() %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(lng = -97.696525, lat = 37.961656, zoom = zoomLevel)
  
  for(i in 1:nrow(weatherData)){
    lbl <- paste(
      weatherData[i,c('Origin')], '>', weatherData[i,c('Dest')], '<br/># Flights:', as.character(weatherData[i,c('NoOfFlights')]),
      '<br/># Cancellations:', as.character(weatherData[i,c('NoOfCancellations')])) %>% lapply(htmltools::HTML)
    
    x <- addPolylines(x, 
      lat = as.numeric(weatherData[i, c('Olat','Dlat' )]), lng = as.numeric(weatherData[i, c('Olong', 'Dlong')]), 
      label = lbl, opacity = weatherData[i,c('NoOfFlights')] / lineOpacity, weight = lineWeight, color = '#3A60C6',
      labelOptions = labelOptions(textsize = "1vh", style = list("line-height" = "1.5vh")))
  }
  
  originWeather <- mapply(getWeatherDetails, origins$Origin, origins$OPRCP, origins$OSNOW, origins$OSNWD, origins$OTMIN, origins$OTMAX, origins$OTAVG, inMetric)
  origins$WeatherLabel <- mapply(getWeatherLabelMarker, origins[, 'OTAVG'], origins[, 'OTMAX'], origins[, 'OTMIN'], inMetric) %>% lapply(htmltools::HTML)

  destWeather <- mapply(getWeatherDetails, dests$Dest, dests$DPRCP, dests$DSNOW, dests$DSNWD, dests$DTMIN, dests$DTMAX, dests$DTAVG, inMetric)
  dests$WeatherLabel <- mapply(getWeatherLabelMarker, dests[, 'DTAVG'], dests[, 'DTMAX'], dests[, 'DTMIN'], inMetric) %>% lapply(htmltools::HTML)
  
  print(dests$WeatherLabel)

  x <- addCircleMarkers(x, lng = origins$Olong, lat = origins$Olat, radius = 5, color = "red", fillColor = "red", popup = originWeather)
  x <- addCircleMarkers(x, lng = dests$Dlong, lat = dests$Dlat, radius = 5, color = "red", fillColor = "red", popup = destWeather)
  x <- addLabelOnlyMarkers(x, lng = origins$Olong, lat = origins$Olat, label = origins$WeatherLabel, labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T, textsize = "1vh"))
  x <- addLabelOnlyMarkers(x, lng = dests$Dlong, lat = dests$Dlat, label = dests$WeatherLabel, labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T, textsize = "1vh"))
  return(x)
}

getWeatherLabelMarker <- function(avgValue, maxValue, minValue, inMetric) {
  val <- if(!is.na(avgValue)) avgValue
  else if(!is.na(maxValue)) maxValue
  else if(!is.na(minValue)) minValue
  else NA
  
  return(if(is.na(val)) '' else temperature(val, inMetric))
}

getWeatherDetails <- function(iata, prcp, snow, snwd, tmin, tmax, tavg, inMetric) {
  result <- paste0('<b>', names(getAirportCodePlusNames(as.character(iata))), '</b><br/>')
  
  hasTemp <- FALSE
  if(!is.na(tmin) & tmin > 0) {
    hasTemp <- TRUE
    result <- paste(result, temperature(tmin, inMetric)) # Temperature is in tenths of C
  }
  if(!is.na(tmin) & tmin > 0 & !is.na(tmax) & tmax > 0)
    result <- paste(result, '/')
  if(!is.na(tmax) & tmax > 0) {
    hsTemp <- TRUE
    result <- paste(result, '<b>', temperature(tmax, inMetric), '</b>') # Temperature is in tenths of C
  }
  
  if(!hasTemp & !is.na(tavg) & tavg > 0)
    result <- paste(result, temperature(tmin, inMetric))
  
  if(!is.na(prcp) & prcp > 0)
    result <- paste(result, '<br/>Rainfall:', distance(prcp / 10.0, inMetric)) # Precipitation is in tenths of mm
  
  if(!is.na(snow) & snow > 0)
    result <- paste(result, '<br/>Snowfall:', distance(snow, inMetric)) # Snowfall is in mm
  
  if(!is.na(snwd) & snwd > 0)
    result <- paste(result, '<br/>Snow depth:', distance(snwd, inMetric)) # Snow depth is in mm
  
  return(result)
}

getDeepDiveCounts <- function(airport, choice, filterValue, monthly, is24Hour) {
  isDayChoice <- choice == 'day'
  isDateChoice <- choice == 'date'
  isAirportChoice <- choice == 'airport'
  
  deepDiveData <- flights[flights$Origin == airport | flights$Dest == airport, ]

    if(isAirportChoice) {
    deepDiveData <- deepDiveData[deepDiveData$Origin == filterValue | deepDiveData$Dest == filterValue, ]
  } else
    if(choice == 'airline') {
      deepDiveData <- merge(deepDiveData, airlines, by = 'AirlineID', all = TRUE)
      deepDiveData <- deepDiveData[deepDiveData$AirlineCode == filterValue, ]
    } else
      if(isDateChoice) {
        month <- as.numeric(format(filterValue, "%m"))
        mday <- as.numeric(format(filterValue, "%d"))
        deepDiveData <- deepDiveData[deepDiveData$Month == month & deepDiveData$DayofMonth == mday, ]
      } else
        if(isDayChoice) {
          deepDiveData <- deepDiveData[deepDiveData$DayOfWeek == filterValue, ]
        }
  
  yaxis <- if(monthly) 'Month' else 'DayOfWeek'
  groupBy <- c('Hour', yaxis)
  
  deepDiveDepartures <- deepDiveData[deepDiveData$Origin == airport, ]
  deepDiveDepartures$Hour <- deepDiveDepartures$DepHour
  deepDiveArrivals <- deepDiveData[deepDiveData$Dest == airport, ]
  deepDiveArrivals$Hour <- deepDiveArrivals$ArrHour
  
  counts <- merge(count(deepDiveDepartures, groupBy), count(deepDiveArrivals, groupBy), by = groupBy, all = TRUE)
  colnames(counts) <- c(groupBy, 'Departures', 'Arrivals')
  
  if(isDateChoice) {
    deepDiveDelays <- deepDiveData[(deepDiveData$NASDelay != 0 & !is.na(deepDiveData$NASDelay))
                                   | (deepDiveData$SecurityDelay != 0 & !is.na(deepDiveData$SecurityDelay))
                                   | (deepDiveData$WeatherDelay != 0 & !is.na(deepDiveData$WeatherDelay))
                                   | (deepDiveData$CarrierDelay != 0 & !is.na(deepDiveData$CarrierDelay))
                                   | (deepDiveData$LateAircraftDelay != 0 & !is.na(deepDiveData$LateAircraftDelay)), ]
    deepDiveDelays$Hour <- deepDiveDelays$ArrHour
    
    deepDiveCancellations <- cancellations[cancellations$Origin == airport
                                           & cancellations$Month == as.numeric(format(filterValue, "%m"))
                                           & cancellations$DayofMonth == as.numeric(format(filterValue, "%d")), ]
    deepDiveCancellations$Hour <- deepDiveCancellations$CRSDepHour # Plot by scheduled departure hour for cancellations
    
    counts <- merge(counts, count(deepDiveDelays, groupBy), by = groupBy, all = TRUE)
    counts <- merge(counts, count(deepDiveCancellations, groupBy), by = groupBy, all = TRUE)
    colnames(counts) <- c(groupBy, 'Departures', 'Arrivals', 'Delays', 'Cancellations')
  } else {
    if(monthly) {
      uniques <- expand.grid(c(0:23), c(1:12))
      cols <- groupBy
      colnames(uniques) <- cols
      counts <- merge(counts, uniques, by = cols, all = TRUE)
      
      counts <- merge(counts, monthsDf, by.x = 'Month', by.y = 'MonthNumber', all = TRUE)
      counts$MonthName <- ordered(counts$MonthName, months)
    } else { # by day of the week
      uniques <- expand.grid(c(0:23), c(1:7))
      cols <- groupBy
      colnames(uniques) <- cols
      counts <- merge(counts, uniques, by = cols, all = TRUE)
      
      counts <- merge(counts, daysOfWeekDf, by.x = 'DayOfWeek', by.y = 'DayNumber', all = TRUE)
      counts$DayName <- ordered(counts$DayName, daysOfWeek)
    }
  }
  counts <- merge(counts, hoursDf(is24Hour), by.x = 'Hour', by.y = 'HourNumber', all = TRUE)
  counts$HourName <- ordered(counts$HourName, hours(is24Hour))
  
  counts[is.na(counts)] <- 0
  return(counts)
}

deepDivePlot <- function(airport, choice, filterValue, plotType, is24Hour) {
  isDayChoice <- choice == 'day'
  isDateChoice <- choice == 'date'
  isAirportChoice <- choice == 'airport'
  
  counts <- getDeepDiveCounts(airport, choice, filterValue, monthly = TRUE, is24Hour)
  plotTitle = paste(airport, '-', if(isDayChoice) daysOfWeekDf[daysOfWeekDf$DayNumber == filterValue, ]$DayName else filterValue)
  
  if(plotType == 'map') {
    counts$n <- counts$Arrivals + counts$Departures
    
    if(isDayChoice) {
      return(getHourMonthHeatMap(counts[, c('Hour', 'Month', 'n')], 'Hour', max(counts$n), is24Hour))
    } else {
      weeklyCounts <- getDeepDiveCounts(airport, choice, filterValue, monthly = FALSE, is24Hour)
      weeklyCounts$n <- weeklyCounts$Arrivals + weeklyCounts$Departures
      
      maxZ <- max(max(counts$n), max(weeklyCounts$n))
      
      weeklyPlot <- getHourDayHeatMap(weeklyCounts[, c('Hour', 'DayOfWeek', 'n')], 'Hour', maxZ, is24Hour)
      monthlyPlot <- getHourMonthHeatMap(counts[, c('Hour', 'Month', 'n')], 'Hour', maxZ, is24Hour)
      
      return(subplot(monthlyPlot, weeklyPlot, nrows = 2, shareX = TRUE))
    }
  } else {
    p <- plot_ly(
        x = counts$HourName,
        y = counts$Departures,
        frame = if(!isDateChoice) counts$MonthName,
        text = paste(counts$Departures, 'Departures from', airport, ifelse(isAirportChoice, paste('to', filterValue), '')),
        hoverinfo = 'text',
        name = paste('Departures from', airport, ifelse(isAirportChoice, paste('to', filterValue), '')),
        type = 'bar',
        marker = list(color = departureColor)
      ) %>%
        add_trace(
          y = counts$Arrivals,
          text = paste(counts$Arrivals, 'Arrivals to', airport, ifelse(isAirportChoice, paste('from', filterValue), '')),
          hoverinfo = 'text',
          name = paste('Arrivals to', airport, ifelse(isAirportChoice, paste('from', filterValue), '')),
          marker = list(color = arrivalColor)
        ) %>%
        layout(hovermode = 'compare', title = plotTitle, font = list(size = plotLabelSize), margin = list(t = plotMarginTop, b = plotMarginBottom), legend = list(x = 0.1, y = 0.9))
  
    if(isDateChoice) {
      p <- p %>%
        add_trace(
          y = counts$Delays,
          text = paste(counts$Delays, 'Delays'),
          hoverinfo = 'text',
          name = 'Delays',
          marker = list(color = '#e7be48')
        ) %>%
        add_trace(
          y = counts$Cancellations,
          text = paste(counts$Cancellations, 'Cancellations'),
          hoverinfo = 'text',
          name = 'Cancellations',
          marker = list(color = '#795126')
        )
    }
  
    return(p)
  }
}

getMap <- function() {
  # Load the map

  us <- readOGR(dsn = 'data/us_states_hexgrid.geojson', layer = 'OGRGeoJSON')

  centers <- cbind.data.frame(data.frame(gCentroid(us, byid = TRUE), id = us@data$iso3166_2))
  us@data <- join(us@data, flightsIL, by = 'iso3166_2', type = 'left')
  us@data[is.na(us@data)]<- 0
  
  us@data$labels <- sprintf(
    'State: %s<br/>Flights from IL: %s, %.2f%%<br/>Flights to IL: %s, %.2f%%<br/>',
    us@data$google_name, us@data$Arrivals, us@data$percentArr, us@data$Departures, us@data$percentDep
  ) %>% lapply(htmltools::HTML)
  
  pal <- colorNumeric(palette = colorRamp(colorScale), domain = us@data$total)
  
  return(leaflet(us, options = leafletOptions(minZoom = 4, maxZoom = 7)) %>% setView(-99.85447, 40.70358, zoom = zoomLevelStates) %>%
           addPolygons(color = 'gray', weight = 1, smoothFactor = 0.5,
                       opacity = 1.0, fillOpacity = 0.8,
                       fillColor = ~pal(us@data$total),
                       highlightOptions = highlightOptions(color = 'black', weight = 2, bringToFront = TRUE),
                       label = us@data$labels,
                       labelOptions = labelOptions( style = list("font-weight" = "normal", "line-height" = "1.5vh"), textsize = "1.3vh", direction = "auto")
           ) %>%
           addLabelOnlyMarkers(lng = centers$x, lat = centers$y, label = centers$id, labelOptions = labelOptions(clickable = FALSE, noHide = T, textOnly = TRUE, offset=c(0,0), textsize="1.5vh")) %>%
           addLegend(pal = pal, values = ~total, title = '# Flights',opacity = 0.8)
         )
}

getHourDayHeatMap <- function(sourceData, hourColname, maxZ, is24Hour) {
  baseData <- expand.grid(0:23, 1:7)
  colnames(baseData) <- c('Hour', 'DayOfWeek')
  
  data <- merge(baseData, sourceData, # get values for all permutations of hour and dayofweek
                by.x = c('Hour', 'DayOfWeek'), 
                by.y = c(hourColname, 'DayOfWeek'),
                all = TRUE)
  data[which(is.na(data[,3]), arr.ind = TRUE), 3] <- 0 # update NAs to 0
  data <- arrange(data, Hour, DayOfWeek) # order by hour, dayofweek
  data <- matrix(data$n, nrow = 7, ncol = 24, dimnames = list(daysOfWeek, hours(is24Hour)) ) # convert to matrix
  return(plot_ly(name = 'By Weekday', x = hours(is24Hour), y = daysOfWeek, z = data, 
                 type = "heatmap", colors = colorRamp(colorScale), zmin = 0, zmax = maxZ) %>% layout(font = list(size = plotLabelSize), margin = list(l = plotMarginLeft, r = plotMarginRight, b = plotMarginBottom))) # plot
}

getHourMonthHeatMap <- function(sourceData, hourColname, maxZ, is24Hour) {
  baseData <- expand.grid(0:23, 1:12)
  colnames(baseData) <- c('Hour', 'Month')
  
  data <- merge(baseData, sourceData, # get values for all permutations of hour and dayofweek
                by.x = c('Hour', 'Month'), 
                by.y = c(hourColname, 'Month'),
                all = TRUE)
  data[which(is.na(data[,3]), arr.ind = TRUE), 3] <- 0 # update NAs to 0
  data <- arrange(data, Hour, Month) # order by hour, dayofweek
  data <- matrix(data$n, nrow = 12, ncol = 24, dimnames = list(months, hours(is24Hour)) ) # convert to matrix
  return(plot_ly(name = 'By Month', x = hours(is24Hour), y = months, z = data, 
                 type = "heatmap", colors = colorRamp(colorScale), zmin = 0, zmax = maxZ, showscale = FALSE) %>% layout(font = list(size = plotLabelSize), margin = list(l = plotMarginLeft, r = plotMarginRight, b = plotMarginBottom))) # plot
}

# server
server <- function(input, output, session) {
  observeEvent(input$dimension, {
    if(input$dimension[1] >= 2000){
      plotLabelSize <<- 25
      plotMarginTop <<- 100
      plotMarginLeft <<- 100
      plotMarginRight <<- 100
      plotMarginBottom <<- 100
      zoomLevel <<- 6
      lineOpacity <<- 4
      lineWeight <<- 3
      zoomLevelStates <<- 5.5
    }
    else{
      plotLabelSize <<- 12
      plotMarginTop <<- 40
      plotMarginLeft <<- 40
      plotMarginRight <<- 40
      plotMarginBottom <<- 40
      zoomLevel <<- 5
      lineOpacity <<- 8
      lineWeight <<- 2
      zoomLevelStates <<- 4.5
    }
  })

  observe({
    hide('airportBreakdown')
    hide('airlineBreakdown')
    hide('dateBreakdown')
    hide('dayBreakdown')
    hide('deepDiveGraph')
    hide('deepDiveMap')

    shinyjs::show(switch(
      input$breakdownRadioButton,
      'date' = 'dateBreakdown',
      'airport' = 'airportBreakdown',
      'airline' = 'airlineBreakdown',
      'day' = 'dayBreakdown'
    ))
    
    shinyjs::show(if(input$breakdownRadioButton == 'date' & input$breakdownPlotType == 'map') 'deepDiveMap' else 'deepDiveGraph')
  })

  observe({
    breakdownAirports <- getTopDestinations(input$breakdownSourceAirport, 50, getFlightData = FALSE)$DestinationAirport
    updateSelectInput(session, 'airportBreakdown', choices = getAirportCodePlusNames(breakdownAirports))
  })

  observe({
    breakdownAirlines <- getTopAirlines(input$breakdownSourceAirport, 50)
    updateSelectInput(session, 'airlineBreakdown', choices = getAirlineCodePlusNames(breakdownAirlines))
  })
  
  observe({
    hide('monthBreakdownFlights')
    hide('flightDataNumberOfFlightsTable')
    hide('monthBreakdownDelays')
    hide('flightDataNumberOfDelaysTable')
    hide('monthBreakdownTop15')
    hide('flightDataTop15DestinationsTable')
  })
  
  observeEvent(input$flightDataTable, {
    toggle('flightDataNumberOfFlights')
    toggle('flightDataNumberOfFlightsTable')
    toggle('monthBreakdownFlights')
    
    toggle('monthBreakdownDelays')
    toggle('flightDataNumberOfDelaysTable')
    toggle('flightDataNumberOfDelays')
    
    toggle('monthBreakdownTop15')
    toggle('flightDataTop15DestinationsTable')
    toggle('flightDataTop15Destinations')
  })

  output$flightDataNumberOfFlights <- renderPlotly({
    subplot(
      flightDataNoOfFlightsPlot(input$noOfFlightsBy, input$flightDataAirport1, input$flightDataStacked, input$timeFormat == '24 hr', input$measurements == 'Metric'),
      flightDataNoOfFlightsPlot(input$noOfFlightsBy, input$flightDataAirport2, input$flightDataStacked, input$timeFormat == '24 hr', input$measurements == 'Metric'),
      shareY = input$flightDataCompare
    ) %>%
      layout(title = paste(input$flightDataAirport1, '- vs. -', input$flightDataAirport2))
  })
  
  output$flightDataNumberOfFlightsTable <- DT::renderDataTable (
    DT::datatable({
      flightDataNoOfFlightsTable(input$flightDataAirport1, input$flightDataAirport2, input$noOfFlightsBy, input$monthBreakdownFlights, input$timeFormat == '24 hr', input$measurements == 'Metric')
    },
    container = tableHeaderTwoAirports(input$flightDataAirport1, input$flightDataAirport2, input$noOfFlightsBy, 'noOfFlights'),
    rownames = FALSE,
    options = list(paging = FALSE, searching = FALSE, dom = 't',order = list(list(0,'asc')))
    )
  )

  output$flightDataNumberOfDelays <- renderPlotly({
    subplot(
      flightDataNoOfDelaysPlot(input$flightDataAirport1, input$flightDataStacked, input$timeFormat == '24 hr'),
      flightDataNoOfDelaysPlot(input$flightDataAirport2, input$flightDataStacked, input$timeFormat == '24 hr'),
      shareY = input$flightDataCompare
    ) %>%
      layout(title = paste(input$flightDataAirport1, '- vs. -', input$flightDataAirport2))
  })
  
  output$flightDataNumberOfDelaysTable <- DT::renderDataTable (
    DT::datatable({
      flightDataNoOfDelaysTable(input$flightDataAirport1, input$flightDataAirport2, input$monthBreakdownDelays, input$timeFormat == '24 hr')
    },
    container = tableHeaderTwoAirports(input$flightDataAirport1, input$flightDataAirport2, 'hour', 'delays'),
    rownames = FALSE,
    options = list(paging = FALSE, searching = FALSE, dom = 't',order = list(list(0,'asc')))
    )
  )

  output$flightDataTop15Destinations <- renderPlotly({
    subplot(
      top15AirportsPlot(input$flightDataAirport1, input$flightDataStacked),
      top15AirportsPlot(input$flightDataAirport2, input$flightDataStacked),
      shareY = input$flightDataCompare
    ) %>%
      layout(title = paste(input$flightDataAirport1, '- vs. -', input$flightDataAirport2))
  })
  
  output$flightDataTop15DestinationsTable <- DT::renderDataTable (
    DT::datatable({
      top15AirportsTable(input$flightDataAirport1, input$flightDataAirport2, input$monthBreakdownTop15)
    },
    container = tableHeaderTwoAirports(input$flightDataAirport1, input$flightDataAirport2, 'top15', 'top15'),
    rownames = FALSE,
    options = list(paging = FALSE, searching = FALSE, dom = 't',order = list(list(0,'asc')))
    )
  )

  output$deepDivePlots <- renderPlotly({
    filterValue <- switch(
      input$breakdownRadioButton,
      'map' = input$dateBreakdown,
      'date' = input$dateBreakdown,
      'airport' = input$airportBreakdown,
      'airline' = input$airlineBreakdown,
      'day' = input$dayBreakdown
    )
    deepDivePlot(input$breakdownSourceAirport, input$breakdownRadioButton, filterValue, input$breakdownPlotType, input$timeFormat == '24 hr')
  })
  
  output$deepDiveLeaflet <- renderLeaflet({ deepDiveMap(input$dateBreakdown, 
                                                        showDepartures = input$deepDiveMapDirection == 'departures', 
                                                        inMetric = input$measurements == 'Metric') })
  
  output$mapFlightsToFromIL <- renderLeaflet({ getMap() })
}

shinyApp(ui = ui, server = server)
