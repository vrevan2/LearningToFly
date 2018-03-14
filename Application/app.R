library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(reshape2) ##??
library(DT)

#### Globals
defaultTz <- "America/Chicago"
is24Hour <- TRUE
isMetric <- FALSE

daysOfWeek <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
monthsDf <- data.frame(months,c(1:12))
colnames(monthsDf) <- c("Name", "Number")
hours <- if(is24Hour) c(0:23) else c(paste(c(0:11), "am"), "Noon", paste(c(1:11), "pm"))

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
          column(width = 1, checkboxInput("compareFlightData", "Compare")),
          column(width = 1, checkboxInput("stackedFlightData", "Stacked")),
          column(width = 1, offset = 1, actionButton("arrivalDepartureData", "Show Data"))),
        fluidRow(tabBox(
          id = "flightDataTabs",
          width = 12,
          tabPanel(
            "Number of Flights",
            radioButtons("noOfFlightsBy", "Plot by", inline = TRUE, c("Airline", "Hour", "Day of the Week", "Flight Time", "Distance")),
            plotOutput("flightDataNumberOfFlights")
          ),
          tabPanel(
            "Number of Delays",
            plotOutput("flightDataNumberOfFlights")
          )
        ))
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
              
      )#tabItem
)))

# server
server <- function(input, output){
  
  getCounts <- function (data, groupBy) {
    counts <- count(data, Month, groupBy)
    colnames(counts) <- c(groupBy, "")
    return(counts)
  }

  byAirline <- c("Month", "AirlineID")
  arrival1 <- count(subset(flights, Dest == 'ORD'), Month, AirlineID)
  colnames(arrival1) <- c(byAirline, "Frequency")
  departure1 <- count(subset(flights, Origin == 'ORD'), Month, AirlineID)
  colnames(departure1) <- c(byAirline, "Frequency")
  arrDep1 <- merge(arrival1, departure1, by = byAirline, all.x=TRUE, all.y =TRUE)
  arrDep1 <- merge(arrDep1, monthsDf, by.x = "Month",  by.y = "Number")
  arrDep1 <- merge(arrDep1, airlines, by = "AirlineID")
  
  arrDepPlot1 <- plot_ly(
    x = as.character(arrDep1$AirlineCode), 
    y = arrDep1$Frequency.x, 
    frame = paste(format(arrDep1$Month, width = 2), '-', arrDep1$Name),
    text = paste(arrDep1$AirlineName, '; Arrivals:', arrDep1$Frequency.x),
    hoverinfo = 'text',
    name = 'Arrivals',
    type = 'bar') %>%
    add_trace(y = arrDep1$Frequency.y,
              text = paste(arrDep1$AirlineName, '; Departures:', arrDep1$Frequency.y),
              hoverinfo = 'text',
              name = 'Departures')
  
  arrival2 <- getCounts(subset(flights, Dest == input$flightDataAirport2), byAirline)
  departure2 <- getCounts(subset(flights, Origin == input$flightDataAirport2), byAirline)
  arrDep2 <- merge(arrival2, departure1, by = byAirline, all.x=TRUE, all.y =TRUE)
  arrDep2 <- merge(arrDep2, airlines, by = "AirlineID")
  
  arrDepPlot2 <- plot_ly(
    x = arrDep2$AirlineID, 
    y = arrDep2$Frequency.x, 
    frame = arrDep2$Month,
    type = 'bar') %>%
    add_trace(y = arrDep2$Freq.y ) %>%
    layout( barmode = 'stack')
  
  output$flightDataNumberOfFlights = 
    renderPlot(
        subplot(arrDepPlot1, arrDepPlot2, shareY = input$compareFlightData)
    )
}

shinyApp(ui = ui, server = server)
