library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyr)
library(leaflet)
library(jpeg)
library(grid)

###### layout ########
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
                  box(title = "Number of Arrivals and Departures by Airlines for the month of January", width = 12,
                      plotOutput("bar_airlines"),
                      dataTableOutput("table_airlines")
                  )
                ),
                conditionalPanel(
                  condition = "input.filter == 'Hours of Day'",
                  box(title = "Number of Arrivals and Departures for each Hour of the Day for the month of January", width = 12,
                      plotOutput("bar_hour"),
                      dataTableOutput("table_hour")
                  )
                ),
                conditionalPanel(
                  condition = "input.filter == 'Days of Week'",
                  box(title = "Number of Arrivals and Departures for each Day of the Week for the month of January", width = 12,
                      plotOutput("bar_day"),
                      dataTableOutput("table_day")
                  )
                ),
                conditionalPanel(
                  condition = "input.filter == 'Delays'",
                  box(title = "Number of Delays for each hour of the day for the month of January", width = 12,
                      plotOutput("bar_delay"),
                      dataTableOutput("table_delay")
                  )
                )
                
                
              )#fluidRow
      ),#tabItem
      tabItem("topCharts",
              fluidRow(
                box(
                  
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
  # number of flights(arrivals and departures) by airlines
  output$bar_airlines <- renderPlot({
    
  })
  
  
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

















