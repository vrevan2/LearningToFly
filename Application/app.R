library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyr)
library(leaflet)
library(jpeg)
library(grid)

# Capitalize the first letter of each word
capwords <- function(s) {
  cap <- function(s)
    paste(toupper(substring(s, 1, 1)),
          tolower(substring(s, 2)),
          sep = "",
          collapse = " ")
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# User Interface
ui <- dashboardPage(
  title = "Learning to Fly - by team 'R you Shiny'",
  skin = c("red"),
  dashboardHeader(title = "Learning to Fly", titleWidth = 350),
  dashboardSidebar(
    includeCSS("styles.css"),
    sidebarMenu(
      menuItem("About", tabName = "about"),
    )
  ),
  dashboardBody(tabItems(
    tabItem("about", includeHTML("about.html"))
  ))
)

# Server
server <- function(input, output) {
  output$attackDeathTable <-
    renderDataTable(attacksDeaths, options = list(columnDefs = list(list(
      targets = 1:3, className = "right"
    )), pageLength = 15))
}

shinyApp(server = server, ui = ui)
