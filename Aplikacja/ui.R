library(DT)
library(shinyjs)
library(sodium)
library(data.table)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
require("RPostgres")
require(data.table)
library("dashboardthemes")
library("shinyBS")

tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);")

# title <- tags$li(class = "dropdown",
#                  tags$a(href='https://www.google.com',
#                 icon("diamond"),
#                 'Diamonds Explorer', target="_blank"))

header <- dashboardHeader( title = "Filmex",  uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 

body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"),
                      shinyDashboardThemes(
                        theme = "flat_red"
                      ))
dashboardPage(header, sidebar, body)