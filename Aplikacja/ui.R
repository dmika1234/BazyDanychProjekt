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
#

tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);")

header <- dashboardHeader(title = span(img(src="logo1.png", width = '130px')),
                          uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 

body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"),
                      shinyDashboardThemes(
                        theme = "flat_red"
                      ))
dashboardPage(header, sidebar, body)


#runApp("D:/Studia/BazyDanych/Labki/Projekt/BazyDanychProjekt/Aplikacja", display.mode = "showcase")







