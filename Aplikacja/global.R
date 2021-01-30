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
source("C:/Users/dmika/OneDrive/Dokumenty/login_credentials.R")


#Zmienne Globalne==============================================================================================================================
con <- dbConnect(RPostgres::Postgres(), dbname = "projekt",
                 host = "localhost", port = 5432, 
                 user = username_db, pass = password_db)



us <- as.data.table(dbGetQuery(con, "SELECT id_konta, haslo, email FROM konta;"))



users <- us$email

passwords <- us$haslo

plans_modal <- as.data.table(dbGetQuery(con, "SELECT * FROM plany;"))

plans_modal[, cena := paste(cena, "zl", sep = " ")]
setnames(plans_modal,
         old = c("nazwa_planu", "max_osob", "cena"),
         new = c("Nazwa planu", "Maksymalna liczba uzytkownikow", "Cena"))

plans_modal <- plans_modal[, -1]
plans <- plans_modal[, "Nazwa planu"]


top_f <- as.data.table(dbGetQuery(con, "SELECT * FROM top_filmow;"))
top_s <- as.data.table(dbGetQuery(con, "SELECT * FROM top_seriali;"))

#============================================================
credentials <- reactive({
  data.table(
  username_id = users,
  passod   = passwords, 
  stringsAsFactors = F)
  
  
})


#=============================================================





#Panel logowania==============================================================================================================================
loginpage <- tags$div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;", 
                      fluidRow(
                        
                        #LogowanieUI==
                        wellPanel(
                          tags$h2("Logowanie", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                          textInput("userName", placeholder="E-mail", label = tagList(icon("user"), "E-mail")),
                          passwordInput("passwd", placeholder="Hasło", label = tagList(icon("unlock-alt"), "Hasło")),
                          br(),
                          div(
                            style = "text-align: center;",
                            actionButton("login", "Zaloguj", style = "color: white; font-weight: 600;"),
                            shinyjs::hidden(
                              div(id = "nomatch",
                                  tags$p("Ups! Błędny login lub hasło!",
                                         style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                         class = "text-center"))),
                            br(),
                            br(),
                            tags$code(paste("Username: ", users[1],  "Password: ", passwords[1])),
                            br()
                          )),
                        #=============
                        
                        
                        #RejestracjaUI==================================
                        wellPanel(
                          tags$h2(HTML('Nie masz konta? <br/> Zarejestruj się'), class = "text-center",
                                  style = "padding-top: 0; font-weight:600;"),
                          textInput("reg_email", placeholder="E-mail", label = tagList(icon("user"), "E-mail")),
                          passwordInput("reg_passwd", placeholder="Hasło", label = tagList(icon("unlock-alt"), "Hasło")),
                          passwordInput("reg_passwd2", placeholder="Hasło", label = tagList(icon("unlock-alt"), "Powtórz hasło")),
                          selectInput("reg_plan", "Wybierz plan z listy", choices = plans),
                          actionButton("about_plan", "Dowiedz sie wiecej o planach",
                                       style='color: black; background-color: white; border-color: white; opacity: .4; padding:4px; font-size:80%'),
                          bsModal("modalExample", "Plany", "about_plan", size = "large",
                                  dataTableOutput("tbl")),
                          br(),
                          div(
                            style = "text-align: center;",
                            actionButton("reg_butt", "Zarejestruj się", style = "color: white; font-weight: 600;")
                            
                            )
                          )
                        #=============
                        
                        )
)

#=============================================================================================================================================
