library(DT)
library(shinyjs)
library(sodium)
library(data.table)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
require("RPostgres")
require(data.table)

#=======================================================


con <- dbConnect(RPostgres::Postgres(), dbname = "projekt",
                 host = "localhost", port = 5432, 
                 user = "postgres", pass = "dell123987")



us <- dbGetQuery(con, "SELECT haslo, email FROM konta;")

users <- us$email

passwords <- us$haslo

plans <- dbGetQuery(con, "SELECT nazwa_planu FROM plany;")


plans
#===========================================





# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 #Logowanie==
                 wellPanel(
                   tags$h2("Zaloguj sie", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="E-mail", label = tagList(icon("user"), "E-mail")),
                   passwordInput("passwd", placeholder="Haslo", label = tagList(icon("unlock-alt"), "Haslo")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "Zaloguj sie", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code(paste("Username: ", users[1],  "Password: ", passwords[1])),
                     br()
                   )),
                 
                 
                 
                 #Rejestracja==
                 wellPanel(
                   tags$h2("Nie masz konta? Zarejestruj sie", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("reg_email", placeholder="E-mail", label = tagList(icon("user"), "E-mail")),
                   passwordInput("reg_passwd", placeholder="Haslo", label = tagList(icon("unlock-alt"), "Haslo")),
                   passwordInput("reg_passwd2", placeholder="Haslo", label = tagList(icon("unlock-alt"), "Powtorz haslo")),
                   selectInput("reg_plan", "Wybierz plan z listy", choices = plans),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("reg_butt", "Zarejestruj sie", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;")
            
                   ))
)



credentials = data.table(
  username_id = users,
  passod   = passwords, 
  stringsAsFactors = F
)




header <- dashboardHeader( title = "Simple Dashboard", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            #pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            #pasverify <- password_verify(pasmatch, Password)
            pasverify <- credentials[username_id==Username, passod]==Password
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  
  
  observeEvent(input$reg_butt, {
    
    if(input$reg_passwd2 != input$reg_passwd){
      showNotification(paste0("Hasla nie pasuja!"), type = 'err')
      
    }
    else{
      tryCatch({
      res <- dbSendQuery(con, paste0("SELECT utworz_konto('",
                            input$reg_email,"', '", input$reg_passwd, "','", input$reg_plan, "');"))
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')
        })
      
    }
  })
  


  
  
  
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      sidebarMenu(
        menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard"))
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItem(tabName ="dashboard", class = "active",
              fluidRow(
                box(width = 12, dataTableOutput('results'))
              ))
    }
    else {
      loginpage
    }
  })
  
  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
  
}

runApp(list(ui = ui, server = server))
