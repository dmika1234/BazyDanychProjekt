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

#=======================================================


con <- dbConnect(RPostgres::Postgres(), dbname = "projekt",
                 host = "localhost", port = 5432, 
                 user = "postgres", pass = "IryaStist")



us <- dbGetQuery(con, "SELECT id_konta, haslo, email FROM konta;")

users <- us$email

passwords <- us$haslo

plans_modal <- as.data.table(dbGetQuery(con, "SELECT * FROM plany;"))

plans_modal[, cena := paste(cena, "zl", sep = " ")]
setnames(plans_modal,
         old = c("nazwa_planu", "max_osob", "cena"),
         new = c("Nazwa planu", "Maksymalna liczba uzytkownikow", "Cena"))

plans_modal <- plans_modal[, -1]
plans <- dbGetQuery(con, "SELECT nazwa_planu FROM plany;")



top_f <- dbGetQuery(con, "SELECT * FROM top_filmow;") 
top_s <- dbGetQuery(con, "SELECT * FROM top_seriali;")

#===========================================





# Main login screen
loginpage <- tags$div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;", 
                      fluidRow(
                 #Logowanie==
                 wellPanel(
                   tags$h2("Logowanie", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                   textInput("userName", placeholder="E-mail", label = tagList(icon("user"), "E-mail")),
                   passwordInput("passwd", placeholder="Haslo", label = tagList(icon("unlock-alt"), "Haslo")),
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
                 
                 
                 
                 #Rejestracja==
                 wellPanel(
                   tags$h2(HTML('Nie masz konta? <br/> Zarejestruj sie'), class = "text-center", style = "padding-top: 0; font-weight:600;"),
                   textInput("reg_email", placeholder="E-mail", label = tagList(icon("user"), "E-mail")),
                   passwordInput("reg_passwd", placeholder="Haslo", label = tagList(icon("unlock-alt"), "Haslo")),
                   passwordInput("reg_passwd2", placeholder="Haslo", label = tagList(icon("unlock-alt"), "Powtorz haslo")),
                   selectInput("reg_plan", "Wybierz plan z listy", choices = plans),
                   actionButton("about_plan", "Dowiedz sie wiecej o planach"),
                   bsModal("modalExample", "Data Table", "about_plan", size = "large",
                           dataTableOutput("tbl")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("reg_butt", "Zarejestruj sie", style = "color: white; font-weight: 600;")
            
                   )))
)



credentials = data.table(
  username_id = users,
  passod   = passwords, 
  stringsAsFactors = F
)

?textInput


header <- dashboardHeader( title = "Simple Dashboard", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 

body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"),
                      shinyDashboardThemes(
                        theme = "flat_red"
                      ))

ui<-dashboardPage(header, sidebar, body)




?shinyDashboardThemes




server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
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
              id_konta <- us[credentials$username_id==Username,]$'id_konta'
              uzytkownicy <- dbGetQuery(con, paste0("SELECT nazwa FROM uzytkownicy WHERE id_konta = ", id_konta, ";"))$nazwa
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
      showNotification(paste0("Podany email nie istnieje!"), type = 'err')
        })
      
    showNotification("GITUWA", type = "message") 
    }
  })
  

  output$tbl <- renderDataTable( plans_modal, options = list(lengthChange = FALSE))
  
  
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      
      sidebarMenu(id="tab",
                  menuItem("Konto", tabName="konto", icon = icon("dashboard")),
                  menuItem("Ogladaj dalej", tabName = "ogladanie", icon =  icon("bar-chart-o")),
                  menuItem("Twoje komentarze", tabName = "komentarze", icon =  icon("list-alt")),
                  menuItem("Top listy", tabName = "topy", icon = icon("line-chart")),
                  menuItem("Top listy", tabName = "dashboard", icon = icon("sign-out"))
      )
    }
  })
  
  HTML("input[type='radio'] { transform: scale(2); }")
 

  
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItems(
        tabItem(tabName ="konto", class = "active",
                fluidRow(
                  tags$h2("Wybierz uzytkownika", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                  radioGroupButtons(
                    inputId = "Id068",
                    choices = uzytkownicy,
                    size = "lg",
                    width = 100,
                    direction = "horizontal"
                  )
                  
                )),
        tabItem(tabName ="ogladanie",
               h2("asdklj")),
        
        tabItem(tabName ="komentarze", 
                h2("sdlkj")),
        
        tabItem(tabName ="topy",
                fluidRow(
                  box(width = 6,
                      radioGroupButtons(
                        inputId = "Id064",
                        label = "Kategorie",
                        choices = c("Wszystkie",
                                    "Animowany", "Biograficzny", "Dokumentalny", 
                                    "Dramat", "Historyczny",
                                    "Horror"  , "Komedia", "Musical", "Romantyczny",
                                    "Sci-fi", "Thriller", "Western")
                      ),
                      dataTableOutput('top_filmy')),
                      box(width = 6, 
                          radioGroupButtons(
                        inputId = "Id064",
                        label = "Kategorie",
                        choices = c("Wszystkie",
                                    "Animowany", "Biograficzny", "Dokumentalny", 
                                    "Dramat", "Historyczny",
                                    "Horror"  , "Komedia", "Musical", "Romantyczny",
                                    "Sci-fi", "Thriller", "Western")
                          ),
                        dataTableOutput('top_seriale'))
                        
                        )
                )
      )
      
      
      
    }
    else {
      loginpage
    }
  })
  


  
  output$top_filmy  <-  DT::renderDataTable({
    datatable(top_f, options = list(width = 5,
                                  searching = FALSE))
  })
  
  output$top_seriale  <-  DT::renderDataTable({
    datatable(top_s, options = list(width = 5,
                                    searching = FALSE))
  })
  
  
  
}  


  
shinyWidgetsGallery()
  
  



runApp(list(ui = ui, server = server))
