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



function(input, output, session) {
  
  credentials <- reactive({
    
    us <- as.data.table(dbGetQuery(con, "SELECT id_konta, haslo, email FROM konta;"))
    users <- us$email
    passwords <- us$haslo
    
    data.table(
      username_id = users,
      passod   = passwords,
      stringsAsFactors = F)
  })
  
  
  us <- reactive({
    
    as.data.table(dbGetQuery(con, "SELECT id_konta, haslo, email FROM konta;"))
    
  })
  
  uzytkownicy_konta <- reactive({
    
    id_konta <- us()[us()$email == input$userName]$id_konta
    dbGetQuery(con, paste0("SELECT * FROM uzytkownicy WHERE id_konta = '", id_konta, "';"))
    
  })  
  
  v <- reactiveValues()
  
  #Panel Logowanie==============================================================================================================================  
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials()$username_id==Username))==1) { 
            #pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            #pasverify <- password_verify(pasmatch, Password)
            pasverify <- credentials()[username_id==Username, passod]==Password
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
  
  #================================================================================================================================================  
  
  
  #Panel rejestracja==================================================================================================================================  
  observeEvent(input$reg_butt, {
    
    if(input$reg_passwd2 != input$reg_passwd){
      showNotification(paste0("Podane hasła nie pasują do siebie!"), type = 'err')
      
    }
    else{
      tryCatch({
        res <- dbSendQuery(con, paste0("SELECT utworz_konto('",
                                       input$reg_email,"', '", input$reg_passwd, "','", input$reg_plan, "');"))
      },
      error = function(err){
        showNotification(paste0("Podany e-mail nie istnieje!"), type = 'err')
      })
      
      
      showNotification("GITUWA", type = "message") 
    }
    
    
  })
  
  
  #============================================================================================================================================  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  output$tbl <- renderDataTable( plans_modal, options = list(lengthChange = FALSE, searching = FALSE, paging = FALSE))
  
  
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  
  
  #Panel boczny po zalogowaniu===================================================================================================================  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      
      sidebarMenu(id="tab",
                  menuItem("Konto", tabName="konto", icon = icon("dashboard")),
                  menuItem("Oglądaj dalej", tabName = "ogladanie", icon =  icon("bar-chart-o")),
                  menuItem("Twoje komentarze", tabName = "komentarze", icon =  icon("list-alt")),
                  menuItem("Top listy", tabName = "topy", icon = icon("line-chart")),
                  menuItem("Top listy", tabName = "dashboard", icon = icon("thumbs-up"))
      )
    }
  })
  

  #===============================================================================================================================================
  
  ?radioGroupButtons
  shinyWidgetsGallery()
  
  
  #Główna treść apki=============================================================================================================================== 
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItems(
        tabItem(tabName ="konto", class = "active",
                box(width = 12, 
                  tags$h2("Wybierz użytkownika", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                  radioGroupButtons(
                    inputId = "uzytkownik",
                    choiceNames =  uzytkownicy_konta()$nazwa,
                    choiceValues = uzytkownicy_konta()$id_uzytkownika,
                    size='lg',
                    direction = "horizontal",
                    justified = TRUE,
                    width = '100%',
                    individual = TRUE
                  )
                  
                )),
        tabItem(tabName ="ogladanie",
                
                fluidRow(
                  box(width = 5, 
                      tags$h2("Oglądaj dalej film!", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                      dataTableOutput('odtworzenia_filmow')),
                  box(width = 7, 
                      tags$h2("Oglądaj dalej serial!", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                      dataTableOutput('odtworzenia_seriali'))
                )
                ),
        
        tabItem(tabName ="komentarze", 
                h2("sdlkj")),
        
        tabItem(tabName ="topy",
                fluidRow(
                  box(width = 6,
                      radioGroupButtons(
                        inputId = "kat_f",
                        label = "Kategorie",
                        choiceNames = c("Wszystkie",
                                    "Animowany", "Biograficzny", "Dokumentalny", 
                                    "Dramat", "Historyczny",
                                    "Horror"  , "Komedia", "Musical", "Romantyczny",
                                    "Sci-fi", "Thriller", "Western"),
                        choiceValues = c(0, 12, 2, 22, 18, 9, 19, 11, 10, 4, 20, 3, 14)
                      ),
                      dataTableOutput('top_filmy')),
                  box(width = 6, 
                      radioGroupButtons(
                        inputId = "kat_s",
                        label = "Kategorie",
                        choiceNames = c("Wszystkie",
                                    "Animowany", "Biograficzny", "Dokumentalny", 
                                    "Dramat", "Historyczny",
                                    "Horror"  , "Komedia", "Musical", "Romantyczny",
                                    "Sci-fi", "Thriller", "Western"),
                        choiceValues = c(0, 12, 2, 22, 18, 9, 19, 11, 10, 4, 20, 3, 14)
                      ),
                      dataTableOutput('top_seriale'))
                  
                )
        )
      )
      #=========================================================================================================================================    
    }
    else {
      loginpage
    }
  })
  
  output$odtworzenia <- DT::renderDataTable({
      
    odtworzenia <- dbGetQuery(con, paste0("SELECT * FROM odtworzenia_u(", input$uzytkownik, ");"))
    
    datatable(odtworzenia, options = list(width = 5,
                                        searching = FALSE))
    
  })
  
  output$odtworzenia_filmow <- DT::renderDataTable({
    
    odtworzenia <- dbGetQuery(con, paste0("SELECT * FROM odtworzenia_f_u(", input$uzytkownik, ");"))
    
    odtworzenia[["Oglądaj dalej"]] <-
      paste0(HTML('
          <div class="btn-group" role="group" aria-label="Basic example">
          <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(odtworzenia),'> Kontynuuj</button></div>'))
    
    datatable(odtworzenia, options = list(width = 5,
                                          searching = FALSE), escape = FALSE)
    
  })
  
  output$odtworzenia_seriali <- DT::renderDataTable({
    
    odtworzenia <- dbGetQuery(con, paste0("SELECT * FROM odtworzenia_s_u(", input$uzytkownik, ");"))
    
    odtworzenia[["Oglądaj dalej"]] <-
      paste0(HTML('
          <div class="btn-group" role="group" aria-label="Basic example">
          <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(odtworzenia),'> Kontynuuj</button></div>'))
    
    datatable(odtworzenia, options = list(width = 5,
                                          searching = FALSE), escape = FALSE)
    
  })
  
  
  
  output$top_filmy  <-  DT::renderDataTable({
    
    if(input$kat_f == 0){
      top_f <- as.data.table(dbGetQuery(con, "SELECT * FROM top_filmow;"))
    }
    else{
      top_f <- as.data.table(dbGetQuery(con, paste0("SELECT * FROM top_f(", input$kat_f, ");")))
    }
    
    datatable(top_f, options = list(width = 5,
                                    searching = FALSE))
  })
  
  output$top_seriale  <-  DT::renderDataTable({
    
    if(input$kat_s == 0){
      top_s <- as.data.table(dbGetQuery(con, "SELECT * FROM top_seriali;"))
    }
    else{
      top_s <- as.data.table(dbGetQuery(con, paste0("SELECT * FROM top_s(", input$kat_s, ");")))
    }
    
    datatable(top_s, options = list(width = 5,
                                    searching = FALSE))
  })
  
  
  
} 