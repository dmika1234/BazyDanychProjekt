library(DT)
library(shinyjs)
library(sodium)
library(data.table)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
require("RPostgres")
library(dplyr)
require(data.table)
library("dashboardthemes")
library("shinyBS")
library(lubridate)
library(stringr)
library(glue)


function(input, output, session){
  
  
  
  #Reactives
  #----
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
  
  
  id_konta <- reactive({
    
    id_konta <- us()[us()$email == input$userName]$id_konta
    
  })
  
  
  zaleglosci <- reactive({
    
    input$potwierdz
    
    kwota <- dbGetQuery(con, paste0("SELECT zaleglosci_pl(", id_konta(), ");"))$zaleglosci_pl
    
    if(kwota==0){
      zaleglosci <- '<span style=\"color:green\"> Konto nie ma zaległości w płaceniu </span>'
    }
    else{
      zaleglosci <- paste0('<span style=\"color:red\"> Konto ma zaległości na ', kwota, ' złotych! </span>')
    }
    
  })
  
  
  
  uzytkownicy_konta <- reactive({
    #input$uz_add_fin
    dbGetQuery(con, paste0("SELECT * FROM uzytkownicy WHERE id_konta = '", id_konta(), "';"))
    
    
  })
  
  
  
  max_uz <- reactive({
    
    
    as.numeric(dbGetQuery(con, paste0("SELECT p.max_osob FROM plany p
                           JOIN konta k ON k.id_planu = p.id_planu
                            WHERE k.id_konta = '", id_konta(), "';")))
    
  })
  #===============
  
  
  
  #Panel Logowanie==============================================================================================================================  
  #----
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
  #----
  observeEvent(input$reg_butt, {
    
    if(input$reg_passwd2 != input$reg_passwd){
      showNotification(paste0("Podane hasła nie pasują do siebie!"), type = 'err')
      
    }
    else{
      tryCatch({
        res <- dbSendQuery(con, paste0("SELECT utworz_konto('",
                                       input$reg_email,"', '", input$reg_passwd, "','", input$reg_plan, "');"))
        
        dbFetch(res)
        if(dbHasCompleted(res)){
          showNotification("Konto dodano pomyślnie!", type = "message") 
          
        }
        
        dbClearResult(res)
        
      },
      error = function(err){
        showNotification(paste0("Podany e-mail nie istnieje!"), type = 'err')
      })
      
      
      
    }
    
    
    
  })
  
  
  #============================================================================================================================================  
  
  
  
  
  
  
  
  
  #Panel boczny po zalogowaniu===================================================================================================================  
  #----
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      
      sidebarMenu(id="tab",
                  menuItem("Konto", tabName="konto", icon = icon("fas fa-users")),
                  menuItem("Oglądaj dalej", tabName = "ogladanie", icon =  icon("fas fa-play")),
                  menuItem("Twoje komentarze", tabName = "komentarze", icon =  icon("fas fa-comments") ),
                  menuItem("Top listy", tabName = "topy", icon = icon("thumbs-up")), 
                  menuItem("Statystyki", tabName = "stats", icon = icon("line-chart"))
      )
    }
  })
  
  
  #===============================================================================================================================================
  
  
  
  #Główna treść apki=============================================================================================================================== 
  #----
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      tabItems(
        tabItem(tabName ="konto", class = "active",
                #----
                column(10, align = 'center', 
                       fluidRow(
                         box(width = 12, 
                             tags$h2("Wybierz użytkownika", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                             radioGroupButtons(
                               inputId = "uzytkownik",
                               choiceNames =  uzytkownicy_konta()$nazwa,
                               choiceValues = uzytkownicy_konta()$id_uzytkownika,
                               size='lg',
                               direction = "horizontal",
                               justified = TRUE,
                               width = '60%',
                               individual = TRUE
                             ),
                             div(
                               style = "text-align: center;",
                               actionBttn("uz_add", "Dodaj użytkownika", style = "pill", color = "danger")),
                             bsModal("uz_add_modal", "Dodanie użytkownika", "uz_add", size = "large", 
                                     wellPanel(
                                       textInput("new_uz_name", placeholder = "Nazwa", label = "Podaj nazwę użytkownika"),
                                       materialSwitch(
                                         inputId = "if_baby_add",
                                         label = "Czy użytkownik to dziecko?", 
                                         status = "primary",
                                         right = TRUE
                                       ),
                                       actionButton("uz_add_fin", "Dodaj"),
                                       actionButton("refresh_uz", "Odśwież użytkowników")
                                     )
                             ))),
                       
                       box(width = 6,  tags$h2(HTML(zaleglosci()), class = "text-center"),
                           
                           div(
                             style = "text-align: center;", 
                             numericInput("zaplac", tags$h3("Wpisz kwotę, którą chcesz zapłacić"), value=0, width = '100%', min=0),
                             actionButton("potwierdz", "Potwierdź płatność", icon = icon("fas fa-dollar-sign")))
                           
                           
                       ),
                       box(width = 6, 
                           tags$h2("Ostatnie płatności", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                           dataTableOutput('platnosci')
                           
                       ),
                )
                
        ),
        #=======================
        
        tabItem(tabName ="ogladanie",
                #---- 
                fluidRow(
                  box(width = 5, 
                      dropdownButton(label = "asd", textInput("prod_findf", "Wyszukaj film", placeholder = "Tytuł"),
                                     actionButton("confirm_findf", "Szukaj"),
                                     icon = icon("search")),
                      uiOutput("found_films"),
                      #textOutput("test"),
                  ),
                  
                  box(width = 7, 
                      dropdownButton(textInput("prod_finds", "Wyszukaj serial", placeholder = "Tytuł"),
                                     actionButton("confirm_finds", "Szukaj"),
                                     icon = icon("search")),
                      uiOutput("found_series"),
                      #textOutput("test1"),
                  )),
                
                fluidRow(box(width = 5, 
                             tags$h2("Oglądaj dalej film!", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                             dataTableOutput('odtworzenia_filmow')
                             #textOutput("test")
                ),
                
                
                
                
                box(width = 7, 
                    tags$h2("Oglądaj dalej serial!", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                    dataTableOutput('odtworzenia_seriali')
                    
                    #textOutput("test1"),
                ),
                
                ),
                uiOutput("watch_ui"),
                
                uiOutput("watch_ui2"),
        ),
        #===============
        
        tabItem(tabName ="komentarze", 
                #----
        ),
        #================
        
        tabItem(tabName ="topy",
                #----
                tags$h1("Jakie produkcje użytkownicy oceniają najlepiej?", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                fluidRow( 
                  box(width = 6,
                      tags$h2("Najlepsze filmy!", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                      radioGroupButtons(
                        inputId = "kat_f",
                        label = "Posortuj po kategorii:",
                        choiceNames = c("Wszystkie",
                                        "Animowany", "Biograficzny", "Dokumentalny", 
                                        "Dramat", "Historyczny",
                                        "Horror"  , "Komedia", "Musical", "Romantyczny",
                                        "Sci-fi", "Thriller", "Western"),
                        choiceValues = c(0, 12, 2, 22, 18, 9, 19, 11, 10, 4, 20, 3, 14)
                      ),
                      dataTableOutput('top_filmy')),
                  uiOutput("watch_ui3"),
                  
                  
                  box(width = 6, 
                      tags$h2("Najlepsze seriale!", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                      radioGroupButtons(
                        inputId = "kat_s",
                        label = "Posortuj po kategorii:",
                        choiceNames = c("Wszystkie",
                                        "Animowany", "Biograficzny", "Dokumentalny", 
                                        "Dramat", "Historyczny",
                                        "Horror"  , "Komedia", "Musical", "Romantyczny",
                                        "Sci-fi", "Thriller", "Western"),
                        choiceValues = c(0, 12, 2, 22, 18, 9, 19, 11, 10, 4, 20, 3, 14)
                      ),
                      dataTableOutput('top_seriale')),
                  uiOutput("watch_ui4")
                  
                ),
                tags$h1("Jakie produkcje użytkownicy oglądają najczęściej?", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                fluidRow(
                  box(width = 6,
                      tags$h2("Najczęściej oglądane filmy!", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                      radioGroupButtons(
                        inputId = "kat_o_f",
                        label = "Posortuj po kategorii:",
                        choiceNames = c("Wszystkie",
                                        "Animowany", "Biograficzny", "Dokumentalny", 
                                        "Dramat", "Historyczny",
                                        "Horror"  , "Komedia", "Musical", "Romantyczny",
                                        "Sci-fi", "Thriller", "Western"),
                        choiceValues = c(0, 12, 2, 22, 18, 9, 19, 11, 10, 4, 20, 3, 14)
                      ),
                      dataTableOutput('top_o_filmy')
                  ),
                  uiOutput("watch_ui5"),
                  
                  
                  box(width = 6, 
                      tags$h2("Najczęściej oglądane seriale!", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                      radioGroupButtons(
                        inputId = "kat_o_s",
                        label = "Posortuj po kategorii:",
                        choiceNames = c("Wszystkie",
                                        "Animowany", "Biograficzny", "Dokumentalny", 
                                        "Dramat", "Historyczny",
                                        "Horror"  , "Komedia", "Musical", "Romantyczny",
                                        "Sci-fi", "Thriller", "Western"),
                        choiceValues = c(0, 12, 2, 22, 18, 9, 19, 11, 10, 4, 20, 3, 14)
                      ),
                      dataTableOutput('top_o_seriale')
                  ),
                  uiOutput("watch_ui6")
                  
                )
        ),
        #=========
        
        tabItem(tabName = "stats",
                fluidPage(
                  
                  tags$h2("Chcesz się dowiedzieć czegoś o naszej platformie?", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                  br(),
                  fluidRow(
                    box(width = 6,
                        tags$h4("Skąd pochodzą dostępne u nas produkcje?", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                        plotOutput("pie_kraj")
                        ),
                    box(width = 6,
                        tags$h4("Jakich kategorii produkcji mamy najwięcej?", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                        plotOutput("pie_kat")
                      
                        )
                  ),
                  fluidRow(column(width = 12, align = 'center',
                  box(width = 2, background = NULL),                                      
                  box(width = 8,
                      tags$h2("Jak użytkownicy oceniają produkcje?", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                      plotOutput("dist_ocen")
                      ))
                  ))
             
                
          )
        
        
      )
      
   
      
      
    }
    else {
      loginpage
    }
  })
  #=========================================================================================================================================
  
  
  
  
  #Dodawaniu uzytkownika  
  #----
  observeEvent(input$uz_add,{
    
    if(nrow(uzytkownicy_konta()) >= as.numeric(max_uz())){
      
      showNotification("Konto posiada maksymalną liczbę użytkowników", type = "err")
      
    }
    
    
  })
  
  observeEvent(input$uz_add_fin,{
    
    
    if(input$new_uz_name %in% uzytkownicy_konta()$nazwa){
      
      showNotification("Użytkownik o podannej nazwie istnieje dla tego konta!", type = "err")
      
    }
    else{
      if(nrow(uzytkownicy_konta()) >= as.numeric(max_uz())){
        
        showNotification("Konto posiada maksymalną liczbę użytkowników", type = "err")
        
      }
      else{
        
        res <- dbSendQuery(con, paste0("SELECT add_uz(", id_konta(), ", '", input$new_uz_name, "', ", input$if_baby_add, ");"))
        dbClearResult(res)
        showNotification("Pomyślnie dodano użytkownika!", type = "message")
        
      } 
    }
    
  })
  
  
  #=======  
  
  
  
  
  
  
  ### OUTPUTY ############
  output$tbl <- renderDataTable( plans_modal, options = list(lengthChange = FALSE, searching = FALSE, paging = FALSE, dom = 't'))
  
  
  
  
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("sign-out"), "Wyloguj się", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  
  
  
  
  
  
  #Funkcja do removeUI
  getInputs <- function(pattern){
    reactives <- names(reactiveValuesToList(input))
    reactives[grep(pattern,reactives)]
  }
  
  getOutputs <- function(pattern){
    reactives <- names(reactiveValuesToList(output))
    reactives[grep(pattern,reactives)]
  }
  #===
  
  #Funkcja DO PRZYCISKÓW#============
  
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  #===
  
  
  
  
  
  
  #Szukanie filmów
  #----
  
  myValue7 <- reactiveValues(id = '',
                             title = '',
                             moment = '')
  
  
  
  found_butt <- reactive({
    
    dane <- dbGetQuery(con, paste0("SELECT * FROM szukaj_f('", input$prod_findf, "',", input$uzytkownik, ");"))
    
    
    data = data.table(
      id_p = dane$id_p,
      Tytuł = dane$tytul,
      Reżyser = dane$rezyser,
      Kraj = dane$kraj,
      Długość = dane$dlugosc_filmu,
      Rok = dane$rok_produkcji,
      Oglądaj = shinyInput(actionButton, nrow(dane),
                           'ffbutton_', label = icon("plus") , 
                           onclick = 'Shiny.onInputChange(\"select_buttonfound\",  this.id)'),
      stringsAsFactors = FALSE)
    
  })
  
  
  
  
  
  
  
  
  observeEvent(input$select_buttonfound, {
    selectedRow <- as.numeric(strsplit(input$select_buttonfound, "_")[[1]][2])
    myValue7$id <- found_butt()[selectedRow, id_p]
    
  })
  
  
  output$findf_tab <- DT::renderDataTable({
    
    
    datatable(found_butt()[, -1], rownames = FALSE, escape = FALSE,
              options = list(lengthChange = FALSE, pageLength = 5, searching = FALSE))
    
  })
  
  
  observeEvent(input$confirm_findf, {
    output$found_films <- renderUI({
      wellPanel(
        tags$h1("Wyszukane filmy", class = "text-center", style = "padding-top: 0; font-weight:200;"),
        dataTableOutput("findf_tab")
      )
    })
  })  
  
  
  
  
  observeEvent(input$select_buttonfound, {
    
    
    res <- dbSendQuery(con, paste0("SELECT odtworz_film(", input$uzytkownik, ", ", myValue7$id, ", ", "'00:00:00');"))
    dbClearResult(res)
    showNotification("Dodano film do oglądania!", type = "message")
    
  })
  
  
  
  
  
  
  #==== 
  
  
  
  
  
  
  
  
  #Szukanie seriali
  #----
  myValue8 <- reactiveValues(id = '')
  
  
  
  found_butts <- reactive({
    
    dane <- dbGetQuery(con, paste0("SELECT * FROM szukaj_s('", input$prod_finds, "',", input$uzytkownik, ");"))
    
    data = data.table(
      id_p = dane$id_p,
      Tytuł = dane$tytul,
      Reżyser = dane$rezyser,
      Kraj = dane$kraj,
      Rok = dane$rok_produkcji,
      Oglądaj = shinyInput(actionButton, nrow(dane),
                           'ssbutton_', label = icon("plus") , 
                           onclick = 'Shiny.onInputChange(\"select_buttonfounds\",  this.id)'),
      stringsAsFactors = FALSE)
  })
  
  
  observeEvent(input$select_buttonfounds, {
    selectedRow <- as.numeric(strsplit(input$select_buttonfounds, "_")[[1]][2])
    myValue8$id <- found_butts()[selectedRow, id_p]
    
  })
  
  
  output$finds_tab <- DT::renderDataTable({
    
    
    datatable(found_butts()[, -1], rownames = FALSE, escape = FALSE, options = list(lengthChange = FALSE, pageLength = 5, searching = FALSE))
    
  })
  
  
  
  observeEvent(input$confirm_finds, {
    output$found_series <- renderUI({
      wellPanel(
        tags$h1("Wyszukane seriale", class = "text-center", style = "padding-top: 0; font-weight:200;"),
        dataTableOutput("finds_tab")
      )
    })
    
  })
  
  
  
  
  
  observeEvent(input$select_buttonfounds, {
    
    id_odc <- dbGetQuery(con, paste0("SELECT id_odcinka FROM odcinki WHERE nr_odcinka = 1 AND nr_sezonu = 1 AND id_produkcji = "
                                     , myValue8$id))$id_odcinka
    
    res <- dbSendQuery(con, paste0("SELECT odtworz_serial(", input$uzytkownik, ", ", id_odc, ", ", "'00:00:00');"))
    dbClearResult(res)
    showNotification("Dodano serial do oglądania!", type = "message")
    
  })
  
  
  
  
  #============
  
  
  
  
  
  
  
  
  
  
  
  ###### odtworzenia ###################
  
  
  
  ##Filmy
  #----
  myValue <- reactiveValues(id = '',
                            title = '',
                            moment = '')
  
  
  table_buttons_film <- reactive({
    
    input$select_buttonfound
    input$addtowatch
    input$addtowatch3
    input$stop_moment_button
    
    
    dane <- dbGetQuery(con, paste0("SELECT * FROM odtworzenia_f_u(", input$uzytkownik, ");"))
    
    
    
    data = data.table(
      Tytuł = dane$tytul,
      Zatrzymanie = dane$moment_zatrzymania,
      Akcje = shinyInput(actionButton, nrow(dane),
                         'button_', label = HTML('<i class="fas fa-caret-right"></i> &nbsp;  &nbsp;
                                                     <i class="far fa-thumbs-up"></i>  &nbsp;  &nbsp; 
                                                     <i class="far fa-comment"></i>') , 
                         onclick = 'Shiny.onInputChange(\"select_buttonfilm\",  this.id)' ),
      id_produkcji = dane$id_p,
      stringsAsFactors = FALSE)
    
  })
  
  
  
  
  observeEvent(input$select_buttonfilm, {
    selectedRow <- as.numeric(strsplit(input$select_buttonfilm, "_")[[1]][2])
    myValue$id <- table_buttons_film()[selectedRow, id_produkcji]
    myValue$title <- table_buttons_film()[selectedRow, Tytuł]
    myValue$moment <- table_buttons_film()[selectedRow, Zatrzymanie]
    myValue$max <- dbGetQuery(con, paste0("SELECT dlugosc_filmu FROM produkcje WHERE id_produkcji=", myValue$id, ";"))$dlugosc_filmu
  })
  
  
  
  
  output$test <- renderText({
    
    
    as.character(myValue$moment)
    
  })
  
  
  
  observeEvent(input$select_buttonfilm, ignoreInit = TRUE, {
    output$watch_ui <- renderUI({
      
      
      showModal(
        
        
        modalDialog(
          fluidRow(box(width = 12,
                       sliderInput("stop_moment_film", "Oglądaj dalej!",   
                                   min = as.POSIXct("2017-01-01 00:00:00"),   
                                   max = as.POSIXct(paste("2017-01-01", myValue$max, sep = " ")),   
                                   value = as.POSIXct(paste("2017-01-01", myValue$moment, sep = " ")),   
                                   timeFormat="%T",   
                                   step = 30, animate = T, ticks = F),
                       actionButton("stop_moment_button", "Zatwierdź czas")
          )),
          
          fluidRow(box(width = 12,
                       radioGroupButtons(
                         inputId = "ocena_odtw",
                         label = "Oceń film",
                         choices = 1:10
                       ),
                       actionButton("add_ocena_film", "Zatwierdź ocenę")
          )),
          
          fluidRow(box(width = 12,
                       textInput("odt_kom", "Skomentuj film!"),
                       actionButton("add_film_kom", "Dodaj komentarz")
          ))
          
          
        )
      )
      
    })
    
  })
  
  
  
  observeEvent(input$stop_moment_button, {
    res <- dbSendQuery(con, paste0("SELECT odtworz_film(", input$uzytkownik, ", ", myValue$id, ", '",
                                   str_extract(input$stop_moment_film+3600, "([0-9]+):([0-9]+):([0-9]+)"), "');"))
    dbClearResult(res)
    showNotification("Miło się oglądało?", type = "message")
  })
  
  
  observeEvent(input$add_ocena_film, {
    tryCatch({
      res <- dbSendQuery(con, paste0("INSERT INTO oceny VALUES (", myValue$id, ", ",input$uzytkownik, ", ", input$ocena_odtw, ");"))
      dbFetch(res)
      
      if(dbHasCompleted(res)){
        showNotification("Dziękujemy za ocenę!", type = "message")
      }
      
      
      dbClearResult(res)
    },
    error = function(err){
      showNotification(paste0("Oceniono już tą produkcję!"), type = 'warning')
    })
    
  })
  
  
  observeEvent(input$add_film_kom, {
    
    res <- dbSendQuery(con, paste0("SELECT skomentuj_film(NULL, '", input$odt_kom, "', ", input$uzytkownik, ", ", myValue$id, ");"))
    dbFetch(res)
    if(dbHasCompleted(res)){
      showNotification("Dodano komentarz!", type = "message")
    }
    dbClearResult(res)
  })
  
  
  
  output$odtworzenia_filmow <- DT::renderDataTable({
    
    datatable(table_buttons_film()[,-4], escape = FALSE , options = list(width = 5, searching = FALSE, lengthChange = FALSE, paging=FALSE, dom='ft'))
    
  })
  #=================================
  
  
  
  
  
  
  
  
  ##Seriale=======================
  #----
  myValue2 <- reactiveValues(id = '',
                             id_p = '',
                             title = '',
                             moment = '')
  
  
  table_buttons_serial <- reactive({
    
    input$next_episode
    input$select_buttonfounds
    input$addtowatch2
    input$addtowatch4
    input$stop_moment_button2
    
    dane <- dbGetQuery(con, paste0("SELECT * FROM odtworzenia_s_u(", input$uzytkownik, ");"))
    
    
    data = data.table(
      Tytuł = dane$tytul,
      Tytuł_odcinka = dane$tytul_odcinka,
      Nr_odcinka = dane$nr_odcinka,
      Nr_sezonu = dane$nr_sezonu,
      Zatrzymanie = dane$moment_zatrzymania,
      Akcje = shinyInput(actionButton, nrow(dane), 'sbutton_',
                         label = HTML('<i class="fas fa-caret-right"></i> &nbsp;  &nbsp;<i class="far fa-thumbs-up"></i>  &nbsp;  &nbsp; <i class="far fa-comment"></i>'),
                         onclick = 'Shiny.onInputChange(\"select_buttonserial\",  this.id);' ),
      id_odcinka = dane$id_o,
      id_prod = dane$id_p,
      stringsAsFactors = FALSE)
    
    setnames(data, old = c("Tytuł_odcinka", "Nr_odcinka", "Nr_sezonu"), new = c("Tytuł odcinka", "Nr odcinka", "Nr sezonu"))
    
  })
  
  
  
  observeEvent(input$select_buttonserial, {
    selectedRow <- as.numeric(strsplit(input$select_buttonserial, "_")[[1]][2])
    myValue2$id <- table_buttons_serial()[selectedRow, id_odcinka]
    myValue2$id_p <- table_buttons_serial()[selectedRow, id_prod]
    myValue2$title <- table_buttons_serial()[selectedRow, Tytuł]
    myValue2$moment <- table_buttons_serial()[selectedRow, Zatrzymanie]
    myValue2$max <- dbGetQuery(con, paste0("SELECT dlugosc_odcinka FROM odcinki WHERE id_odcinka=", myValue2$id, ";"))$dlugosc_odcinka
  })
  
  
  output$test1 <- renderText({
    
    
    as.character(myValue2$moment)
    
    
  })
  
  
  
  
  observeEvent(input$select_buttonserial, ignoreInit = TRUE, {
    output$watch_ui2 <- renderUI({
      
      showModal(
        modalDialog(
          
          fluidRow(box(width=12,
                       sliderInput("stop_moment_serial", "Oglądaj dalej!",   
                                   min = as.POSIXct("2017-01-01 00:00:00"),   
                                   max = as.POSIXct(paste("2017-01-01", myValue2$max, sep = " ")),   
                                   value = as.POSIXct(paste("2017-01-01", myValue2$moment, sep = " ")),   
                                   timeFormat="%T",   
                                   step = 30, animate = T, ticks = F),
                       actionButton("stop_moment_button2", "Zatwierdź czas"),
                       actionButton("next_episode", "Oglądaj dalej")
                       
          )),
          fluidRow(box(width = 12,
                       radioGroupButtons(
                         inputId = "ocena_odtw2",
                         label = "Oceń serial",
                         choices = 1:10
                       ),
                       actionButton("add_ocena_serial", "Zatwierdź ocenę")
          )),
          fluidRow(box(width = 12,
                       textInput("odt_kom2", "Skomentuj odcinek"),
                       actionButton("add_serial_kom", "Dodaj komentarz")
                       
          ))
        )
      )
      
    })
    
    
    observeEvent(input$next_episode, {
      id_o <- dbGetQuery(con, paste0("SELECT id_kolejnego_odcinka(", myValue2$id, ");"))
      
      if(id_o == 0){
        showNotification("Produkcja nie ma kolejnych odcinków", type = "warning")
      }
      else{
        
        res <- dbSendQuery(con, paste0("SELECT odtworz_serial(", input$uzytkownik, ", ", id_o, ", '00:00:00');"))
        dbFetch(res)
        if(dbHasCompleted(res)){
          showNotification("Dodano kolejny odcinek do oglądania!", type = "message")
          dbClearResult(res)
        }
        
        
      }
      
      
      
    })
  })
  
  
  
  
  
  
  observeEvent(input$stop_moment_button2, {
    
    if(is.na(str_extract(input$stop_moment_serial+3600, "([0-9]+):([0-9]+):([0-9]+)"))){
      
      res <- dbSendQuery(con, paste0("SELECT odtworz_serial(", input$uzytkownik, ", ", myValue2$id, ", '00:00:00');"))
      
    }
    else{
      
      res <- dbSendQuery(con, paste0("SELECT odtworz_serial(", input$uzytkownik, ", ", myValue2$id, ", '",
                                     str_extract(input$stop_moment_serial+3600, "([0-9]+):([0-9]+):([0-9]+)"), "');"))
    }
    
    dbFetch(res)
    if(dbHasCompleted(res)){
      showNotification("Miło się oglądało?", type = "message")
    }
    dbClearResult(res)
    
  })
  
  
  observeEvent(input$add_ocena_serial, {
    tryCatch({
      res <- dbSendQuery(con, paste0("INSERT INTO oceny VALUES (", myValue2$id_p, ", ", input$uzytkownik, ", ", input$ocena_odtw2, ");"))
      dbFetch(res)
      
      if(dbHasCompleted(res)){
        showNotification("Dziękujemy za ocenę!", type = "message")
      }
      
      dbClearResult(res)
    },
    error = function(err){
      showNotification(paste0("Oceniono już tą produkcję!"), type = 'warning')
    })
    
    
  })
  
  
  observeEvent(input$add_serial_kom, {
    
    res <- dbSendQuery(con, paste0("SELECT skomentuj_serial(NULL, '", input$odt_kom2, "', ", input$uzytkownik, ", ", myValue2$id, ");"))
    dbClearResult(res)
    showNotification("Dodano komentarz!", type = "message")
    
  })
  
  
  
  output$odtworzenia_seriali <- DT::renderDataTable({
    
    #colnames(table_buttons_serial()) <- c("Tytuł", "Tytuł odcinka", "Nr odcinka", "Nr sezonu", "Zatrzymanie", "Akcje",
    #"id_odcinka", "id_prod")
    
    datatable(table_buttons_serial()[,1:6], escape = FALSE , options = list(width = 5, searching = FALSE, lengthChange = FALSE, paging=FALSE, dom='ft'))
    
  })
  
  ######################################################
  
  
  
  
  
  
  
  
  ###### topy #######################
  
  ## top najlepiej oceniane filmy
  #----
  myValue3 <- reactiveValues(id = '',
                             title = '',
                             butt = '')
  
  
  table_buttons_topf <- reactive({
    
    dane <- dbGetQuery(con, paste0("SELECT * FROM top_f(", input$kat_f,",", input$uzytkownik , ");"))
    
    data = data.table(
      Tytuł = dane$tytul,
      Średnia = dane$srednia,
      Akcje = shinyInput(actionButton, nrow(dane),
                         'topfbutton_', label = HTML('<i class="fas fa-caret-right"></i> &nbsp;  &nbsp;
                                                     <i class="far fa-thumbs-up"></i>  &nbsp;  &nbsp; 
                                                     <i class="far fa-comment"></i>'), onclick = 'Shiny.onInputChange(\"select_buttontopf\",  this.id)' ),
      id_p = dane$id_p,
      stringsAsFactors = FALSE)
  })
  
  observeEvent(input$select_buttontopf, {
    selectedRow <- as.numeric(strsplit(input$select_buttontopf, "_")[[1]][2])
    myValue3$id <- as.numeric(table_buttons_topf()[selectedRow, 4])
    
    
  })
  
  
  
  output$top_filmy  <-  DT::renderDataTable({
    
    datatable(table_buttons_topf()[, -4], escape = FALSE , options = list(width = 5, searching = FALSE, lengthChange = FALSE))
    
  })
  
  
  
  
  observeEvent(input$select_buttontopf, ignoreInit = TRUE, {  
    output$watch_ui3 <- renderUI({
      
      
      showModal(
        modalDialog(
          fluidRow(box(width = 12,
                       actionButton("addtowatch", "Dodaj film do oglądania")
          )),
          
          fluidRow(box(width = 12,
                       radioGroupButtons(
                         inputId = "ocena_top",
                         label = "Oceń film",
                         choices = 1:10
                       ),
                       actionButton("add_ocena_top", "Zatwierdź ocenę")
          )),
          
          fluidRow(box(width = 12,
                       textInput("top_kom", "Skomentuj produkcję"),
                       actionButton("add_top_kom", "Dodaj komentarz")
          ))
        )
      ) 
      
    })
  })
  
  
  observeEvent(input$addtowatch, {
    
    res <- dbSendQuery(con, paste0("SELECT odtworz_film(", input$uzytkownik, ", ", myValue3$id, ", ", "'00:00:00');"))
    dbClearResult(res)
    showNotification("Dodano film do oglądania!", type = "message")
    
  })
  
  observeEvent(input$add_ocena_top, {
    tryCatch({
      res <- dbSendQuery(con, paste0("INSERT INTO oceny VALUES (", myValue3$id, ", ", input$uzytkownik, ", ", input$ocena_top, ");"))
      dbFetch(res)
      
      if(dbHasCompleted(res)){
        showNotification("Dziękujemy za ocenę!", type = "message")
      }
      
      dbClearResult(res)
    },
    error = function(err){
      showNotification(paste0("Oceniono już tą produkcję!"), type = 'warning')
    })
    
  })
  
  
  observeEvent(input$add_top_kom, {
    
    res <- dbSendQuery(con, paste0("SELECT skomentuj_film(NULL, '", input$top_kom, "', ", input$uzytkownik, ", ", myValue3$id, ");"))
    dbClearResult(res)
    showNotification("Dodano komentarz!", type = "message")
    
  })
  #===========
  
  
  
  
  ## top najlepiej oceniane seriale 
  #----
  myValue4 <- reactiveValues(id = '',
                             title = '',
                             butt = '')
  
  
  table_buttons_tops <- reactive({
    
    dane <- dbGetQuery(con, paste0("SELECT * FROM top_s(", input$kat_s,",", input$uzytkownik , ");"))
    
    data = data.table(
      Tytuł = dane$tytul,
      Średnia = dane$srednia,
      Akcje = shinyInput(actionButton, nrow(dane),
                         'topsbutton_', label = HTML('<i class="fas fa-caret-right"></i> &nbsp;  &nbsp;
                                                     <i class="far fa-thumbs-up"></i>  &nbsp;  &nbsp; 
                                                     <i class="far fa-comment"></i>'), onclick = 'Shiny.onInputChange(\"select_buttontops\",  this.id)' ),
      id_p = dane$id_p,
      stringsAsFactors = FALSE)
  })
  
  
  
  observeEvent(input$select_buttontops, {
    selectedRow <- as.numeric(strsplit(input$select_buttontops, "_")[[1]][2])
    myValue4$id <- as.numeric(table_buttons_tops()[selectedRow, 4])
    
    
  })
  
  
  
  output$top_seriale <-  DT::renderDataTable({
    
    
    datatable(table_buttons_tops()[, -4], escape = FALSE , options = list(width = 5, searching = FALSE, lengthChange = FALSE))
  })
  
  
  
  
  observeEvent(input$select_buttontops, ignoreInit = TRUE, { 
    output$watch_ui4 <- renderUI({
      
      
      showModal(
        modalDialog(
          fluidRow(box(width=12,
                       actionButton("addtowatch2", "Dodaj serial do oglądania")
          )),
          
          fluidRow(box(width=12,
                       radioGroupButtons(
                         inputId = "ocena_top2",
                         label = "Oceń serial",
                         choices = 1:10
                       ),
                       actionButton("add_ocena_top2", "Zatwierdź ocenę")
          ))
        )
        
      )
      
    })
    
    
  })
  
  
  
  observeEvent(input$addtowatch2, {
    
    id_odc <- dbGetQuery(con, paste0("SELECT id_odcinka FROM odcinki WHERE nr_odcinka = 1 AND nr_sezonu = 1 AND id_produkcji = "
                                     , myValue4$id))$id_odcinka
    
    res <- dbSendQuery(con, paste0("SELECT odtworz_serial(", input$uzytkownik, ", ", id_odc, ", ", "'00:00:00');"))
    dbClearResult(res)
    showNotification("Dodano serial do oglądania!", type = "message")
    
  })
  
  observeEvent(input$add_ocena_top2, {
    tryCatch({
      res <- dbSendQuery(con, paste0("INSERT INTO oceny VALUES (", myValue4$id, ", ", input$uzytkownik, ", ", input$ocena_top2, ");"))
      dbFetch(res)
      
      if(dbHasCompleted(res)){
        showNotification("Dziękujemy za ocenę!", type = "message")
      }
      
      dbClearResult(res)
    },
    error = function(err){
      showNotification(paste0("Oceniono już tą produkcję!"), type = 'warning')
    })
    
    
    
  })
  
  #===========
  
  
  
  
  
  
  ## top najbardziej ogladane filmy
  #----
  
  myValue5 <- reactiveValues(id = '',
                             title = '',
                             butt = '')
  
  
  table_buttons_topof <- reactive({
    
    dane <- dbGetQuery(con, paste0("SELECT * FROM top_o_f(", input$kat_o_f,",", input$uzytkownik , ");"))
    
    data = data.table(
      Tytuł =   dane$tytul,
      Akcje = shinyInput(actionButton, nrow(dane),
                         'topofbutton_', label = HTML('<i class="fas fa-caret-right"></i> &nbsp;  &nbsp;
                                                     <i class="far fa-thumbs-up"></i>  &nbsp;  &nbsp; 
                                                     <i class="far fa-comment"></i>'),
                         onclick = 'Shiny.onInputChange(\"select_buttontopof\",  this.id)' ),
      id_p =   dane$id_p,
      stringsAsFactors = FALSE)
    
  })
  
  
  
  
  
  observeEvent(input$select_buttontopof, {
    selectedRow <- as.numeric(strsplit(input$select_buttontopof, "_")[[1]][2])
    myValue5$id <- as.numeric(table_buttons_topof()[selectedRow, 3])
    
    
  })
  
  
  
  output$top_o_filmy <-  DT::renderDataTable({
    
    
    datatable(table_buttons_topof()[, -3], escape = FALSE , options = list(width = 5, searching = FALSE, lengthChange = FALSE))
  })
  
  
  
  
  observeEvent(input$select_buttontopof, ignoreInit = TRUE, {
    output$watch_ui5 <- renderUI({
      showModal(
        modalDialog(
          fluidRow(box(width=12,
                       actionButton("addtowatch3", "Dodaj film do oglądania")
          )),
          
          fluidRow(box(width=12,
                       radioGroupButtons(
                         inputId = "ocena_top3",
                         label = "Oceń film",
                         choices = 1:10
                       ),
                       actionButton("add_ocena_top3", "Zatwierdź ocenę")
          )),
          
          fluidRow(box(width=12, 
                       textInput("top_kom3", "Skomentuj produkcję"),
                       actionButton("add_top_kom3", "Dodaj komentarz")
          ))
          
        )
      )
    })
    
    
  })  
  
  
  
  
  observeEvent(input$addtowatch3, {
    
    res <- dbSendQuery(con, paste0("SELECT odtworz_film(", input$uzytkownik, ", ", myValue5$id, ", ", "'00:00:00');"))
    dbClearResult(res)
    showNotification("Dodano film do oglądania!", type = "message")
    
  })
  
  observeEvent(input$add_ocena_top3, {
    tryCatch({
      res <- dbSendQuery(con, paste0("INSERT INTO oceny VALUES (", myValue5$id, ", ", input$uzytkownik, ", ", input$ocena_top3, ");"))
      dbFetch(res)
      
      if(dbHasCompleted(res)){
        showNotification("Dziękujemy za ocenę!", type = "message")
      }
      
      dbClearResult(res)
    },
    error = function(err){
      showNotification(paste0("Oceniono już tą produkcję!"), type = 'warning')
    })
    
    
  })
  
  
  observeEvent(input$add_top_kom3, {
    
    res <- dbSendQuery(con, paste0("SELECT skomentuj_film(NULL, '", input$top_kom3, "', ", input$uzytkownik, ", ", myValue5$id, ");"))
    dbClearResult(res)
    showNotification("Dodano komentarz!", type = "message")
    
  })
  
  
  #============== 
  
  
  
  
  
  
  
  ## top najbardziej ogladane seriale
  #----
  myValue6 <- reactiveValues(id = '',
                             title = '',
                             butt = '')
  
  
  table_buttons_topos <- reactive({
    
    dane <- dbGetQuery(con, paste0("SELECT * FROM top_o_s(", input$kat_o_s, ",", input$uzytkownik , ");"))
    
    data = data.table(
      Tytuł = dane$tytul,
      Akcje = shinyInput(actionButton, nrow(dane),
                         'toposbutton_', label = HTML('<i class="fas fa-caret-right"></i> &nbsp;  &nbsp;
                                                     <i class="far fa-thumbs-up"></i>  &nbsp;  &nbsp; 
                                                     <i class="far fa-comment"></i>'),
                         onclick = 'Shiny.onInputChange(\"select_buttontopos\",  this.id)' ),
      id_p =   dane$id_p,
      stringsAsFactors = FALSE)
    
  })
  
  
  
  
  
  observeEvent(input$select_buttontopos, {
    selectedRow <- as.numeric(strsplit(input$select_buttontopos, "_")[[1]][2])
    myValue6$id <- as.numeric(table_buttons_topos()[selectedRow, 3])
    
    
  })
  
  
  
  output$top_o_seriale <-  DT::renderDataTable({
    
    
    datatable(table_buttons_topos()[, -3], escape = FALSE , options = list(width = 5, searching = FALSE, lengthChange = FALSE))
  })
  
  
  
  
  observeEvent(input$select_buttontopos, ignoreInit =  TRUE, {
    output$watch_ui6 <- renderUI({
      showModal(
        modalDialog(
          fluidRow(box(width=12, 
                       actionButton("addtowatch4", "Dodaj serial do oglądania")
          )),
          
          fluidRow(box(width=12,
                       radioGroupButtons(
                         inputId = "ocena_top4",
                         label = "Oceń serial",
                         choices = 1:10
                       ),
                       actionButton("add_ocena_top4", "Zatwierdź ocenę")
          ))
        )
      )
    })
    
  })  
  
  
  
  
  
  observeEvent(input$addtowatch4, {
    
    id_odc <- dbGetQuery(con, paste0("SELECT id_odcinka FROM odcinki WHERE nr_odcinka = 1 AND nr_sezonu = 1 AND id_produkcji = "
                                     , myValue6$id))$id_odcinka
    
    res <- dbSendQuery(con, paste0("SELECT odtworz_serial(", input$uzytkownik, ", ", id_odc, ", ", "'00:00:00');"))
    dbClearResult(res)
    showNotification("Dodano serial do oglądania!", type = "message")
    
  })
  
  observeEvent(input$add_ocena_top4, {
    tryCatch({
      res <- dbSendQuery(con, paste0("INSERT INTO oceny VALUES (", myValue6$id, ", ", input$uzytkownik, ", ", input$ocena_top4, ");"))
      dbFetch(res)
      
      if(dbHasCompleted(res)){
        showNotification("Dziękujemy za ocenę!", type = "message")
      }
      
      dbClearResult(res)
    },
    error = function(err){
      showNotification(paste0("Oceniono już tą produkcję!"), type = 'warning')
    })
    
  })
  
  #====================================================
  
  
  
  
  
  observeEvent(input$potwierdz, {
    tryCatch({
      res <- dbSendQuery(con, paste0("SELECT zaplac(", id_konta(), ", ", input$zaplac, ");"))
      
      dbFetch(res)
      
      if(dbHasCompleted(res)){
        showNotification("Płatność zaksięgowana", type = "message")
      }
      
      dbClearResult(res)
    },
    error = function(err){
      showNotification(paste0("Ups, coś poszło nie tak"), type = 'warning')
    })
    
  })
  
  
  
  
  
  
  
  
  ## płatności
  
  platnosci <- reactive({
    
    input$potwierdz
    
    dt <- as.data.table(dbGetQuery(con, paste0("SELECT * FROM plat_konta(", id_konta(), ");")))
    
    colnames(dt) <- c("Data", "Kwota")
    
    dt
    
  })
  
  
  
  output$platnosci  <-  DT::renderDataTable({
    
    datatable(platnosci(), options = list(width = 5, lengthChange = FALSE, searching = FALSE, dom = 'ft'))
  })
  
  #######################################
  
  
  
  
  #Wykresy do statystyk  
  #----  
  ##
  output$pie_kraj <- renderPlot({
    
    produkcje <- as.data.table(dbGetQuery(con, "SELECT * FROM produkcje;"))
    kraje_il <- produkcje[, .N, by = kraj]
    best <- kraje_il[order(N, decreasing = TRUE)[1:5], kraj]
    kraje_il[!(kraj %in% best), kraj := "inne"]
    kraje_il <- kraje_il[, sum(N), by = kraj]
    
    
    kraje_il <- kraje_il %>% 
      arrange(desc(kraj)) %>%
      mutate(prop = V1 / sum(kraje_il$V1) *100) %>%
      mutate(ypos = cumsum(prop)- 0.5*prop )
    
    x <- kraje_il[, prop]
    
    y <- floor(x)
    
    ile <- 100 - sum(y)
    
    which_add_vec <- head(order(x-y, decreasing = TRUE), ile)
    
    y[which_add_vec] <- y[which_add_vec] +1
    
    
    ggplot(kraje_il, aes(x = "", y = prop, fill = kraj)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      geom_text(aes(y = ypos, label = paste(y, "%")), color = "white", size=6) +
      scale_fill_manual(values=c("#55DDE0", "#999999", "#2F4858", "#F6AE2D", "#F26419", "#33658A")) +
      labs(fill = "Kraj")
    
  })  
  
  ##
  
  
  output$dist_ocen <- renderPlot({
    
    ocenki <- as.data.table(dbGetQuery(con, "SELECT ocena FROM oceny;"))
    ggplot(ocenki) +
      geom_histogram(aes(x = ocena), binwidth = 1, color = "pink", fill =  "#33658A") +
      theme_classic() +
      labs(x = "Ocena", y = "Ilość występowania") +
      scale_x_continuous(limits = c(0, 11), breaks = 1:10,
                         labels = 1:10)
    
  })
  
  ##
  
  output$pie_kat <- renderPlot({
    
    produkcje_kat <- as.data.table(dbGetQuery(con, "SELECT * FROM produkcje_with_kat;"))
    setnames(produkcje_kat, old = c('nazwa_kategorii'), new = c("nazwa"))
    
    kat_il <- produkcje_kat[, .N, by = nazwa]
    kat_il <- kat_il[order(N, decreasing = TRUE)[1:5]]
    
    
    kat_il <- kat_il %>% 
      arrange(desc(nazwa)) %>%
      mutate(prop = N / sum(kat_il$N) *100) %>%
      mutate(ypos = cumsum(prop)- 0.5*prop )
    
    x <- kat_il[, prop]
    
    y <- floor(x)
    
    ile <- 100 - sum(y)
    
    which_add_vec <- head(order(x-y, decreasing = TRUE), ile)
    
    y[which_add_vec] <- y[which_add_vec] +1
    
    
    
    # piechart
    ggplot(kat_il, aes(x = "", y = prop, fill = nazwa)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      geom_text(aes(y = ypos, label = paste(y, "%")), color = "white", size=6) +
      scale_fill_manual(values=c("#55DDE0", "#999999", "#2F4858", "#F6AE2D", "#F26419")) +
      labs(fill = "Nazwa kategorii")
    
  })  
  
  
  #==============================  
  
  
  
  
  
  
  
  
} 
