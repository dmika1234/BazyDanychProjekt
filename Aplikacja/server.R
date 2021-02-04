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
  
  
  
  uzytkownicy_konta <- reactive({
    #a <- input$uz_add_fin
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
                  menuItem("Top listy", tabName = "topy", icon = icon("line-chart")),
                  menuItem("Top listy", tabName = "dashboard", icon = icon("thumbs-up"))
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
                    ),
                    box(width = 5, 
                        tags$h2("Ostatnie płatności", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                        dataTableOutput('platnosci')
                        
                    ))
                
        ),
        #=======================
        
        tabItem(tabName ="ogladanie",
                #---- 
                fluidRow(
                  box(width = 5, 
                      tags$h2("Oglądaj dalej film!", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                      dropdownButton(textInput("prod_findf", "Wyszukaj film", placeholder = "Tytuł"),
                                     actionButton("confirm_findf", "Szukaj"),
                                     icon = icon("search")),
                      dataTableOutput('odtworzenia_filmow'),
                      br(),
                      br(),
                      uiOutput("found_films")
                      
                      #textOutput("test")
                  ),
                  
                  uiOutput("watch_ui"),
                  
                  box(width = 7, 
                      tags$h2("Oglądaj dalej serial!", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                      dropdownButton(textInput("prod_finds", "Wyszukaj serial", placeholder = "Tytuł"),
                                     actionButton("confirm_finds", "Szukaj"),
                                     icon = icon("search")),
                      dataTableOutput('odtworzenia_seriali'),
                      br(),
                      br(),
                      uiOutput("found_series")
                      #textOutput("test1"),
                  ),
                  
                  uiOutput("watch_ui2"),
                )
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
        )
        #=========
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
  output$tbl <- renderDataTable( plans_modal, options = list(lengthChange = FALSE, searching = FALSE, paging = FALSE))
  
  
  
  
  
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
    
    data = data.table(
      id_p = dbGetQuery(con, paste0("SELECT * FROM produkcje WHERE czy_serial = FALSE AND tytul = '", input$prod_findf, "';"))$id_produkcji,
      Tytuł = dbGetQuery(con, paste0("SELECT * FROM produkcje WHERE czy_serial = FALSE AND tytul = '", input$prod_findf, "';"))$tytul,
      Reżyser = dbGetQuery(con, paste0("SELECT * FROM produkcje WHERE czy_serial = FALSE AND tytul = '", input$prod_findf, "';"))$rezyser,
      Kraj = dbGetQuery(con, paste0("SELECT * FROM produkcje WHERE czy_serial = FALSE AND tytul = '", input$prod_findf, "';"))$kraj,
      Długość = dbGetQuery(con, paste0("SELECT * FROM produkcje WHERE czy_serial = FALSE AND tytul = '", input$prod_findf, "';"))$dlugosc_filmu,
      Rok = dbGetQuery(con, paste0("SELECT * FROM produkcje WHERE czy_serial = FALSE AND tytul = '", input$prod_findf, "';"))$rok_produkcji,
      Oglądaj = shinyInput(actionButton, nrow(dbGetQuery(con, paste0("SELECT * FROM produkcje WHERE czy_serial = FALSE AND tytul = '", input$prod_findf, "';"))),
                           'ffbutton_', label = icon("plus") , 
                           onclick = 'Shiny.onInputChange(\"select_buttonfound\",  this.id)'),
      stringsAsFactors = FALSE)
  })
  
  
  observeEvent(input$select_buttonfound, {
    selectedRow <- as.numeric(strsplit(input$select_buttonfound, "_")[[1]][2])
    myValue7$id <- found_butt()[selectedRow, id_p]
    
  })
  
  
  output$findf_tab <- DT::renderDataTable({
    
    
    datatable(found_butt()[, -1], rownames = FALSE, escape = FALSE, options = list(lengthChange = FALSE, searching = FALSE, paging = FALSE, dom = 't'))
    
  })
  
  
  output$found_films <- renderUI({
    req(input$confirm_findf)
    wellPanel(
      tags$h1("Wyszukane filmy", class = "text-center", style = "padding-top: 0; font-weight:200;"),
      dataTableOutput("findf_tab")
    )
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
    
    data = data.table(
      id_p = dbGetQuery(con, paste0("SELECT * FROM produkcje WHERE czy_serial = TRUE AND tytul = '", input$prod_finds, "';"))$id_produkcji,
      Tytuł = dbGetQuery(con, paste0("SELECT * FROM produkcje WHERE czy_serial = TRUE AND tytul = '", input$prod_finds, "';"))$tytul,
      Reżyser = dbGetQuery(con, paste0("SELECT * FROM produkcje WHERE czy_serial = TRUE AND tytul = '", input$prod_finds, "';"))$rezyser,
      Kraj = dbGetQuery(con, paste0("SELECT * FROM produkcje WHERE czy_serial = TRUE AND tytul = '", input$prod_finds, "';"))$kraj,
      Długość = dbGetQuery(con, paste0("SELECT * FROM produkcje WHERE czy_serial = TRUE AND tytul = '", input$prod_finds, "';"))$dlugosc_filmu,
      Rok = dbGetQuery(con, paste0("SELECT * FROM produkcje WHERE czy_serial = TRUE AND tytul = '", input$prod_finds, "';"))$rok_produkcji,
      Oglądaj = shinyInput(actionButton, nrow(dbGetQuery(con, paste0("SELECT * FROM produkcje WHERE czy_serial = TRUE AND tytul = '", input$prod_finds, "';"))),
                           'ssbutton_', label = icon("plus") , 
                           onclick = 'Shiny.onInputChange(\"select_buttonfounds\",  this.id)'),
      stringsAsFactors = FALSE)
  })
  
  
  observeEvent(input$select_buttonfounds, {
    selectedRow <- as.numeric(strsplit(input$select_buttonfounds, "_")[[1]][2])
    myValue8$id <- found_butts()[selectedRow, id_p]
    
  })
  
  
  output$finds_tab <- DT::renderDataTable({
    
    
    datatable(found_butts()[, -1], rownames = FALSE, escape = FALSE, options = list(lengthChange = FALSE, searching = FALSE, paging = FALSE, dom = 't'))
    
  })
  
  
  output$found_series <- renderUI({
    req(input$confirm_finds)
    wellPanel(
      tags$h1("Wyszukane seriale", class = "text-center", style = "padding-top: 0; font-weight:200;"),
      dataTableOutput("finds_tab")
    )
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
    
    data = data.frame(
      Tytuł = dbGetQuery(con, paste0("SELECT * FROM odtworzenia_f_u(", input$uzytkownik, ");"))$tytul,
      Zatrzymanie = dbGetQuery(con, paste0("SELECT * FROM odtworzenia_f_u(", input$uzytkownik, ");"))$moment_zatrzymania,
      Akcje = shinyInput(actionButton, nrow(dbGetQuery(con, paste0("SELECT * FROM odtworzenia_f_u(", input$uzytkownik, ");"))),
                         'button_', label = HTML('<i class="fas fa-caret-right"></i> &nbsp;  &nbsp;
                                                     <i class="far fa-thumbs-up"></i>  &nbsp;  &nbsp; 
                                                     <i class="far fa-comment"></i>') , 
                         onclick = 'Shiny.onInputChange(\"select_buttonfilm\",  this.id)' ),
      id_produkcji = dbGetQuery(con, paste0("SELECT * FROM odtworzenia_f_u(", input$uzytkownik, ");"))$id_p,
      stringsAsFactors = FALSE)
  })
  
  
  
  
  observeEvent(input$select_buttonfilm, {
    selectedRow <- as.numeric(strsplit(input$select_buttonfilm, "_")[[1]][2])
    myValue$id <- table_buttons_film()[selectedRow, 4]
    myValue$title <- table_buttons_film()[selectedRow, 1]
    myValue$moment <- table_buttons_film()[selectedRow, 2]
  })
  
  
  
  
  output$test <- renderText({
    
    
    as.character(myValue$moment)
    
  })
  
  
  output$watch_ui <- renderUI({
    req(input$select_buttonfilm)
    modalDialog(
      fluidRow(box(width = 12,
                   sliderInput("stop_moment_film", "Oglądaj dalej!",   
                  min = as.POSIXct("2017-01-01 00:00:00"),   
                  max = as.POSIXct(paste("2017-01-01",
                                         dbGetQuery(con,
                                                    paste0("SELECT dlugosc_filmu FROM produkcje WHERE id_produkcji="
                                                           , myValue$id, ";"))$dlugosc_filmu, sep = " ")),   
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
    
    datatable(table_buttons_film()[,-4], escape = FALSE , options = list(width = 5, searching = FALSE, lengthChange = FALSE))
    
  })
  #=================================
  
  
  
  
  
  
  
  
  ##Seriale=======================
  #----
  myValue2 <- reactiveValues(id = '',
                             id_p = '',
                             title = '',
                             moment = '')
  
  
  table_buttons_serial <- reactive({
    
    input$select_buttonfounds
    input$addtowatch2
    input$addtowatch4
    input$stop_moment_button2
    
    data = data.table(
      Tytuł = dbGetQuery(con, paste0("SELECT * FROM odtworzenia_s_u(", input$uzytkownik, ");"))$tytul,
      Tytuł_odcinka = dbGetQuery(con, paste0("SELECT * FROM odtworzenia_s_u(", input$uzytkownik, ");"))$tytul_odcinka,
      Nr_odcinka = dbGetQuery(con, paste0("SELECT * FROM odtworzenia_s_u(", input$uzytkownik, ");"))$nr_odcinka,
      Nr_sezonu = dbGetQuery(con, paste0("SELECT * FROM odtworzenia_s_u(", input$uzytkownik, ");"))$nr_sezonu,
      Zatrzymanie = dbGetQuery(con, paste0("SELECT * FROM odtworzenia_s_u(", input$uzytkownik, ");"))$moment_zatrzymania,
      Akcje = shinyInput(actionButton, nrow(dbGetQuery(con, paste0("SELECT * FROM odtworzenia_s_u(", input$uzytkownik, ");"))),
                         'sbutton_', label = HTML('<i class="fas fa-caret-right"></i> &nbsp;  &nbsp;
                                                     <i class="far fa-thumbs-up"></i>  &nbsp;  &nbsp; 
                                                     <i class="far fa-comment"></i>'), onclick = 'Shiny.onInputChange(\"select_buttonserial\",  this.id)' ),
      id_odcinka = dbGetQuery(con, paste0("SELECT * FROM odtworzenia_s_u(", input$uzytkownik, ");"))$id_o,
      id_prod = dbGetQuery(con, paste0("SELECT * FROM odtworzenia_s_u(", input$uzytkownik, ");"))$id_p,
      stringsAsFactors = FALSE)
    
    
    
  })
  
  
  
  observeEvent(input$select_buttonserial, {
    selectedRow <- as.numeric(strsplit(input$select_buttonserial, "_")[[1]][2])
    myValue2$id <- table_buttons_serial()[selectedRow, id_odcinka]
    myValue2$id_p <- table_buttons_serial()[selectedRow, id_prod]
    myValue2$title <- table_buttons_serial()[selectedRow, Tytuł]
    myValue2$moment <- table_buttons_serial()[selectedRow, Zatrzymanie]
  })
  
  
  output$test1 <- renderText({
    
    
    as.character(myValue2$moment)
    
  })
  
  
  output$watch_ui2 <- renderUI({
    req(input$select_buttonserial)
    modalDialog(
      
      fluidRow(box(width=12,
                  sliderInput("stop_moment_serial", "Oglądaj dalej!",   
                  min = as.POSIXct("2017-01-01 00:00:00"),   
                  max = as.POSIXct(paste("2017-01-01",
                                         dbGetQuery(con,
                                                    paste0("SELECT dlugosc_odcinka FROM odcinki WHERE id_odcinka="
                                                           , myValue2$id, ";"))$dlugosc_odcinka, sep = " ")),   
                  value = as.POSIXct(paste("2017-01-01", myValue2$moment, sep = " ")),   
                  timeFormat="%T",   
                  step = 30, animate = T, ticks = F),
                  actionButton("stop_moment_button2", "Zatwierdź czas")
                   
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
  })
  
  
  
  observeEvent(input$stop_moment_button2, {
    res <- dbSendQuery(con, paste0("SELECT odtworz_serial(", input$uzytkownik, ", ", myValue2$id, ", '",
                                   str_extract(input$stop_moment_serial+3600, "([0-9]+):([0-9]+):([0-9]+)"), "');"))
    showNotification("Miło się oglądało?", type = "message")
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
    
    datatable(table_buttons_serial()[,1:6], escape = FALSE , options = list(width = 5, searching = FALSE, lengthChange = FALSE))
    
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
  
  
  
  
  output$watch_ui3 <- renderUI({
    req(input$select_buttontopf)
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
  
  
  output$watch_ui4 <- renderUI({
    req(input$select_buttontops)
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
  
  
  output$watch_ui5 <- renderUI({
    req(input$select_buttontopof)
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
  
  
  
  
  output$watch_ui6 <- renderUI({
    req(input$select_buttontopos)
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
  
  
  
  
  
  
  
  
  
  
  
  
  ## płatności
  
  output$platnosci  <-  DT::renderDataTable({
    
    platnosci <- as.data.table(dbGetQuery(con, paste0("SELECT * FROM plat_konta(", id_konta(), ");")))
    
    colnames(platnosci) <- c("Data", "Kwota")
    
    datatable(platnosci, options = list(width = 5, lengthChange = FALSE, searching = FALSE))
  })
  
  #######################################
  
  
  
  
  
  
  
  
  
  
  
} 
