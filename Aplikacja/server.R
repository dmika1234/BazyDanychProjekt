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
      },
      error = function(err){
        showNotification(paste0("Podany e-mail nie istnieje!"), type = 'err')
      })
      
      
      showNotification("GITUWA", type = "message") 
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
                      dataTableOutput('odtworzenia_filmow')),
                      uiOutput("watch_ui"),
                  box(width = 7, 
                      tags$h2("Oglądaj dalej serial!", class = "text-center", style = "padding-top: 0; font-weight:600;"),
                      dataTableOutput('odtworzenia_seriali')),
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
                      dataTableOutput('top_seriale'))
                  
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
                      )
                  
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
        
        dbSendQuery(con, paste0("SELECT add_uz(", id_konta(), ", '", input$new_uz_name, "', ", input$if_baby_add, ");"))
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
  
  
  
  
  
  #Funkcja DO PRZYCISKÓW#============
  
    
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }
    #===
    
    
    
    ###### odtworzenia ###################
    
  
    
    ##Filmy
    #----
    myValue <- reactiveValues(id = '',
                              title = '',
                              moment = '')
    
    
    table_buttons_film <- reactive({
      input$addtowatch
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
  
  
    
    
    
    output$watch_ui <- renderUI({
      req(input$select_buttonfilm)
        modalDialog(
          
          sliderInput("stop_moment_film", "Oglądaj dalej!",   
                      min = as.POSIXct("2017-01-01 00:00:00"),   
                      max = as.POSIXct(paste("2017-01-01",
                                             dbGetQuery(con,
                                                        paste0("SELECT dlugosc_filmu FROM produkcje WHERE id_produkcji="
                                                               , myValue$id, ";"))$dlugosc_filmu, sep = " ")),   
                      value = as.POSIXct(paste("2017-01-01", myValue$moment, sep = " ")),   
                      timeFormat="%T",   
                      step = 30, animate = T, ticks = F),
        actionButton("stop_moment_button", "Zatwierdź czas"),
        radioGroupButtons(
          inputId = "ocena_odtw",
          label = "Oceń film",
          choices = 1:10
        ),
        actionButton("add_ocena_film", "Zatwierdź ocenę"),
        textInput("odt_kom", "Skomentuj film!"),
        actionButton("add_film_kom", "Dodaj komentarz")
        
                  )
    })
    
    observeEvent(input$stop_moment_button, {
      dbSendQuery(con, paste0("SELECT odtworz_film(", input$uzytkownik, ", ", myValue$id, ", '",
                              str_extract(input$stop_moment_film+3600, "([0-9]+):([0-9]+):([0-9]+)"), "');"))
      showNotification("Miło się oglądało?", type = "message")
    })
    
    
    observeEvent(input$add_ocena_film, {
      tryCatch({
        res <- dbSendQuery(con, paste0("INSERT INTO oceny VALUES (", myValue$id, ", ",input$uzytkownik, ", ", input$ocena_odtw, ");"))
      },
      error = function(err){
        showNotification(paste0("Oceniono już tą produkcję!"), type = 'warning')
      })
  
      if(!(myValue$id %in% dbGetQuery(con, paste0("SELECT id_produkcji FROM oceny WHERE id_uzytkownika=", input$uzytkownik, ";"))$id_produkcji)){
        
        showNotification("Dziękujemy za ocenę!", type = "message")
      }
      
    })
    
    
    observeEvent(input$add_film_kom, {
      
      dbSendQuery(con, paste0("SELECT skomentuj_film(NULL, '", input$odt_kom, "', ", input$uzytkownik, ", ", myValue$id, ");"))
      showNotification("Dodano komentarz!", type = "message")
      
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
    myValue2$id <- table_buttons_serial()[selectedRow, 7]
    myValue2$id_p <- table_buttons_serial()[selectedRow, 8]
    myValue2$title <- table_buttons_serial()[selectedRow, 1]
    myValue2$moment <- as.character(table_buttons_serial()[selectedRow, 5])
  })
  
  

  
  output$watch_ui2 <- renderUI({
    req(input$select_buttonserial)
    modalDialog(

      sliderInput("stop_moment_serial", "Oglądaj dalej!",   
                  min = as.POSIXct("2017-01-01 00:00:00"),   
                  max = as.POSIXct(paste("2017-01-01",
                                         dbGetQuery(con,
                                                    paste0("SELECT dlugosc_odcinka FROM odcinki WHERE id_odcinka="
                                                           , myValue2$id, ";"))$dlugosc_odcinka, sep = " ")),   
                  value = as.POSIXct(paste("2017-01-01", myValue2$moment, sep = " ")),   
                  timeFormat="%T",   
                  step = 30, animate = T, ticks = F),
      actionButton("stop_moment_button2", "Zatwierdź czas"),
      radioGroupButtons(
        inputId = "ocena_odtw2",
        label = "Oceń serial",
        choices = 1:10
      ),
      actionButton("add_ocena_serial", "Zatwierdź ocenę"),
      textInput("odt_kom2", "Skomentuj odcinek"),
      actionButton("add_serial_kom", "Dodaj komentarz")
      
    )
  })
  
  
  
  observeEvent(input$stop_moment_button2, {
    dbSendQuery(con, paste0("SELECT odtworz_serial(", input$uzytkownik, ", ", myValue2$id, ", '",
                            str_extract(input$stop_moment_serial+3600, "([0-9]+):([0-9]+):([0-9]+)"), "');"))
    showNotification("Miło się oglądało?", type = "message")
  })
  
  
  observeEvent(input$add_ocena_serial, {
    tryCatch({
      res <- dbSendQuery(con, paste0("INSERT INTO oceny VALUES (", myValue2$id_p, ", ", input$uzytkownik, ", ", input$ocena_odtw2, ");"))
    },
    error = function(err){
      showNotification(paste0("Oceniono już tą produkcję!"), type = 'warning')
    })
    
    if(!(myValue2$id_p %in% dbGetQuery(con, paste0("SELECT id_produkcji FROM oceny WHERE id_uzytkownika=", input$uzytkownik, ";"))$id_produkcji)){
      
      showNotification("Dziękujemy za ocenę!", type = "message")
    }

    
  })
  
  
  observeEvent(input$add_serial_kom, {
    
    dbSendQuery(con, paste0("SELECT skomentuj_serial(NULL, '", input$odt_kom2, "', ", input$uzytkownik, ", ", myValue2$id, ");"))
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

    
    if(input$kat_f == 0){
      data = data.table(
        Tytuł = dbGetQuery(con, "SELECT * FROM top_filmow;")$Tytul,
        Średnia = dbGetQuery(con, "SELECT * FROM top_filmow;")$'Srednia ocen',
        Akcje = shinyInput(actionButton, nrow(dbGetQuery(con, "SELECT * FROM top_filmow;")),
                           'topfbutton_', label = HTML('<i class="fas fa-caret-right"></i> &nbsp;  &nbsp;
                                                     <i class="far fa-thumbs-up"></i>  &nbsp;  &nbsp; 
                                                     <i class="far fa-comment"></i>'),
                           onclick = 'Shiny.onInputChange(\"select_buttontopf\",  this.id)' ),
        id_p = dbGetQuery(con, "SELECT * FROM top_filmow;")$id_p,
        stringsAsFactors = FALSE)
    }
    else{
      data = data.table(
        Tytuł = dbGetQuery(con, paste0("SELECT * FROM top_f(", input$kat_f, ");"))$tytul,
        Średnia = dbGetQuery(con, paste0("SELECT * FROM top_f(", input$kat_f, ");"))$srednia,
        Akcje = shinyInput(actionButton, nrow(dbGetQuery(con, "SELECT * FROM top_filmow;")),
                           'topfbutton_', label = HTML('<i class="fas fa-caret-right"></i> &nbsp;  &nbsp;
                                                     <i class="far fa-thumbs-up"></i>  &nbsp;  &nbsp; 
                                                     <i class="far fa-comment"></i>'), onclick = 'Shiny.onInputChange(\"select_buttontopf\",  this.id)' ),
        id_p = dbGetQuery(con, paste0("SELECT * FROM top_f(", input$kat_f, ");"))$id_p,
        stringsAsFactors = FALSE)
    }
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
      actionButton("addtowatch", "Dodaj film do oglądania"),
      radioGroupButtons(
        inputId = "ocena_top",
        label = "Oceń film",
        choices = 1:10
      ),
      actionButton("add_ocena_top", "Zatwierdź ocenę"),
      textInput("top_kom", "Skomentuj produkcję"),
      actionButton("add_top_kom", "Dodaj komentarz")
      
    )
  })
  
  
  observeEvent(input$addtowatch, {
    
    dbSendQuery(con, paste0("SELECT odtworz_film(", input$uzytkownik, ", ", myValue3$id, ", ", "'00:00:00');"))
    showNotification("Dodano film do oglądania!", type = "message")
    
  })
  
  observeEvent(input$add_ocena_top, {
    tryCatch({
      res <- dbSendQuery(con, paste0("INSERT INTO oceny VALUES (", myValue3$id, ", ", input$uzytkownik, ", ", input$ocena_top, ");"))
    },
    error = function(err){
      showNotification(paste0("Oceniono już tą produkcję!"), type = 'warning')
    })
    
    if(!(myValue3$id %in% dbGetQuery(con, paste0("SELECT id_produkcji FROM oceny WHERE id_uzytkownika=", input$uzytkownik, ";"))$id_produkcji)){
      
      showNotification("Dziękujemy za ocenę!", type = "message")
    }
    
    
  })
  

  observeEvent(input$add_top_kom, {
    
    dbSendQuery(con, paste0("SELECT skomentuj_film(NULL, '", input$top_kom, "', ", input$uzytkownik, ", ", myValue3$id, ");"))
    showNotification("Dodano komentarz!", type = "message")
    
  })
  #===========
  
  
  
  
  ## top najlepiej oceniane seriale 
  
  output$top_seriale  <-  DT::renderDataTable({
    
    if(input$kat_s == 0){
      top_s <- as.data.table(dbGetQuery(con, "SELECT * FROM top_seriali;"))
    }
    else{
      top_s <- as.data.table(dbGetQuery(con, paste0("SELECT * FROM top_s(", input$kat_s, ");")))
    }
    
    colnames(top_s) <- c("Tytuł", "Średnia")
    
    top_s[[HTML("Oglądaj/Oceń/ <br/> Skomentuj")]] <-
      paste0(HTML('
          <div class="btn-group" role="group" aria-label="Basic example">
          <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(top_s),'> <i class="fas fa-caret-right"></i> </button>
          <button type="button" class="btn btn-secondary delete" id=rate',1:nrow(top_s), '><i class="far fa-thumbs-up"></i></button> 
          <button type="button" class="btn btn-secondary delete" id=comment',1:nrow(top_s),'><i class="far fa-comment"></i> </button>
          </div>'))
    
    datatable(top_s, options = list(width = 5, lengthChange = FALSE, searching = FALSE), escape = FALSE)
  })
  
  
  
  ## top najbardziej ogladane filmy
  
  output$top_o_filmy  <-  DT::renderDataTable({
    
    if(input$kat_o_f == 0){
      top_f <- as.data.table(dbGetQuery(con, "SELECT * FROM top_o_filmow;"))
    }
    else{
      top_f <- as.data.table(dbGetQuery(con, paste0("SELECT * FROM top_o_f(", input$kat_o_f, ");")))
    }
    
    colnames(top_f) <- c("Tytuł")
    
    top_f[[HTML("Oglądaj/Oceń/ <br/> Skomentuj")]] <-
      paste0(HTML('
          <div class="btn-group" role="group" aria-label="Basic example">
          <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(top_f),'> <i class="fas fa-caret-right"></i> </button>
          <button type="button" class="btn btn-secondary delete" id=rate',1:nrow(top_f), '><i class="far fa-thumbs-up"></i></button> 
          <button type="button" class="btn btn-secondary delete" id=comment',1:nrow(top_f),'><i class="far fa-comment"></i> </button>
          </div>'))
    
    datatable(top_f, options = list(width = 5, lengthChange = FALSE, searching = FALSE), escape = FALSE)
  })
  
  ## top najbardziej ogladane seriale
  
  output$top_o_seriale  <-  DT::renderDataTable({
    
    if(input$kat_o_s == 0){
      top_s <- as.data.table(dbGetQuery(con, "SELECT * FROM top_o_seriali;"))
    }
    else{
      top_s <- as.data.table(dbGetQuery(con, paste0("SELECT * FROM top_o_s(", input$kat_o_s, ");")))
      
    }
    

    top_s[[HTML("Oglądaj/Oceń/ <br/> Skomentuj")]] <-
      paste0(HTML('
          <div class="btn-group" role="group" aria-label="Basic example">
          <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(top_s),'> <i class="fas fa-caret-right"></i> </button>
          <button type="button" class="btn btn-secondary delete" id=rate',1:nrow(top_s), '><i class="far fa-thumbs-up"></i></button> 
          <button type="button" class="btn btn-secondary delete" id=comment',1:nrow(top_s),'><i class="far fa-comment"></i> </button>
          </div>'))

    
    datatable(top_s, options = list(width = 5, lengthChange = FALSE, searching = FALSE), escape = FALSE)
  })
  
  ## płatności
  
    output$platnosci  <-  DT::renderDataTable({
    
    platnosci <- as.data.table(dbGetQuery(con, paste0("SELECT * FROM plat_konta(", id_konta(), ");")))
    
    colnames(platnosci) <- c("Data", "Kwota")
    
    datatable(platnosci, options = list(width = 5, lengthChange = FALSE, searching = FALSE))
  })
  
  #######################################
  
} 
