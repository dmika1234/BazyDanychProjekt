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
  
  
  id_konta <- reactive({
    
    id_konta <- us()[us()$email == input$userName]$id_konta
    
  })
  
  
  
  uzytkownicy_konta <- reactive({
    
    dbGetQuery(con, paste0("SELECT * FROM uzytkownicy WHERE id_konta = '", id_konta(), "';"))

    
  })  
  
  # 
  # ikonki_konta <- reactive({
  #   
  #   x <- 1:nrow(uzytkownicy_konta())
  #   
  #   for(i in 1:nrow(uzytkownicy_konta())) {
  #     
  #     if(uzytkownicy_konta$czy_dziecko[i] == FALSE)
  #       {
  #       x[i] <- "fas fa-user-ninja"
  #     }
  #     else
  #       {
  #       x[i] <- "fas fa-child"
  #     }
  #   }
  #     x
  #   
  #   
  # })
  
  
  max_uz <- reactive({
    
    
    as.numeric(dbGetQuery(con, paste0("SELECT p.max_osob FROM plany p
                           JOIN konta k ON k.id_planu = p.id_planu
                            WHERE k.id_konta = '", id_konta(), "';")))
    
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
  
  

  
  
  
  
  
  
  
  
  
  
  
  

  
  
  #Panel boczny po zalogowaniu===================================================================================================================  
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
  
?radioGroupButtons
  
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
                  ),
                  div(
                    style = "text-align: center;",
                  actionBttn("uz_add", "Dodaj użytkownika", style = "pill", 
                             color = "danger")),
                  bsModal("uz_add_modal", "Dodanie użytkownika", "uz_add", size = "large", 
                          wellPanel(
                            textInput("new_uz_name", placeholder = "Nazwa", label = "Podaj nazwę użytkownika"),
                            materialSwitch(
                              inputId = "if_baby_add",
                              label = "Czy użytkownik to dziecko?", 
                              status = "primary",
                              right = TRUE
                            ),
                            actionButton("uz_add_fin", "Dodaj")
                          )
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
                HTML(
                  '<div id="respond">
                    
                    <h3>Leave a Comment</h3>
                    
                    <form action="post_comment.php" method="post" id="commentform">
                    
                    <label for="comment_author" class="required">Your name</label>
                    <input type="text" name="comment_author" id="comment_author" value="" tabindex="1" required="required">
                    
                    <label for="email" class="required">Your email;</label>
                    <input type="email" name="email" id="email" value="" tabindex="2" required="required">
                    
                    <label for="comment" class="required">Your message</label>
                    <textarea name="comment" id="comment" rows="10" tabindex="4"  required="required"></textarea>
                    
                    <-- comment_post_ID value hard-coded as 1 -->
                    <input type="hidden" name="comment_post_ID" value="1" id="comment_post_ID" />
                    <input name="submit" type="submit" value="Submit comment" />
                    
                    </form>
                    
                    </div>'
                )
                ),
       
        tabItem(tabName ="topy",
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
      )
      #=========================================================================================================================================    
    }
    else {
      loginpage
    }
  })
  
  
  observeEvent(input$uz_add,{
    
    if(nrow(uzytkownicy_konta()) >= as.numeric(max_uz())){
      
      showNotification("Konto posiada maksymalną liczbę użytkowników", type = "err")
      
    }
    
    
  })
  
  
  #Dodawaniu uzytkownika
  observeEvent(input$uz_add_fin,{
    
    
    if(input$new_uz_name %in% uzytkownicy_konta()){
      
      showNotification("Użytkownik o podannej nazwie istnieje dla tego konta!", type = "err")
      
    }
    else{
      if(nrow(uzytkownicy_konta()) >= as.numeric(max_uz())){
        
        showNotification("Konto posiada maksymalną liczbę użytkowników", type = "err")
        
      }
      else{
        
        dbSendQuery(con, paste0("SELECT add_uz(", id_konta(), ", '", input$new_uz_name, "', ", input$if_baby_add, ");"))
        showNotification("Pomyślnie dodano użytkownika!", type = "message")
        isolate(uzytkownicy_konta)
        
      } 
    }
    
  })
  
  
  
  
  ### OUTPUTY ############
  
  
  
  
  output$tbl <- renderDataTable( plans_modal, options = list(lengthChange = FALSE, searching = FALSE, paging = FALSE))
  
  
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  
  
  ###### odtworzenia ###################
  
  output$odtworzenia_filmow <- DT::renderDataTable({
    
    odtworzenia <- dbGetQuery(con, paste0("SELECT * FROM odtworzenia_f_u(", input$uzytkownik, ");"))
    
    colnames(odtworzenia) <- c("Tytuł", "Moment zatrzymania")
    
    odtworzenia[["Oglądaj dalej"]] <-
      paste0(HTML('
          <div class="btn-group" role="group" aria-label="Basic example">
          <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(odtworzenia),'> <i class="fas fa-caret-right"></i> </button>
          <button type="button" class="btn btn-secondary delete" id=rate',1:nrow(odtworzenia), '><i class="far fa-thumbs-up"></i></button> 
          <button type="button" class="btn btn-secondary delete" id=comment',1:nrow(odtworzenia),'><i class="far fa-comment"></i> </button>
          </div>'))
    
    datatable(odtworzenia, options = list(width = 5, searching = FALSE, lengthChange = FALSE), escape = FALSE)
    
  })
  
  output$odtworzenia_seriali <- DT::renderDataTable({
    
    odtworzenia <- dbGetQuery(con, paste0("SELECT * FROM odtworzenia_s_u(", input$uzytkownik, ");"))
    
    colnames(odtworzenia) <- c("Tytuł", "Tytuł odcinka", "Nr odcinka", "Nr sezonu", "Moment zatrzymania")
    
    odtworzenia[["Oglądaj dalej"]] <-
      paste0(HTML('
          <div class="btn-group" role="group" aria-label="Basic example">
          <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(odtworzenia),'> <i class="fas fa-caret-right"></i> </button>
          <button type="button" class="btn btn-secondary delete" id=rate',1:nrow(odtworzenia), '><i class="far fa-thumbs-up"></i></button> 
          <button type="button" class="btn btn-secondary delete" id=comment',1:nrow(odtworzenia),'><i class="far fa-comment"></i> </button>
          </div>'))
    
    datatable(odtworzenia, options = list(width = 5, searching = FALSE, lengthChange = FALSE), escape = FALSE)
    
  })
  
  ######################################################
  
 shinyWidgetsGallery()
  
  
  
  ###### topy #######################
  
  ## top najlepiej oceniane filmy
  
  output$top_filmy  <-  DT::renderDataTable({
    
    if(input$kat_f == 0){
      top_f <- as.data.table(dbGetQuery(con, "SELECT * FROM top_filmow;"))
    }
    else{
      top_f <- as.data.table(dbGetQuery(con, paste0("SELECT * FROM top_f(", input$kat_f, ");")))
    }
    
    colnames(top_f) <- c("Tytuł", "Średnia")
    
    top_f[[HTML("Oglądaj/Oceń/ <br/> Skomentuj")]] <-
      paste0(HTML('
          <div class="btn-group" role="group" aria-label="Basic example">
          <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(top_f),'> <i class="fas fa-caret-right"></i> </button>
          <button type="button" class="btn btn-secondary delete" id=rate',1:nrow(top_f), '><i class="far fa-thumbs-up"></i></button> 
          <button type="button" class="btn btn-secondary delete" id=comment',1:nrow(top_f),'><i class="far fa-comment"></i> </button>
          </div>'))
    
    datatable(top_f, options = list(width = 5, lengthChange = FALSE, searching = FALSE), escape = FALSE)
  })
  
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
    
    colnames(top_s) <- c("Tytuł")

    top_s[[HTML("Oglądaj/Oceń/ <br/> Skomentuj")]] <-
      paste0(HTML('
          <div class="btn-group" role="group" aria-label="Basic example">
          <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(top_s),'> <i class="fas fa-caret-right"></i> </button>
          <button type="button" class="btn btn-secondary delete" id=rate',1:nrow(top_s), '><i class="far fa-thumbs-up"></i></button> 
          <button type="button" class="btn btn-secondary delete" id=comment',1:nrow(top_s),'><i class="far fa-comment"></i> </button>
          </div>'))

    
    datatable(top_s, options = list(width = 5, lengthChange = FALSE, searching = FALSE), escape = FALSE)
  })
  
  #######################################
  
} 