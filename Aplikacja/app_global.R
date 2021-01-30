

#Panel logowania==============================================================================================================================
loginpage <- tags$div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;", 
                      fluidRow(
                        
                        #LogowanieUI==
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
                                  tags$p("Ups! BĹ‚Ä™dny login lub hasĹ‚o!",
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
                            
                            )
                         )
                        #=============
                        )
)

#Zmienne Globalne==============================================================================================================================
con <- dbConnect(RPostgres::Postgres(), dbname = "projekt",
                 host = "localhost", port = 5432, 
                 user = "postgres", pass = "dell123987")



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

#============================================================
credentials = data.table(
  username_id = users,
  passod   = passwords, 
  stringsAsFactors = F)
#=============================================================

source("ui.R")
source("server.R")





runApp(list(ui = ui, server = server))
