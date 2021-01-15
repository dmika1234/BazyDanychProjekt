require("OpenRepGrid")
require(dplyr)
require(randomNames)
require("RPostgres")
require(data.table)





#Kraje======================================================================================================================================
kraje <- c("Stany Zjednoczone","Stany Zjednoczone","Stany Zjednoczone","Stany Zjednoczone","Stany Zjednoczone","Stany Zjednoczone",
           "Wielka Brytania","Wielka Brytania","Wielka Brytania","Wielka Brytania",
           "Hiszpania","Hiszpania", "Francja","Francja","Francja", "Wlochy","Wlochy",
           "Niemcy","Niemcy", "Dania", "Hong Kong", "Japonia", "Polska", "Szwecja", "Szwecja")


il_seriali <- 5610

kraje_produkcji <- sample(kraje, il_seriali, replace = TRUE)


write.table(kraje_produkcji, file = "kraje_produkcji.csv", quote = FALSE, row.names = TRUE, col.names = FALSE, sep = ",")
#======================================================================================================================================



#Odcinki======================================================================================================================================

sink(file = "odcinki.txt")

for(i in 1:(il_seriali)){
  
  il_sezon <- sample(1:12, 1, prob = 12:1/78)
  
  for(j in 1:il_sezon){
    
    il_odc <- sample(1:24, 1)
    
    for(k in 1:il_odc){
      
      czas_trwania <- paste(sample(0:2, 1, prob = c(0.4, 0.5, 0.1)), sample(0:59, 1), sample(0:59, 1), sep = ":")
      tytul <- paste0(as.character(randomWords(sample(2:4, 1))), collapse = " ")
      cat(paste0("INSERT INTO odcinki(id_odcinka, nr_sezonu, nr_odcinka, dlugosc_odcinka, tytul_odcinka) VALUES (", i+18726, ",",
                 j, ",", k, ",'", czas_trwania, "','",tytul, "');", "\n"))
    }
  }
}
sink()


#======================================================================================================================================



#Konta=====================================================================================================================================
email_platforms <- c("@gmail.com", "@gmail.com", "@gmail.com", "@gmail.com", "@gmail.com", "@onet.pl", "@wp.pl", "@o2.pl", "@yahoo.com")
znaki <- 1:61
znaki[1:26] <- letters
znaki[27:52] <- LETTERS
znaki[53:61] <- 1:9
znaki <- as.character(znaki)

##
sink(file = "konta.txt")

for(i in 1:1000){
  
  email <- paste(randomWords(1),randomWords(1), sample(email_platforms, 1), sep = "")
  il_haslo <- sample(8:16)
  haslo <- paste(sample(znaki, il_haslo, replace = TRUE), collapse = "")
  data <- sample(seq(as.Date('2013/01/01'), as.Date('2021/01/10'), by="day"), 1)
  id_planu <- sample(1:5, 1)
  
  cat(paste0("INSERT INTO konta(email, haslo, data_zalozenia, id_planu) VALUES ('", email, "','",
             haslo, "','", data, "','", id_planu, "');", "\n"))
  
}
sink()
##
#=====================================================================================================================================



con <- dbConnect(RPostgres::Postgres(), dbname = "projekt",
                 host = "localhost", port = 5432, 
                 user = "postgres", pass = "dell123987")





##losowanie czy_dla_dzieci do tabeli produkcje dla filmów

filmy.sql <- dbGetQuery(con, "SELECT * FROM produkcje WHERE czy_serial=FALSE;")
liczba_filmow <- nrow(filmy.sql)

czy_dla_dzieci <- sample(c(0,1), liczba_filmow, replace = TRUE, prob=c(0.2, 0.8))

View(czy_dla_dzieci)

write.table(czy_dla_dzieci, file = "czy_dla_dzieci.txt", quote = FALSE, col.names = FALSE)






##losowanie tabeli odcinki

seriale.sql <- dbGetQuery(con, "SELECT * FROM produkcje WHERE czy_serial=TRUE;")
liczba_seriali <- nrow(seriale.sql)


sink(file = "odcinki.txt")

for(i in 1:(liczba_seriali)){
  
  liczba_sezon <- sample(1:12, 1, prob = c(30, 20, 10:1), replace = TRUE)
  
  for(j in 1:liczba_sezon){
    
    liczba_odc <- sample(1:24, 1, prob = 24:1, replace = TRUE)
    
    for(k in 1:liczba_odc){
      
      czas_trwania <- paste(sample(0:2, 1, prob = c(0.4, 0.5, 0.1)), sample(0:59, 1, replace = TRUE), sample(0:59, 1, replace = TRUE), sep = ":")
      tytul <- paste0(as.character(randomWords(sample(2:4, 1, replace = TRUE))), collapse = " ")
      cat(paste0("INSERT INTO odcinki(id_produkcji, nr_sezonu, nr_odcinka, dlugosc_odcinka, tytul_odcinka) VALUES (", i+18725, ",",
                 j, ",", k, ",'", czas_trwania, "','",tytul, "');", "\n"))
    }
  }
}
sink()


##losowanie tabeli w_kategorii dla seriali


seriale.sql <- dbGetQuery(con, "SELECT * FROM produkcje WHERE czy_serial=TRUE;")
kategorie.sql <- dbGetQuery(con, "SELECT * FROM kategorie;")

liczba_seriali <- nrow(seriale.sql)
liczba_kategorii <- nrow(kategorie.sql)

sink(file = "w_kategorii_seriale.txt")



for(i in 1:(liczba_seriali)){
  
  a <- sample(1:liczba_kategorii, 3, replace = FALSE)
  
  cat(paste0("INSERT INTO w_kategorii(id_produkcji, id_kategorii) VALUES (", seriale.sql$id_produkcji[i], ",", a[1], "); \n"))
  cat(paste0("INSERT INTO w_kategorii(id_produkcji, id_kategorii) VALUES (", seriale.sql$id_produkcji[i], ",", a[2], "); \n"))
  cat(paste0("INSERT INTO w_kategorii(id_produkcji, id_kategorii) VALUES (", seriale.sql$id_produkcji[i], ",", a[3], "); \n"))
}

sink()






##losowanie użytkowników

konta.sql <- dbGetQuery(con, "SELECT * FROM konta JOIN plany USING(id_planu);")
# View(konta.sql)
liczba_kont <- nrow(konta.sql)

sink(file = "uzytkownicy.txt")

for(i in 1:liczba_kont){
  max_uz <- konta.sql$max_osob[i]
  liczba <- sample(1:max_uz, 1) #losujemy ilu użytkowników wylosować 
  
  nazwa <- paste(randomWords(liczba), sample(9, liczba), sep="")
  czy_dziecko <- c(FALSE, sample(c(TRUE, FALSE), liczba, replace = TRUE))
  #bo chcemy choć jednego dorosłego
  
  for(j in 1:liczba){
    
    cat(paste0("INSERT INTO uzytkownicy(id_konta, nazwa, czy_dziecko) VALUES (", konta.sql$id_konta[i] , ",'", nazwa[j] , "', ", czy_dziecko[j]  ,"); \n"))
  }
}
sink()


View(unique(randomWords(100)))







##tabela oceny

uzytkownicy.sql <- dbGetQuery(con, "SELECT * FROM uzytkownicy;")
produkcje.sql <- dbGetQuery(con, "SELECT * FROM produkcje;")



sink(file = "oceny.txt")

id_uz <- sample(uzytkownicy.sql$id_uzytkownika, 100000, replace = TRUE)
id_prod <- sample(produkcje.sql$id_produkcji, 100000, replace = TRUE)

a <- unique(cbind(id_uz, id_prod)) #dany użytkownik może ocenić daną produkcję tylko raz

# View(nrow(a))

ocena <- sample(1:10, nrow(a), replace = TRUE, prob = c(1:5, 5:1))

# View(ocena)

cat(paste0("INSERT INTO oceny(id_produkcji, id_uzytkownika, ocena) VALUES (", a[,2] , ",", a[,1] , ", ", ocena  ,"); \n"))

sink()  








##tabela komentarze

zdanie <- function(n) {
  x <- randomSentence(n)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  znak <- sample(c(".","!", "?" ), 1, prob = c(5, 4, 1))
  x <- paste(x, znak, sep="")
  return(x)
}


komentarz <- function(n) {
  k <- ""
  
  for (i in 1:n){
    dlugosc_zdania <- sample(3:8, 1)
    k <- paste(k, zdanie(dlugosc_zdania), sep=" ")
  }
  return(k)
}



produkcje.sql <- dbGetQuery(con, "SELECT * FROM produkcje;")


uzytkownicy.sql <- dbGetQuery(con, "SELECT u.id_uzytkownika, k.data_zalozenia
                                    FROM uzytkownicy u
                                    JOIN konta k USING(id_konta);")

odcinki_serialu.sql <- dbGetQuery(con, "SELECT id_odcinka, id_produkcji FROM odcinki JOIN produkcje USING(id_produkcji);")







#pierwsze komenatrze - te któe nie mają poprzednika
#nie wiem zbytnio jak to skrócić na razie

sink(file = "komentarze_pierwsze.txt")

for(i in 1:5000){
  
  ilosc_zdan <- sample(6, 1)
  tresc <- komentarz(ilosc_zdan)
  
  id_uz <- sample(uzytkownicy.sql$id_uzytkownika, 1)
  
  data_zalozenia <- uzytkownicy.sql[uzytkownicy.sql$id_uzytkownika==id_uz,]$data_zalozenia
  data <- sample(seq(data_zalozenia, as.Date('2021/01/10'), by = "day"), 1)
  
  id_prod <- sample(produkcje.sql$id_produkcji, 1)
  
  if(produkcje.sql[produkcje.sql$id_produkcji==id_prod,]$czy_serial){
    
    id_odcinka <- sample(odcinki_serialu.sql[odcinki_serialu.sql$id_produkcji == id_prod,]$id_odcinka, 1)
    
  }
  else{
    
    id_odcinka <- "NULL"
    
  }
  
  cat(paste0("INSERT INTO komentarze(tresc, id_uzytkownika, data, id_produkcji, id_odcinka) VALUES 
             ('", tresc , "',", id_uz , ", '", data  ,"',", id_prod, ",", id_odcinka, "); \n"))
  
}

sink()



##komenatarze odpowiadające



komentarze.sql <- dbGetQuery(con, "SELECT * FROM komentarze;")

sink(file="komentarze_drugie.txt")

for(i in 1:3000){
  
  id_pop_kom <- sample(komentarze.sql$id_komentarza, 1)
  
  
  ilosc_zdan <- sample(6, 1)
  tresc <- komentarz(ilosc_zdan)
  
  id_uz <- sample(uzytkownicy.sql$id_uzytkownika, 1)
  
  data_zalozenia <- uzytkownicy.sql[uzytkownicy.sql$id_uzytkownika==id_uz,]$data_zalozenia
  data_pop_kom <- komentarze.sql[komentarze.sql$id_komentarza==id_pop_kom,]$data
  d <- min(data_pop_kom, data_zalozenia)
  
  data <- sample(seq(as.Date(d), as.Date('2021/01/10'), by = "day"), 1)
  
  id_prod <- komentarze.sql[komentarze.sql$id_komentarza==id_pop_kom,]$id_produkcji
  
  id_odcinka <- komentarze.sql[komentarze.sql$id_komentarza==id_pop_kom,]$id_odcinka
  
  if(is.na(id_odcinka)){
    id_odcinka <- "NULL"
  }
  
  cat(paste0("INSERT INTO komentarze(id_pop_kom, tresc, id_uzytkownika, data, id_produkcji, id_odcinka) VALUES 
             (",id_pop_kom , ",'", tresc , "',", id_uz , ", '", data  ,"',", id_prod, ",", id_odcinka, "); \n"))
  
}
sink()


##komenatarze odpowiadające



komentarze.sql <- dbGetQuery(con, "SELECT * FROM komentarze;")

sink(file="komentarze_trzecie.txt")

for(i in 1:2000){
  
  id_pop_kom <- sample(komentarze.sql$id_komentarza, 1)
  
  
  ilosc_zdan <- sample(6, 1)
  tresc <- komentarz(ilosc_zdan)
  
  id_uz <- sample(uzytkownicy.sql$id_uzytkownika, 1)
  
  data_zalozenia <- uzytkownicy.sql[uzytkownicy.sql$id_uzytkownika==id_uz,]$data_zalozenia
  data_pop_kom <- komentarze.sql[komentarze.sql$id_komentarza==id_pop_kom,]$data
  d <- min(data_pop_kom, data_zalozenia)
  
  data <- sample(seq(as.Date(d), as.Date('2021/01/10'), by = "day"), 1)
  
  id_prod <- komentarze.sql[komentarze.sql$id_komentarza==id_pop_kom,]$id_produkcji
  
  id_odcinka <- komentarze.sql[komentarze.sql$id_komentarza==id_pop_kom,]$id_odcinka
  
  if(is.na(id_odcinka)){
    id_odcinka <- "NULL"
  }
  
  cat(paste0("INSERT INTO komentarze(id_pop_kom, tresc, id_uzytkownika, data, id_produkcji, id_odcinka) VALUES 
             (",id_pop_kom , ",'", tresc , "',", id_uz , ", '", data  ,"',", id_prod, ",", id_odcinka, "); \n"))
  
}
sink()

##komenatarze odpowiadające



komentarze.sql <- dbGetQuery(con, "SELECT * FROM komentarze;")

sink(file="komentarze_czwarte.txt")

for(i in 1:2000){
  
  id_pop_kom <- sample(komentarze.sql$id_komentarza, 1)
  
  
  ilosc_zdan <- sample(6, 1)
  tresc <- komentarz(ilosc_zdan)
  
  id_uz <- sample(uzytkownicy.sql$id_uzytkownika, 1)
  
  data_zalozenia <- uzytkownicy.sql[uzytkownicy.sql$id_uzytkownika==id_uz,]$data_zalozenia
  data_pop_kom <- komentarze.sql[komentarze.sql$id_komentarza==id_pop_kom,]$data
  d <- min(data_pop_kom, data_zalozenia)
  
  data <- sample(seq(as.Date(d), as.Date('2021/01/10'), by = "day"), 1)
  
  id_prod <- komentarze.sql[komentarze.sql$id_komentarza==id_pop_kom,]$id_produkcji
  
  id_odcinka <- komentarze.sql[komentarze.sql$id_komentarza==id_pop_kom,]$id_odcinka
  
  if(is.na(id_odcinka)){
    id_odcinka <- "NULL"
  }
  
  cat(paste0("INSERT INTO komentarze(id_pop_kom, tresc, id_uzytkownika, data, id_produkcji, id_odcinka) VALUES 
             (",id_pop_kom , ",'", tresc , "',", id_uz , ", '", data  ,"',", id_prod, ",", id_odcinka, "); \n"))
  
}
sink()
















## odtworzenia

id_uz.sql <- as.data.table(dbGetQuery(con, "SELECT id_uzytkownika FROM uzytkownicy;"))
liczba_uz <- nrow(id_uz.sql)

produkcje.sql <- as.data.table(dbGetQuery(con, "SELECT * FROM produkcje;"))
nrow(produkcje.sql)

odcinki_serialu.sql <- as.data.table(dbGetQuery(con, "SELECT id_odcinka, id_produkcji, dlugosc_odcinka FROM odcinki JOIN produkcje USING(id_produkcji);"))




start<-as.ITime("00:00:00")  

sink(file="odtworzenia.txt")

for (i in 1:liczba_uz){
  
  id_uz <- id_uz.sql[i, id_uzytkownika]
  
  liczba_odtworzen <- sample(1:20, 1)
  
  for(j in 1:liczba_odtworzen){
  id_prod <- sample(produkcje.sql$id_produkcji, 1)
  
  if(produkcje.sql[id_produkcji == id_prod, czy_serial]){
    
    id_odc <- sample(odcinki_serialu.sql[id_produkcji == id_prod, id_odcinka], 1)
    moment <- sample(seq((start), odcinki_serialu.sql[id_odcinka == id_odc ,dlugosc_odcinka]), 1)
    
  }
  else{
    
    moment <- sample(seq((start), produkcje.sql[id_produkcji == id_prod ,dlugosc_filmu]), 1)
    id_odc <- "NULL"
  }
  
  
  cat(paste0("INSERT INTO odtworzenia(id_uzytkownika, id_produkcji, moment_zatrzymania, id_odcinka) VALUES
             (", id_uz , ",", id_prod , ",'", moment , "', ", id_odc, "); \n"))
  
}
}
sink()






##losowanie płatności

konta.sql <- as.data.table(dbGetQuery(con, "SELECT * FROM konta JOIN plany USING(id_planu);"))
## pierwszy miesiąc nie płaci
liczba_kont <- nrow(konta.sql)
liczba_kont

View(sum(konta.sql$data_zalozenia==Inf))


library(lubridate)


sink(file = "platnosci.txt")

for (i in 1:liczba_kont){
  
  id_k <- konta.sql[i, id_konta]
  
  kwota <- konta.sql[i, cena]
  
  dzien <- sample(28, 1)
  
  data_zalozenia <- as.Date(konta.sql[i, data_zalozenia], "%Y-%m-%d")
  
  wektor_miesiecy <- seq.Date(data_zalozenia, as.Date('2021-01-10'), by="month")
  
  if(length(wektor_miesiecy) != 1){
    for(i in 2:length(wektor_miesiecy)){
      
      data <- wektor_miesiecy[i]
      
      day(data) <- dzien
      
      cat(paste0("INSERT INTO platnosci(id_konta, data, kwota) VALUES(", id_k, ",'", data, "',", kwota , "); \n"))
    }
  }
  
  
  
}

sink()


