DROP TABLE konta CASCADE;
CREATE TABLE konta (
	id_konta SERIAL,
	email VARCHAR(50) NOT NULL,
	haslo VARCHAR(50) NOT NULL,
	data_zalozenia DATE NOT NULL,
	id_planu INTEGER NOT NULL
);



DROP TABLE plany CASCADE;
CREATE TABLE plany (
	id_planu SERIAL,
	nazwa_plau VARCHAR(20) NOT NULL,
	max_osob INTEGER NOT NULL,
	cena DECIMAL(5,2) NOT NULL
);




DROP TABLE uzytkownicy CASCADE;
CREATE TABLE uzytkownicy (
	id_uzytkownika SERIAL,
	id_konta INTEGER NOT NULL,
	nazwa VARCHAR(20) NOT NULL,
	czy_dziecko BOOLEAN NOT NULL DEFAULT '0'
);




DROP TABLE platnosci CASCADE;
CREATE TABLE platnosci (
	id_platnosci SERIAL,
	id_konta INTEGER NOT NULL,
	data DATE NOT NULL,
	kwota DECIMAL(5,2) NOT NULL
);




DROP TABLE produkcje CASCADE;
CREATE TABLE produkcje (
	id_produkcji SERIAL,
	tytul VARCHAR(255) NOT NULL,
	rezyser VARCHAR(255),
	kraj VARCHAR(50),
	czy_serial BOOLEAN NOT NULL,
	dlugosc_filmu TIME,
	rok_produkcji INTEGER,
	czy_dla_dzieci BOOLEAN NOT NULL DEFAULT '0'
);




DROP TABLE kategorie CASCADE;
CREATE TABLE kategorie (
	id_kategorii SERIAL,
	nazwa_kategorii VARCHAR(20) NOT NULL UNIQUE
);




DROP TABLE odcinki CASCADE;
CREATE TABLE odcinki (
	id_odcinka SERIAL,
	id_produkcji INTEGER NOT NULL,
	nr_sezonu INTEGER NOT NULL,
	nr_odcinka INTEGER NOT NULL,
	dlugosc_odcinka TIME NOT NULL,
	tytul_odcinka VARCHAR(255) NOT NULL
);



DROP TABLE odtworzenia CASCADE;
CREATE TABLE odtworzenia (
	id_odtworzenia SERIAL PRIMARY KEY,
	id_uzytkownika INTEGER NOT NULL,
	id_produkcji INTEGER NOT NULL,
	moment_zatrzymania TIME NOT NULL,
	id_odcinka INTEGER
);






DROP TABLE w_kategorii CASCADE;
CREATE TABLE w_kategorii (
	id_kategorii INTEGER NOT NULL,
	id_produkcji INTEGER NOT NULL	
);






DROP TABLE oceny CASCADE;
CREATE TABLE oceny (
	id_produkcji INTEGER NOT NULL,
	id_uzytkownika INTEGER NOT NULL,
	ocena INTEGER 
);



DROP TABLE komentarze CASCADE;
CREATE TABLE komentarze (
	id_komentarza SERIAL,
	id_pop_kom INTEGER,
	tresc TEXT NOT NULL,
	id_uzytkownika INTEGER NOT NULL,
	data DATE NOT NULL,
	id_produkcji INTEGER NOT NULL,
	id_odcinka INTEGER 
);
















