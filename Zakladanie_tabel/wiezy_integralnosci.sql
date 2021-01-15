--Klucze główne--
ALTER TABLE konta ADD PRIMARY KEY (id_konta);
ALTER TABLE plany ADD PRIMARY KEY (id_planu);
ALTER TABLE uzytkownicy ADD PRIMARY KEY (id_uzytkownika);
ALTER TABLE platnosci ADD PRIMARY KEY (id_platnosci);
ALTER TABLE produkcje ADD PRIMARY KEY (id_produkcji);
ALTER TABLE kategorie ADD PRIMARY KEY (id_kategorii);
ALTER TABLE odtworzenia ADD PRIMARY KEY (id_odtworzenia);
ALTER TABLE odcinki ADD PRIMARY KEY (id_odcinka);
ALTER TABLE oceny ADD PRIMARY KEY (id_produkcji,id_uzytkownika);
ALTER TABLE komentarze ADD PRIMARY KEY (id_komentarza);
ALTER TABLE w_kategorii ADD PRIMARY KEY(id_kategorii, id_produkcji);
----
--Klucze obce--
ALTER TABLE konta ADD FOREIGN KEY (id_planu) REFERENCES plany(id_planu) ON UPDATE CASCADE ON DELETE RESTRICT;

ALTER TABLE uzytkownicy ADD FOREIGN KEY (id_konta) REFERENCES konta(id_konta) ON UPDATE CASCADE ON DELETE RESTRICT;

ALTER TABLE platnosci ADD FOREIGN KEY (id_konta)  REFERENCES konta(id_konta);

ALTER TABLE produkcje ADD CHECK (rok_produkcji BETWEEN 1800 AND 2100);

ALTER TABLE odcinki ADD FOREIGN KEY (id_produkcji) REFERENCES produkcje(id_produkcji) ON UPDATE CASCADE ON DELETE RESTRICT;

ALTER TABLE odtworzenia ADD FOREIGN KEY (id_uzytkownika) REFERENCES uzytkownicy(id_uzytkownika) ON UPDATE CASCADE ON DELETE RESTRICT;
ALTER TABLE odtworzenia ADD FOREIGN KEY (id_produkcji) REFERENCES produkcje(id_produkcji) ON UPDATE CASCADE ON DELETE RESTRICT;
ALTER TABLE odtworzenia ADD FOREIGN KEY (id_odcinka) REFERENCES odcinki(id_odcinka) ON UPDATE CASCADE ON DELETE RESTRICT;

ALTER TABLE w_kategorii ADD FOREIGN KEY (id_kategorii) REFERENCES kategorie(id_kategorii) ON UPDATE CASCADE ON DELETE RESTRICT;
ALTER TABLE w_kategorii ADD FOREIGN KEY (id_produkcji) REFERENCES produkcje(id_produkcji) ON UPDATE CASCADE ON DELETE RESTRICT;


ALTER TABLE oceny ADD FOREIGN KEY (id_produkcji) REFERENCES produkcje(id_produkcji) ON UPDATE CASCADE ON DELETE RESTRICT;
ALTER TABLE oceny ADD FOREIGN KEY (id_uzytkownika) REFERENCES uzytkownicy(id_uzytkownika) ON UPDATE CASCADE ON DELETE RESTRICT;
ALTER TABLE oceny ADD CHECK (ocena BETWEEN 1 AND 10);


ALTER TABLE komentarze ADD FOREIGN KEY (id_pop_kom) REFERENCES komentarze(id_komentarza) ON UPDATE CASCADE ON DELETE RESTRICT;
ALTER TABLE komentarze ADD FOREIGN KEY (id_uzytkownika) REFERENCES uzytkownicy(id_uzytkownika) ON UPDATE CASCADE ON DELETE RESTRICT;
ALTER TABLE komentarze ADD FOREIGN KEY (id_produkcji) REFERENCES produkcje(id_produkcji) ON UPDATE CASCADE ON DELETE RESTRICT;
----
