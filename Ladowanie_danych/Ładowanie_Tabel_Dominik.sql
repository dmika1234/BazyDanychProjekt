--Ładowanie_Tabel--

--produkcje--
\copy produkcje FROM 'D:/Studia/BazyDanych/Labki/Projekt/BazyDanychProjekt/Ladowanie_danych/produkcje.txt'  with (NULL '', HEADER 1, FORMAT CSV , DELIMITER ',', ENCODING UTF8)
----
--kategorie--
\copy kategorie FROM 'D:/Studia/BazyDanych/Labki/Projekt/BazyDanychProjekt/Ladowanie_danych/kategorie.txt'  with (NULL '', HEADER 1, FORMAT CSV , DELIMITER ',', ENCODING UTF8)
----
--w_kategorii--
\i 'D:/Studia/BazyDanych/Labki/Projekt/BazyDanychProjekt/Ladowanie_danych/w_kategorii.txt'
----
--plany--
\i 'D:/Studia/BazyDanych/Labki/Projekt/BazyDanychProjekt/Ladowanie_danych/plany.txt'
----
--konta--
\i 'D:/Studia/BazyDanych/Labki/Projekt/BazyDanychProjekt/Ladowanie_danych/konta.txt'
----
--uzytkownicy--
\i 'D:/Studia/BazyDanych/Labki/Projekt/BazyDanychProjekt/Ladowanie_danych/uzytkownicy.txt'
----
--platnosci--
\i 'D:/Studia/BazyDanych/Labki/Projekt/BazyDanychProjekt/Ladowanie_danych/platnosci.txt'
----7
--odcinki--
\i 'D:/Studia/BazyDanych/Labki/Projekt/BazyDanychProjekt/Ladowanie_danych/odcinki.txt'
----
--odtworzenia--
\i 'D:/Studia/BazyDanych/Labki/Projekt/BazyDanychProjekt/Ladowanie_danych/odtworzenia.txt'
----
--oceny--
\i 'D:/Studia/BazyDanych/Labki/Projekt/BazyDanychProjekt/Ladowanie_danych/oceny.txt'
----
--komentarze--
\i 'D:/Studia/BazyDanych/Labki/Projekt/BazyDanychProjekt/Ladowanie_danych/komentarze_pierwsze.txt'
\i 'D:/Studia/BazyDanych/Labki/Projekt/BazyDanychProjekt/Ladowanie_danych/komentarze_drugie.txt'
\i 'D:/Studia/BazyDanych/Labki/Projekt/BazyDanychProjekt/Ladowanie_danych/komentarze_trzecie.txt'
\i 'D:/Studia/BazyDanych/Labki/Projekt/BazyDanychProjekt/Ladowanie_danych/komentarze_czwarte.txt'
----


