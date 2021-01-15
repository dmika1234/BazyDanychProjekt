--Ładowanie_Tabel--

--produkcje--
\copy produkcje FROM 'D:/Studia/BazyDanych/Labki/Projekt/INSERTY/produkcje.txt'  with (NULL '', HEADER 1, FORMAT CSV , DELIMITER ',', ENCODING UTF8)
----
--kategorie--
\copy kategorie FROM 'D:/Studia/BazyDanych/Labki/Projekt/INSERTY/kategorie.txt'  with (NULL '', HEADER 1, FORMAT CSV , DELIMITER ',', ENCODING UTF8)
----
--w_kategorii--
\copy w_kategorii FROM 'D:/Studia/BazyDanych/Labki/Projekt/INSERTY/w_kategorii.txt'  with (NULL '', HEADER 1, FORMAT CSV , DELIMITER ',', ENCODING UTF8)
----
--plany--
\i 'D:/Studia/BazyDanych/Labki/Projekt/INSERTY/plany.txt'
----
--konta--
\i 'D:/Studia/BazyDanych/Labki/Projekt/INSERTY/konta.txt'
----
--uzytkownicy--
\i 'D:/Studia/BazyDanych/Labki/Projekt/INSERTY/uzytkownicy.txt'
----
--platnosci--
\i 'D:/Studia/BazyDanych/Labki/Projekt/INSERTY/platnosci.txt'
----
--odcinki--
\i 'D:/Studia/BazyDanych/Labki/Projekt/INSERTY/odcinki.txt'
----
--odtworzenia--
\i 'D:/Studia/BazyDanych/Labki/Projekt/INSERTY/odtworzenia.txt'
----
--oceny--
\i 'D:/Studia/BazyDanych/Labki/Projekt/INSERTY/oceny.txt'
----
--komentarze--
\i 'D:/Studia/BazyDanych/Labki/Projekt/INSERTY/komentarze_pierwsze.txt'
\i 'D:/Studia/BazyDanych/Labki/Projekt/INSERTY/komentarze_drugie.txt'
\i 'D:/Studia/BazyDanych/Labki/Projekt/INSERTY/komentarze_trzecie.txt'
\i 'D:/Studia/BazyDanych/Labki/Projekt/INSERTY/komentarze_czwarte.txt'
----
