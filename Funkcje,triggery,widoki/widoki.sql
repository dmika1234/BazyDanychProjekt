--Top produkcji--
DROP VIEW top_produkcji;
CREATE VIEW top_produkcji AS
SELECT p.tytul AS "Tytul", round(avg(ocena), 2) AS "Srednia ocen"
FROM oceny o
	JOIN produkcje p ON o.id_produkcji = p.id_produkcji
GROUP BY o.id_produkcji, p.tytul
	HAVING COUNT(o.ocena)>10
ORDER BY avg(o.ocena) DESC
LIMIT 10;
--



--wgląd w filmy--
DROP VIEW info_filmy;
CREATE VIEW info_filmy AS
SELECT p.tytul AS "Tytul", p.rezyser AS Rezyser, p.kraj AS kraj, p.dlugosc_filmu AS Dlugosc, p.rok_produkcji AS "Rok produkcji", k.nazwa_kategorii AS Kategoria
FROM produkcje p
	JOIN w_kategorii w ON p.id_produkcji = w.id_produkcji
	JOIN  kategorie k ON w.id_kategorii = k.id_kategorii
WHERE czy_serial = FALSE;
--

--wgląd w odcinki--
DROP VIEW info_odcinki;
CREATE VIEW info_odcinki AS 
SELECT p.tytul AS "Tytul serialu", o.tytul_odcinka AS "Tytul odcinka", o.nr_sezonu AS "Numer sezonu", o.nr_odcinka AS "Numer odcinka",
 p.rezyser AS Rezyser, p.kraj AS Kraj, o.dlugosc_odcinka AS "Dlugosc odcinka", p.rok_produkcji AS "Rok produkcji", k.nazwa_kategorii AS Kategoria
FROM produkcje p
	JOIN w_kategorii w ON p.id_produkcji = w.id_produkcji
	JOIN  kategorie k ON w.id_kategorii = k.id_kategorii
	JOIN odcinki o ON p.id_produkcji = o.id_produkcji
WHERE czy_serial = TRUE;
--


--komentarze produkcji--??
DROP VIEW kom_prod;
CREATE VIEW kom_prod AS
SELECT p.tytul AS "Tytul produkcji", o.tytul_odcinka AS "Tytul odcinka", k.tresc "Tresc komentarza"
FROM komentarze k
	JOIN produkcje p ON k.id_produkcji = p.id_produkcji
	JOIN odcinki o ON k.id_odcinka = o.id_odcinka;
----




