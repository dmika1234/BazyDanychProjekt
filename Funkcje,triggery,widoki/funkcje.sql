--funkcja placenia

CREATE OR REPLACE FUNCTION zaplac(id_k INTEGER, kw DECIMAL(5,2)) RETURNS VOID AS $$
BEGIN
	INSERT INTO platnosci(id_konta, data, kwota) VALUES(id_k, CURRENT_DATE, kw);

END;
$$ LANGUAGE plpgsql;




--funkcja komentowania


CREATE OR REPLACE FUNCTION skomentuj_film(id_pop INTEGER, tr TEXT, id_u INTEGER, id_p INTEGER) RETURNS VOID AS $$
BEGIN

	INSERT INTO komentarze(id_pop_kom, tresc, id_uzytkownika, data, id_produkcji) VALUES(id_pop, tr, id_u, CURRENT_DATE, id_p);

END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION skomentuj_serial(id_pop INTEGER, tr TEXT, id_u INTEGER, id_o INTEGER) RETURNS VOID AS $$
DECLARE
	id_p INTEGER;
BEGIN

	SELECT id_produkcji INTO id_p FROM odcinki WHERE id_odcinka = id_o;
	INSERT INTO komentarze(id_pop_kom, tresc, id_uzytkownika, data, id_produkcji, id_odcinka) VALUES(id_pop, tr, id_u, CURRENT_DATE, id_p, id_o);
END;
$$ LANGUAGE plpgsql;




--funkcja odtwarzania
DROP FUNCTION odtworz_film;

CREATE OR REPLACE FUNCTION odtworz_film(id_u INTEGER, id_p INTEGER , mom TIME) RETURNS VOID AS $$	
BEGIN 
	INSERT INTO odtworzenia(id_uzytkownika, id_produkcji, moment_zatrzymania) VALUES(id_u, id_p, mom); 

END; 
$$ LANGUAGE plpgsql; 



CREATE OR REPLACE FUNCTION odtworz_serial(id_u INTEGER, id_o INTEGER, mom TIME) RETURNS VOID AS $$
DECLARE
	id_p INTEGER;

BEGIN
	SELECT id_produkcji INTO id_p FROM odcinki WHERE id_odcinka = id_o;
	INSERT INTO odtworzenia(id_uzytkownika, id_produkcji, moment_zatrzymania, id_odcinka) VALUES(id_u, id_p, mom, id_o);

END;
$$ LANGUAGE plpgsql;





--funkcja ile komentarzy ma dana produkcja

CREATE OR REPLACE FUNCTION komentarze_produkcji(id_prod INTEGER) RETURNS INTEGER AS $$
DECLARE
	liczba_kom INTEGER;
BEGIN
	SELECT count(*) INTO liczba_kom FROM komentarze WHERE id_produkcji = id_prod;
	RETURN liczba_kom;

END;
$$ LANGUAGE plpgsql;



--funkcja ile komentarzy ma dany odcinek

CREATE OR REPLACE FUNCTION komentarze_odcinka_serialu(id_o INTEGER) RETURNS INTEGER AS $$
DECLARE
	liczba_kom INTEGER;
BEGIN
	SELECT count(*) INTO liczba_kom FROM komentarze WHERE id_odcinka = id_o;
	RETURN liczba_kom;
END;
$$ LANGUAGE plpgsql;




--funkcja jaka jest średnia ocen dla danej produkcji

CREATE OR REPLACE FUNCTION srednia_ocen(id_prod INTEGER) RETURNS DECIMAL(3,2) AS $$
DECLARE
	srednia DECIMAL(3, 2);
BEGIN
	SELECT round(avg(ocena), 2) INTO srednia FROM oceny WHERE id_produkcji = id_prod;
	RETURN srednia;

END;
$$ LANGUAGE plpgsql;
							   
							   
							   
							   

							   
							   
--funkcje topu				
							   
CREATE OR REPLACE FUNCTION top(id_kat INTEGER) RETURNS TABLE(tytul VARCHAR(255), srednia DECIMAL(5, 2), id_p INTEGER) AS $$
BEGIN
	RETURN QUERY 
		SELECT p.tytul, round(avg(o.ocena), 2), p.id_produkcji
		FROM produkcje p
			JOIN oceny o ON p.id_produkcji = o.id_produkcji
			JOIN w_kategorii w ON w.id_produkcji = p.id_produkcji
		WHERE w.id_kategorii = id_kat
		GROUP BY p.tytul, p.id_produkcji
			HAVING COUNT(o.ocena)>10
		ORDER BY avg(o.ocena) DESC
		LIMIT 10;

END;
$$ LANGUAGE plpgsql;

SELECT * FROM top(2);
												 
												 
												 
												 
DROP FUNCTION top_f;												 
CREATE OR REPLACE FUNCTION top_f(id_kat INTEGER) RETURNS TABLE(tytul VARCHAR(255), srednia DECIMAL(5, 2), id_p INTEGER) AS $$  
BEGIN  
	RETURN QUERY   
		SELECT p.tytul, round(avg(o.ocena), 2), p.id_produkcji  
		FROM produkcje p  
			JOIN oceny o ON p.id_produkcji = o.id_produkcji  
			JOIN w_kategorii w ON w.id_produkcji = p.id_produkcji  
		WHERE w.id_kategorii = id_kat AND p.czy_serial = FALSE  
		GROUP BY p.tytul, p.id_produkcji  
			HAVING COUNT(o.ocena)>10  
		ORDER BY avg(o.ocena) DESC  
		LIMIT 50;  
		
END;  
$$ LANGUAGE plpgsql; 

SELECT * FROM top(2);
			
												   
												   
												   
												   
												   
												   
												   
DROP FUNCTION top_s;												   
CREATE OR REPLACE FUNCTION top_s(id_kat INTEGER) RETURNS TABLE(tytul VARCHAR(255), srednia DECIMAL(5, 2), id_p INTEGER) AS $$ 
BEGIN 
	RETURN QUERY  
		SELECT p.tytul, round(avg(o.ocena), 2), p.id_produkcji 
		FROM produkcje p 
			JOIN oceny o ON p.id_produkcji = o.id_produkcji 
			JOIN w_kategorii w ON w.id_produkcji = p.id_produkcji 
		WHERE w.id_kategorii = id_kat AND p.czy_serial = TRUE 
		GROUP BY p.tytul, p.id_produkcji 
			HAVING COUNT(o.ocena)>10 
		ORDER BY avg(o.ocena) DESC 
		LIMIT 50; 
		

END; 
$$ LANGUAGE plpgsql; 
												 
												 
												 
												



--Tworzenie konta--
DROP FUNCTION utworz_konto;

CREATE OR REPLACE FUNCTION utworz_konto(em VARCHAR(255), ha VARCHAR(255), nazwa_pl VARCHAR(20)) RETURNS VOID AS $$
DECLARE
id_p INTEGER;
id_k INTEGER;
BEGIN
	SELECT id_planu INTO id_p FROM plany WHERE nazwa_planu = nazwa_pl;
	SELECT max(id_konta)+1 INTO id_k FROM konta;
	INSERT INTO konta (id_konta, email, haslo, data_zalozenia, id_planu) VALUES (id_k, em, ha, CURRENT_DATE, id_p);
	INSERT INTO uzytkownicy (id_konta, nazwa, czy_dziecko) VALUES (id_k, 'kot1', FALSE);

END;
$$ LANGUAGE plpgsql;
											   
											   
											   
											   
											   
											   
											   
											   
											   
											   
											   
											   
											   
											   
											   
											   
											   
											   
---funkcja wypisująca odtworzenia dla konkretnego użytkownika

--filmow
DROP FUNCTION odtworzenia_f_u;

CREATE OR REPLACE FUNCTION odtworzenia_f_u(id_u INTEGER) RETURNS TABLE(tytul VARCHAR(255), moment_zatrzymania TIME, id_p INTEGER) AS $$
BEGIN
	RETURN QUERY 
	SELECT p.tytul, o.moment_zatrzymania, o.id_produkcji
	FROM (SELECT o.id_produkcji, max(o.id_odtworzenia)
			FROM odtworzenia o
				JOIN produkcje p ON p.id_produkcji = o.id_produkcji
			WHERE o.id_uzytkownika = id_u
				AND p.czy_serial = FALSE
			GROUP BY o.id_produkcji) AS pod
		JOIN odtworzenia o ON o.id_odtworzenia = pod.max
		JOIN produkcje p ON p.id_produkcji = o.id_produkcji
		WHERE o.moment_zatrzymania < p.dlugosc_filmu
		ORDER BY p.id_produkcji;

END;
$$ LANGUAGE plpgsql;	


--seriali
DROP FUNCTION odtworzenia_s_u; 

CREATE OR REPLACE FUNCTION odtworzenia_s_u(id_u INTEGER) RETURNS TABLE(tytul VARCHAR(255), tytul_odcinka VARCHAR(255), nr_odcinka INTEGER, nr_sezonu INTEGER, moment_zatrzymania TIME, id_o INTEGER, id_p INTEGER) AS $$ 
BEGIN 
	RETURN QUERY  
	SELECT p.tytul, od.tytul_odcinka, od.nr_odcinka, od.nr_sezonu, o.moment_zatrzymania, o.id_odcinka, o.id_produkcji  	
	FROM (SELECT o.id_odcinka, max(o.id_odtworzenia)  
			FROM odtworzenia o 
				JOIN produkcje p ON p.id_produkcji = o.id_produkcji 
			WHERE o.id_uzytkownika = id_u 
				AND p.czy_serial = TRUE 
			GROUP BY o.id_odcinka) AS pod 
		JOIN odtworzenia o ON o.id_odtworzenia = pod.max 
		JOIN produkcje p ON p.id_produkcji = o.id_produkcji 
		JOIN odcinki od ON od.id_odcinka = o. id_odcinka  	
		WHERE o.moment_zatrzymania < od.dlugosc_odcinka 
		ORDER BY p.id_produkcji; 

END; 
$$ LANGUAGE plpgsql; 



--wszystkie
DROP FUNCTION odtworzenia_u;

CREATE OR REPLACE FUNCTION odtworzenia_u(id_u INTEGER) RETURNS TABLE(tytul VARCHAR(255)) AS $$
BEGIN
	RETURN QUERY 
	
	SELECT pod.tytul FROM
		(SELECT o.id_odtworzenia as id, p.tytul as tytul
		FROM odtworzenia o
			JOIN produkcje p ON p.id_produkcji = o.id_produkcji
			JOIN odcinki od ON od.id_odcinka = o. id_odcinka
		WHERE o.id_uzytkownika = id_u 
			AND o.moment_zatrzymania < od.dlugosc_odcinka
			AND p.czy_serial=TRUE
		
		UNION
		
		SELECT o.id_odtworzenia as id, p.tytul as tytul
		FROM odtworzenia o
			JOIN produkcje p ON p.id_produkcji = o.id_produkcji
		WHERE o.id_uzytkownika = id_u 
			AND o.moment_zatrzymania < p.dlugosc_filmu
			AND p.czy_serial=FALSE) AS pod
			
	ORDER BY pod.id;

END;
$$ LANGUAGE plpgsql;



--Dodanie użytkownika--
DROP FUNCTION add_uz;

CREATE OR REPLACE FUNCTION add_uz(id_k INTEGER, nazwa VARCHAR(255), czy_dziecko1 BOOLEAN) RETURNS VOID AS $$ 
BEGIN 

	INSERT INTO uzytkownicy (id_konta, nazwa, czy_dziecko) VALUES (id_k, nazwa, czy_dziecko1); 

END; 
$$ LANGUAGE plpgsql; 
----

										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
										   
-----top najczęściej oglądanych

--filmów

 DROP VIEW top_o_filmow; 
CREATE VIEW top_o_filmow AS   
SELECT p.tytul, p.id_produkcji AS id_p 
FROM produkcje p  
	JOIN odtworzenia o ON o.id_produkcji = p.id_produkcji  
WHERE p.czy_serial = FALSE  
GROUP BY p.tytul, p.id_produkcji 
ORDER BY count(o.id_odtworzenia) DESC 
LIMIT 50;  



--sortowanie po kategoriach
DROP FUNCTION top_o_f;

CREATE OR REPLACE FUNCTION top_o_f(id_kat INTEGER) RETURNS TABLE(tytul VARCHAR(255), id_p INTEGER) AS $$   
BEGIN   
	RETURN QUERY    
		SELECT p.tytul, p.id_produkcji  
		FROM produkcje p   
			JOIN odtworzenia o ON o.id_produkcji = p.id_produkcji    
			JOIN w_kategorii w ON w.id_produkcji = p.id_produkcji   
		WHERE w.id_kategorii = id_kat AND p.czy_serial = FALSE   
		GROUP BY p.tytul, p.id_produkcji  
		ORDER BY count(o.id_odtworzenia) DESC  
		LIMIT 50;    

END;   
$$ LANGUAGE plpgsql;    




--seriali

DROP VIEW top_o_seriali;   
CREATE VIEW top_o_seriali AS   
SELECT p.tytul, p.id_produkcji  AS id_p 
FROM produkcje p    
	JOIN odtworzenia o ON o.id_produkcji = p.id_produkcji   
WHERE p.czy_serial = TRUE   
GROUP BY p.tytul, p.id_produkcji  
ORDER BY count(o.id_odtworzenia) DESC  
LIMIT 50;   


DROP FUNCTION top_o_s;
CREATE OR REPLACE FUNCTION top_o_s(id_kat INTEGER) RETURNS TABLE(tytul VARCHAR(255), id_p INTEGER) AS $$   
BEGIN   
	RETURN QUERY    
		SELECT p.tytul, p.id_produkcji  
		FROM produkcje p   
			JOIN odtworzenia o ON o.id_produkcji = p.id_produkcji   
			JOIN w_kategorii w ON w.id_produkcji = p.id_produkcji   
		WHERE w.id_kategorii = id_kat AND p.czy_serial = TRUE   
		GROUP BY p.tytul, p.id_produkcji  
		ORDER BY count(o.id_odtworzenia) DESC  
		LIMIT 50;     

END;   
$$ LANGUAGE plpgsql;   
									       
									       
									       
									       
									       
									       
									       
									       
									       
									       
									       
									       
									       
									       
--platnosci dla konkretnego konta
CREATE OR REPLACE FUNCTION plat_konta(id_k INTEGER) RETURNS TABLE(data_p DATE, kw DECIMAL(5, 2)) AS $$ 
BEGIN 
	RETURN QUERY  
		SELECT p.data, p.kwota 
		FROM platnosci p 
		WHERE p.id_konta = id_k 
		ORDER BY p.data DESC; 

END; 
$$ LANGUAGE plpgsql;  
											  
											  
											  
											  
											  
											  
											  
											  
											  
											  
											  
											  
											  
											  
											  
											  
											  
											  
-- funkcja podająco id następnego odcinka

DROP FUNCTION id_kolejnego_odcinka;

CREATE OR REPLACE FUNCTION id_kolejnego_odcinka(id_o INTEGER) RETURNS INTEGER AS $$
DECLARE
ten_odcinek RECORD;
kolejny_odcinek RECORD;
BEGIN
	SELECT * INTO ten_odcinek 
	FROM odcinki 
	WHERE id_odcinka = id_o;
	
	SELECT * INTO kolejny_odcinek 
	FROM odcinki 
	WHERE id_produkcji = ten_odcinek.id_produkcji 
		AND nr_sezonu = ten_odcinek.nr_sezonu
		AND nr_odcinka = ten_odcinek.nr_odcinka + 1;
		
	IF(FOUND) THEN
		RETURN kolejny_odcinek.id_odcinka;
	ELSE
		SELECT * INTO kolejny_odcinek
		FROM odcinki
		WHERE id_produkcji = ten_odcinek.id_produkcji 
			AND nr_sezonu = ten_odcinek.nr_sezonu + 1
			AND nr_odcinka = 1;
			
		IF(FOUND) THEN
			RETURN kolejny_odcinek.id_odcinka;
		ELSE 
			RETURN 0;
		END IF;
	END IF;
	
	

END;
$$ LANGUAGE plpgsql;

