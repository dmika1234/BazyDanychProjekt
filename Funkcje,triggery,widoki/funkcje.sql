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
