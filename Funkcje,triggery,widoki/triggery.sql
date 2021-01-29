
--Sprawdzenie czy liczba uzytkowników na koncie się zgadza--
CREATE OR REPLACE FUNCTION max_check() RETURNS TRIGGER AS $$
DECLARE
	liczba_uz INTEGER;
	max_uz INTEGER;
BEGIN
	SELECT count(*) INTO liczba_uz FROM uzytkownicy u WHERE id_konta = NEW.id_konta; 
	
	SELECT p.max_osob INTO max_uz FROM uzytkownicy u 
		JOIN konta k ON u.id_konta = k.id_konta 
			JOIN plany p ON k.id_planu = p.id_planu 
		WHERE u.id_konta = NEW.id_konta;
	
	
	IF(liczba_uz >= max_uz) THEN
		RAISE EXCEPTION ’Liczba uzytkownikow nie moze przekroczyc maksymalnej!’;
	ELSE
		RETURN NEW;
	END IF;
						
END;
$$ LANGUAGE plpgsql;

----
DROP TRIGGER tr_max_check ON uzytkownicy;
CREATE TRIGGER tr_max_check BEFORE INSERT OR UPDATE ON uzytkownicy FOR EACH ROW EXECUTE PROCEDURE max_check();
----
--Sprawdzenie
INSERT INTO uzytkownicy (id_konta, nazwa, czy_dziecko) VALUES (6, 'maro', FALSE);
DELETE FROM uzytkownicy WHERE id_uzytkownika = ;	
----



--Sprawdzenie czy data zapłaty jest po dacie założenia konta--
CREATE OR REPLACE FUNCTION check_pay_date() RETURNS TRIGGER AS $$
DECLARE
	data_zal DATE;

BEGIN

	SELECT data_zalozenia INTO data_zal FROM konta WHERE id_konta = NEW.id_konta;
	
	IF(data_zal > NEW.data) THEN
		RAISE EXCEPTION 'Data platnosci nie zgadza sie z data zalozenia konta!';
	ELSE
		RETURN NEW;
	END IF;
END;
$$ LANGUAGE plpgsql;
----
DROP TRIGGER tr_check_pay_date ON platnosci;
CREATE TRIGGER tr_check_pay_date BEFORE INSERT OR UPDATE ON platnosci FOR EACH ROW EXECUTE PROCEDURE check_pay_date();
----
--Sprawdzenie--
SELECT data_zalozenia FROM konta WHERE id_konta = 6;
INSERT INTO platnosci (id_konta, data, kwota) VALUES (6, '2012-05-07', 60);
----







--Sprawdzenie czy moment zatrzymania zawiera sie w czasie trwania produkcji--
CREATE OR REPLACE FUNCTION check_stop_time() RETURNS TRIGGER AS $$
DECLARE
	dlugosc_prod TIME;
	co_to INTEGER;
	dlugosc_odc TIME;
BEGIN
	
	SELECT dlugosc_filmu INTO dlugosc_prod FROM produkcje WHERE id_produkcji = NEW.id_produkcji;
	co_to = NEW.id_odcinka;
	SELECT dlugosc_odcinka INTO dlugosc_odc FROM odcinki WHERE id_odcinka = NEW.id_odcinka;
	
	IF(co_to IS NULL) THEN
		IF(NEW.moment_zatrzymania > dlugosc_prod) THEN 
			RAISE EXCEPTION 'Moment zatrzymania nie zgadza sie z czasem trwania filmu!';
		ELSE
			RETURN NEW;
		END IF;
	ELSE
		IF(NEW.moment_zatrzymania > dlugosc_odc) THEN
			RAISE EXCEPTION 'Moment zatrzymania nie zgadza sie z czasem trwania odcinka!';
		ELSE
			RETURN NEW;
		END IF;
	END IF;
END;
$$ LANGUAGE plpgsql;

----
DROP TRIGGER tr_check_stop_time ON odtworzenia;
CREATE TRIGGER tr_check_stop_time BEFORE INSERT OR UPDATE ON odtworzenia FOR EACH ROW EXECUTE PROCEDURE check_stop_time();
----
--Sprawdzenie--
INSERT INTO odtworzenia (id_uzytkownika, id_produkcji, moment_zatrzymania, id_odcinka) VALUES (2, 18726, '01:10:52', 1);
DELETE FROM odtworzenia WHERE id_odtworzenia =  20973;
INSERT INTO odtworzenia (id_uzytkownika, id_produkcji, moment_zatrzymania, id_odcinka) VALUES (2, 2, '01:10:52', NULL);
----	






CREATE OR REPLACE FUNCTION () RETURNS TRIGGER AS $$
DECLARE
	

BEGIN

END;
$$ LANGUAGE plpgsql;

DROP TRIGGER tr ON platnosci;
CREATE TRIGGER tr BEFORE INSERT OR UPDATE ON  FOR EACH ROW EXECUTE PROCEDURE ();




--Sprawdzenie czy mail jest faktycznie mailem--
CREATE OR REPLACE FUNCTION czy_mail() RETURNS TRIGGER AS $$

BEGIN 

	IF NEW.email !~* '^[A-Za-z0-9._%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$' THEN
		RAISE EXCEPTION 'Podany email nie istnieje!';
	ELSE
		RETURN NEW;
	END IF;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER tr_czy_mail ON konta;
CREATE TRIGGER tr_czy_mail BEFORE INSERT OR UPDATE ON konta FOR EACH ROW EXECUTE PROCEDURE czy_mail();
--Sprawdzenie--
INSERT INTO konta (email, haslo, data_zalozenia, id_planu) VALUES ('hisdepicts@onet.pl', '1234', CURRENT_DATE, 2);
---

--Sprawdzenie czy podany mail nie jest już w bazie--
CREATE OR REPLACE FUNCTION mail_check() RETURNS TRIGGER AS $$

BEGIN
	
	IF EXISTS(SELECT email FROM konta WHERE email = NEW.email) THEN
		RAISE EXCEPTION 'Konto o podanym emailu istnieje!';
	ELSE
		RETURN NEW;
	END IF;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER tr_mail_check ON konta;
CREATE TRIGGER tr_mail_check BEFORE INSERT OR UPDATE ON konta FOR EACH ROW EXECUTE PROCEDURE mail_check();
--Sprawdzenie--
INSERT INTO konta (email, haslo, data_zalozenia, id_planu) VALUES ('asd', '1234', CURRENT_DATE, 2);
---








--trigger sprawdzający, że data utworzenia komentarza jest późniejsza niż data założenia konta

CREATE OR REPLACE FUNCTION com_date_check() RETURNS TRIGGER AS $$
DECLARE
	data_zal DATE;

BEGIN
	SELECT data_zalozenia INTO data_zal 
	FROM konta k
	JOIN uzytkownicy u ON u.id_konta = k.id_konta
	WHERE u.id_uzytkownika = NEW.id_uzytkownika;
	
	IF(data_zal > NEW.data) THEN
		RAISE EXCEPTION 'Data dodania komentarza jest pozniejsza niz data zlozenia konta!';
	ELSE
		RETURN NEW;
	END IF;	

END;
$$ LANGUAGE plpgsql;

DROP TRIGGER tr_com_date_check ON komentarze;
CREATE TRIGGER tr_com_date_check BEFORE INSERT OR UPDATE ON komentarze FOR EACH ROW EXECUTE PROCEDURE com_date_check();


INSERT INTO komentarze(tresc, id_uzytkownika, data, id_produkcji ) VALUES('aaaa', 1, '2019-11-25', 1);
UPDATE komentarze SET data = '2018-01-01' WHERE id_komentarza = 12005;




--trigger sprawdzający, że komentujemy odcinek a nie produkcję


CREATE OR REPLACE FUNCTION com_check() RETURNS TRIGGER AS $$
DECLARE
	id_p INTEGER;

BEGIN
	IF(NEW.id_odcinka IS NULL) THEN
		RETURN NEW;
	
	ELSE
		SELECT id_produkcji INTO id_p 
		FROM odcinki 
		WHERE id_odcinka = NEW.id_odcinka;
		
		IF(NEW.id_produkcji = id_p) THEN
			RETURN NEW;
		ELSE
			RAISE EXCEPTION 'Zle id_produkcji';
		END IF;
	END IF;

END;
$$ LANGUAGE plpgsql;

DROP TRIGGER tr_com_check ON komentarze;
CREATE TRIGGER tr_com_check BEFORE INSERT OR UPDATE ON komentarze FOR EACH ROW EXECUTE PROCEDURE com_check();




CREATE OR REPLACE FUNCTION odcinki_check() RETURNS TRIGGER AS $$
DECLARE
	krotka RECORD;
BEGIN
	SELECT * INTO krotka FROM odcinki 
	WHERE id_produkcji = NEW.id_produkcji 
		AND nr_sezonu = NEW.nr_sezonu
		AND nr_odcinka = NEW.nr_odcinka;
		
	IF(FOUND) THEN RAISE EXCEPTION 'Istnieje już taki odcinek';
	ELSE
	RETURN NEW;
	END IF;

END;
$$ LANGUAGE plpgsql;

DROP TRIGGER odcinki_check ON odcinki;
CREATE TRIGGER odcinki_check BEFORE INSERT OR UPDATE ON odcinki FOR EACH ROW EXECUTE PROCEDURE odcinki_check();
