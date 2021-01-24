--Top produkcji--
SELECT round(avg(ocena), 2), id_produkcji
FROM oceny
GROUP BY id_produkcji
HAVING COUNT(ocena)>10
ORDER BY avg(ocena) DESC
LIMIT 10;
--
--
SELECT id_produkcji, COUNT(ocena)
FROM oceny
GROUP BY id_produkcji
ORDER BY COUNT(ocena) DESC;
----

SELECT count(*)
FROM oceny;



SELECT email, COUNT(id_konta)
FROM konta
GROUP BY email
ORDER BY COUNT(id_konta) DESC;