--Top produkcji--
DROP VIEW top_produkcji;
CREATE VIEW top_produkcji AS
SELECT p.tytul, round(avg(ocena), 2) AS "Srednia ocen"
FROM oceny o
JOIN produkcje p ON o.id_produkcji = p.id_produkcji
GROUP BY o.id_produkcji, p.tytul
HAVING COUNT(o.ocena)>10
ORDER BY avg(o.ocena) DESC
LIMIT 10;
--



--wgląd w produkcje--




--