--All years

SELECT m.Main_ID,
       m.eGiftID,
       m.FirstName,
       m.LastName,
       m.CompanyName,
       m.Title,
       m.Phone_Cell,
       m.Phone_Home,
       m.EMail,
       SUM(h.Raised) AS TotalRaised,
       COUNT(DISTINCT CASE WHEN h.Participant = 1  THEN h.EventYear END) AS NumYearsRiding
FROM tblMain AS m
LEFT JOIN tblHistory AS h ON m.Main_ID = h.Main_ID
WHERE m.Title IS NOT NULL 
	AND  (
	Title in ('COO', 'CFO', 'CEO') or 
	Title LIKE '%chief executive officer%'
    OR Title LIKE '%chief financial officer%'
    OR Title LIKE '%chief operating officer%')

GROUP BY m.Main_ID, m.eGiftID, m.FirstName, m.LastName, m.CompanyName, m.Title, m.Phone_Cell, m.Phone_Home, m.EMail
ORDER BY m.Main_ID;


-- 2023

SELECT m.Main_ID,
       m.eGiftID,
       m.FirstName,
       m.LastName,
       m.CompanyName,
       m.Title,
       m.Phone_Cell,
       m.Phone_Home,
       m.EMail,
       SUM(CASE WHEN h.Participant = 1 THEN h.Raised ELSE 0 END) AS TotalRaised,
       COUNT(DISTINCT CASE WHEN h.Participant = 1  THEN h.EventYear END) AS NumYearsRiding
FROM tblMain AS m
LEFT JOIN tblHistory AS h ON m.Main_ID = h.Main_ID
WHERE m.Title IN ('COO', 'CFO', 'CEO') or 
	Title LIKE '%chief executive officer%'
    OR Title LIKE '%chief financial officer%'
    OR Title LIKE '%chief operating officer%'

AND h.EventYear = 2023
GROUP BY m.Main_ID, m.eGiftID, m.FirstName, m.LastName, m.CompanyName, m.Title, m.Phone_Cell, m.Phone_Home, m.EMail
ORDER BY m.Main_ID;

