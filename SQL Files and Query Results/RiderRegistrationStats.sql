-- Rider Registration Stats with First Year This Year
SELECT 
    m.FirstName, 
    m.LastName, 
    m.Email, 
    m.DOB,
    m.State,
    m.Gender,
    DATEDIFF(YEAR, m.DOB, GETDATE()) - 
        CASE 
            WHEN DATEADD(YEAR, DATEDIFF(YEAR, m.DOB, GETDATE()), m.DOB) > GETDATE() 
            THEN 1 
            ELSE 0 
        END AS Age,
    h.Raised, 
    h.Commitment, 
    c.Description AS eventdesc, 
    m.CountryCode,
    m.MED_Cancer, --- Living Proof?
    (
        SELECT MIN(EventYear)
        FROM dbo.tblHistory AS sub
        WHERE sub.Main_ID = m.Main_ID
    ) AS [First Year Riding],
    CASE 
        WHEN (
            SELECT MIN(EventYear)
            FROM dbo.tblHistory AS sub
            WHERE sub.Main_ID = m.Main_ID
        ) = CONVERT(int, dbo.fn_GetYear())
        THEN 1
        ELSE 0
    END AS [First Year This Year]
FROM dbo.tblMain m
INNER JOIN dbo.tblHistory h ON h.Main_ID = m.Main_ID AND h.EventName = 'pmc' AND h.EventYear = CONVERT(int, dbo.fn_GetYear())
INNER JOIN dbo.tblSysCode c ON c.CodeType = 'event' AND c.code = h.event
WHERE 
    m.MainType IS NULL 
    AND m.Deceased = 0
    AND h.participant = 1;


