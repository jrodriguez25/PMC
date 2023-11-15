----ALL QUERIES FOR PMC



-- Total Number of Riders


SELECT COUNT(Main_ID) 
FROM tblRegRider 
WHERE EventYear = CONVERT(int, dbo.fn_CharCurrentYear())
AND (R_Status = 'V' OR R_Status = 'R');

SELECT COUNT(DISTINCT m.Main_ID) as NumberOfRegisteredRiders
FROM pmcweb.dbo.tblMain m 
INNER JOIN pmcweb.dbo.tblRegRider r3 ON r3.Main_ID = m.Main_ID AND r3.EventYear = 2023
WHERE m.MainType IS NULL AND r3.Event IS NOT NULL;


--Average Age of Riders

SELECT
    AVG(DATEDIFF(YEAR, m.DOB, GETDATE())) AS AverageAge
FROM
    tblMain m
    INNER JOIN tblRegRider r ON r.Main_ID = m.Main_ID 
WHERE
    r.EventYear = CONVERT(int, dbo.fn_CharCurrentYear())
    AND r.R_Status = 'r'
    AND m.MainType IS NULL;

-- Average Raised Per Rider

SELECT 
    AVG(h.Raised) AS AverageRaised,
    m.MainType
FROM 
    tblHistory h
    INNER JOIN tblMain m ON h.Main_ID = m.Main_ID
WHERE 
    h.EventYear = CONVERT(int, dbo.fn_CharCurrentYear()) AND
    h.Participant = 1 AND
    h.Virtual = 0 AND
    m.MainType IS NULL AND
	h.EventName like '%pmc%'
GROUP BY 
    m.MainType;


-- Heavy Hitters

select Count(Raised) from tblHistory 
Where EventYear = CONVERT( int, dbo.fn_CharCurrentYear()) AND
EventName like '%pmc%' AND
Raised >= 10000


-- Percent and Number Male, Female, Non-Binary

SELECT
    SUM(CASE WHEN m.Gender = 'M' THEN 1 ELSE 0 END) * 100.0 / COUNT(*) AS PercentMale,
    SUM(CASE WHEN m.Gender = 'F' THEN 1 ELSE 0 END) * 100.0 / COUNT(*) AS PercentFemale,
    SUM(CASE WHEN m.Gender IS NULL OR m.Gender NOT IN ('M', 'F') THEN 1 ELSE 0 END) * 100.0 / COUNT(*) AS PercentNullOrNonBinary
FROM
    tblMain m
INNER JOIN tblHistory h ON m.Main_ID = h.Main_ID
WHERE
    m.MainType IS NULL AND 
	h.[Virtual] = 0 AND
	h.Participant = 1 AND
    h.EventYear = CONVERT(int, dbo.fn_CharCurrentYear()) AND
	h.EventName like '%pmc%';



-- Total Number of Volunteers


select  COUNT (Main_ID) from tblRegVolunteer
Where EventYear = convert(int,dbo.fn_CharCurrentYear())



--Total Raised (Donors)

select SUM(Amount) from tblDonation
WHERE EventYear = convert(int,dbo.fn_CharCurrentYear()) 




-- Total Number of Donors

select COUNT(Main_ID) from tblDonation
Where EventYear = convert(int,dbo.fn_CharCurrentYear())

-- Average Donation

select AVG(Amount) from tblDonation
WHERE EventYear = convert(int,dbo.fn_CharCurrentYear()) 

select * from tblDonation

