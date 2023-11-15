-- Total Number of Riders

Select Count (Main_ID) from Unpaved.tblRegRider
Where EventYear = CONVERT(int, dbo.fn_GetYear())
;


--Average Age of Riders

SELECT
    AVG(DATEDIFF(YEAR, m.DOB, GETDATE())) AS AverageAge
FROM
    tblMain m
   Inner join pmcweb.Unpaved.tblRegRider r on m.Main_ID = r.Main_ID and r.EventYear = pmcweb.dbo.fn_GetYear()
Inner Join pmcweb.dbo.tblSysCode s on s.code = r.EventOption and s.CodeType = 'EventOption_Unpaved'

select * from tblReg

-- Average Raised Per Rider

SELECT 
    AVG(h.Raised) AS AverageRaised
FROM 
    tblHistory h
    INNER JOIN Unpaved.tblRegRider u ON h.Main_ID = u.Main_ID
	Where u.EventYear = convert(int, dbo.fn_GetYear())


-- Summitters

select Count(Raised) from tblHistory 
Where EventYear = CONVERT( int, dbo.fn_GetYear()) AND
EventName like '%unpaved%' AND
Raised >= 4000


-- Percent and Number Male, Female, Non-Binary

SELECT
    SUM(CASE WHEN m.Gender = 'M' THEN 1 ELSE 0 END) * 100.0 / COUNT(*) AS PercentMale,
    SUM(CASE WHEN m.Gender = 'F' THEN 1 ELSE 0 END) * 100.0 / COUNT(*) AS PercentFemale,
    SUM(CASE WHEN m.Gender IS NULL OR m.Gender NOT IN ('M', 'F') THEN 1 ELSE 0 END) * 100.0 / COUNT(*) AS PercentNullOrNonBinary
FROM
    tblMain m
INNER JOIN Unpaved.tblRegRider u ON m.Main_ID = u.Main_ID
WHERE
    m.MainType IS NULL AND 
	u.EventYear = convert(int, dbo.fn_GetYear())

-- Total Number of Volunteers


select  COUNT (Main_ID) from Unpaved.tblRegVolunteer
Where EventYear = convert(int,dbo.fn_GetYear())

select * from unpaved.tblRegRider

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

