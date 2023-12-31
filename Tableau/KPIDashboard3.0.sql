--Riders PMC Only
-- Riders PMC Only
WITH m AS (
  SELECT
    m.Main_ID, m.FirstName, m.LastName, m.MED_Cancer, m.gender, UPPER(m.City) AS City, m.CountryCode,
    CASE WHEN ISNULL(CountryCode, 'US') = 'US' THEN m.State ELSE NULL END AS State
  FROM tblMain m
  WHERE m.MainType IS NULL
),
h AS (
  SELECT
    h.Main_ID, h.EventYear, h.Raised, h.Participant, h.Volunteer, h.Virtual, h.TeamID, h.EventName,
    DATEDIFF(YEAR, m.DOB, h.EventYear) AS Age,
    c.Description AS Route,
    r.RegDate,
    CASE
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 13 AND 19 THEN '<20'
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 20 AND 29 THEN '20-29'
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 30 AND 39 THEN '30-39'
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 40 AND 49 THEN '40-49'
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 50 AND 59 THEN '50-59'
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 60 AND 69 THEN '60-69'
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 70 AND 79 THEN '70-79'
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) >= 80 THEN '80+'
    END AS AgeGroup
  FROM tblHistory h
    INNER JOIN tblMain m ON m.Main_ID = h.Main_ID
    INNER JOIN tblRegRider r ON r.Main_ID = m.Main_ID AND r.EventYear = h.EventYear
    LEFT JOIN tblSysCode c ON c.CodeType = 'Event' AND c.Code = r.Event
  WHERE h.EventYear >= '2000'
),
a AS (
  SELECT ROW_NUMBER() OVER (PARTITION BY Main_ID ORDER BY Main_ID) AS YearsRiding,
    Main_ID, EventYear
  FROM (
    SELECT Main_ID, EventYear
    FROM tblHistory h
    WHERE h.Participant = 1
    GROUP BY Main_ID, EventYear
  ) a
  GROUP BY Main_ID, EventYear
)
SELECT
  m.*, h.*, a.YearsRiding,
  CASE WHEN a.YearsRiding > 1 THEN 1 ELSE 0 END AS RodePreviously,
  CASE WHEN h.Route IN ('Reimagined', 'Reimagined Teen') THEN 1 ELSE 0 END AS Reimagined
FROM m
  INNER JOIN h ON h.Main_ID = m.Main_ID
  LEFT JOIN a ON a.Main_ID = m.Main_ID AND a.EventYear = h.EventYear;


---Volunteers Only

SELECT DISTINCT
    h.Main_ID, 
    m.Firstname,
    m.Lastname,
    h.EventYear, 
    h.Raised, 
    h.Participant, 
    h.Volunteer, 
    h.EventName, 
    m.MED_Cancer,
    CASE 
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 13 AND 19 THEN '<20'
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 20 AND 29 THEN '20-29'
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 30 AND 39 THEN '30-39'
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 40 AND 49 THEN '40-49'
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 50 AND 59 THEN '50-59'
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 60 AND 69 THEN '60-69'
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 70 AND 79 THEN '70-79'
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) >= 80 THEN '80+'
    END AS AgeGroup,
    CASE
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 13 AND 96 THEN DATEDIFF(YEAR, m.DOB, h.EventYear)
    END AS Age,
    m.Gender, 
    UPPER(m.City) AS City,
    CASE 
        WHEN m.State IN ('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'FL', 'GA', 'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY') THEN m.State
        ELSE NULL
    END AS State,
    m.CountryCode,
    (
        SELECT COUNT(DISTINCT EventYear)
        FROM tblHistory
        WHERE Main_ID = h.Main_ID AND EventYear <= h.EventYear
    ) AS YearsVolunteering,
    CASE 
        WHEN h.EventYear = (SELECT MIN(EventYear) FROM tblHistory WHERE Main_ID = h.Main_ID) THEN 0
        ELSE 1
    END AS VolunteeredPreviously
FROM 
    tblHistory h
LEFT JOIN 
    dbo.tblMain m ON m.Main_ID = h.Main_ID
WHERE m.MainType IS NULL and h.Volunteer = 1;

---Riders Winter Cycle

SELECT DISTINCT
    h.Main_ID, 
    m.Firstname,
    m.Lastname,
    h.EventYear, 
    h.Raised, 
    h.Participant, 
    h.Volunteer, 
	h.TeamID,
	h.Virtual,
    h.EventName, 
    m.MED_Cancer,
    CASE 
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 13 AND 19 THEN '<20'
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 20 AND 29 THEN '20-29'
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 30 AND 39 THEN '30-39'
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 40 AND 49 THEN '40-49'
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 50 AND 59 THEN '50-59'
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 60 AND 69 THEN '60-69'
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 70 AND 79 THEN '70-79'
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) >= 80 THEN '80+'
    END AS AgeGroup,
    CASE
        WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 13 AND 96 THEN DATEDIFF(YEAR, m.DOB, h.EventYear)
    END AS Age,
    m.Gender, 
    UPPER(m.City) AS City,
    CASE 
        WHEN m.State IN ('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'FL', 'GA', 'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY') THEN m.State
        ELSE NULL
    END AS State,
    m.CountryCode,
    (
        SELECT COUNT(DISTINCT EventYear)
        FROM tblHistory
        WHERE Main_ID = h.Main_ID AND EventYear <= h.EventYear
    ) AS YearsRiding,
    CASE 
        WHEN h.EventYear = (SELECT MIN(EventYear) FROM tblHistory WHERE Main_ID = h.Main_ID) THEN 0
        ELSE 1
    END AS RodePreviously
FROM 
    tblHistory h
LEFT JOIN 
    dbo.tblMain m ON m.Main_ID = h.Main_ID
WHERE m.MainType IS NULL and h.Participant = 1 and h.EventName = 'Winter-Cycle-Boston';





---UNPAVED WITH ROUTES


WITH m AS (
  SELECT
    m.Main_ID, m.FirstName, m.LastName, m.MED_Cancer, m.gender, UPPER(m.City) AS City, m.CountryCode,
    CASE WHEN ISNULL(CountryCode, 'US') = 'US' THEN m.State ELSE NULL END AS State
  FROM pmcweb.dbo.tblMain m
  WHERE m.MainType IS NULL
),
h AS (
  SELECT
    h.Main_ID, h.EventYear, h.Raised, h.Participant, h.Volunteer, h.Virtual, h.TeamID, h.EventName,
    DATEDIFF(YEAR, m.DOB, h.EventYear) AS Age,
    c.Description AS Route,
    r.RegDate,
    CASE
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 13 AND 19 THEN '<20'
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 20 AND 29 THEN '20-29'
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 30 AND 39 THEN '30-39'
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 40 AND 49 THEN '40-49'
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 50 AND 59 THEN '50-59'
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 60 AND 69 THEN '60-69'
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 70 AND 79 THEN '70-79'
      WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) >= 80 THEN '80+'
    END AS AgeGroup
  FROM pmcweb.dbo.tblHistory h
    INNER JOIN pmcweb.dbo.tblMain m ON m.Main_ID = h.Main_ID and h.EventName like '%unpaved%'
    INNER JOIN pmcweb.Unpaved.tblRegRider r ON r.Main_ID = m.Main_ID AND r.EventYear = h.EventYear
    LEFT JOIN pmcweb.dbo.tblSysCode c ON c.CodeType = 'EventOption_Unpaved' AND c.Code = r.EventOption
  WHERE h.EventYear >= '2000'
),
a AS (
  SELECT ROW_NUMBER() OVER (PARTITION BY Main_ID ORDER BY Main_ID) AS YearsRiding,
    Main_ID, EventYear
  FROM (
    SELECT Main_ID, EventYear
    FROM tblHistory h
    WHERE h.Participant = 1
    GROUP BY Main_ID, EventYear
  ) a
  GROUP BY Main_ID, EventYear
)
SELECT
  m.*, h.*, a.YearsRiding,
  CASE WHEN a.YearsRiding > 1 THEN 1 ELSE 0 END AS RodePreviously,
  CASE WHEN h.Route IN ('Reimagined', 'Reimagined Teen') THEN 1 ELSE 0 END AS Reimagined
FROM m
  INNER JOIN h ON h.Main_ID = m.Main_ID
  LEFT JOIN a ON a.Main_ID = m.Main_ID AND a.EventYear = h.EventYear;



-- ----Riders Unpaved
-- SELECT DISTINCT
--     h.Main_ID, 
--     m.Firstname,
--     m.Lastname,
--     h.EventYear, 
--     h.Raised, 
--     h.Participant, 
--     h.Volunteer, 
-- 	h.TeamID,
-- 	h.Virtual,
--     h.EventName, 
--     m.MED_Cancer,
--     CASE 
--         WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 13 AND 19 THEN '<20'
--         WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 20 AND 29 THEN '20-29'
--         WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 30 AND 39 THEN '30-39'
--         WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 40 AND 49 THEN '40-49'
--         WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 50 AND 59 THEN '50-59'
--         WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 60 AND 69 THEN '60-69'
--         WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 70 AND 79 THEN '70-79'
--         WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) >= 80 THEN '80+'
--     END AS AgeGroup,
--     CASE
--         WHEN DATEDIFF(YEAR, m.DOB, h.EventYear) BETWEEN 13 AND 96 THEN DATEDIFF(YEAR, m.DOB, h.EventYear)
--     END AS Age,
--     m.Gender, 
--     UPPER(m.City) AS City,
--     CASE 
--         WHEN m.State IN ('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'FL', 'GA', 'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY') THEN m.State
--         ELSE NULL
--     END AS State,
--     m.CountryCode,
--     (
--         SELECT COUNT(DISTINCT EventYear)
--         FROM tblHistory
--         WHERE Main_ID = h.Main_ID AND EventYear <= h.EventYear
--     ) AS YearsRiding,
--     CASE 
--         WHEN h.EventYear = (SELECT MIN(EventYear) FROM tblHistory WHERE Main_ID = h.Main_ID) THEN 0
--         ELSE 1
--     END AS RodePreviously
-- FROM 
--     tblHistory h
-- LEFT JOIN 
--     dbo.tblMain m ON m.Main_ID = h.Main_ID
-- WHERE m.MainType IS NULL and h.Participant = 1 and h.EventName = 'Unpaved';