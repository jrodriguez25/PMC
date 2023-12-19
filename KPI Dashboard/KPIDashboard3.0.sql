--Riders PMC Only

with m as (
  select
    m.Main_ID, m.FirstName, m.LastName, m.MED_Cancer, m.gender, UPPER(m.City) AS City, m.CountryCode,
    case when isnull(CountryCode,'US') = 'US' then m.State else null end as State
  from tblMain m
  where m.MainType is null
),h as (
  select
    h.Main_ID, h.EventYear, h.Raised, h.Participant, h.Volunteer, h.Virtual, h.TeamID, h.EventName,
    DATEDIFF(YEAR, m.DOB, h.EventYear) as Age,
    c.Description as Route,
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
  from tblHistory h
  inner join tblMain m on m.Main_ID=h.Main_ID
  inner join tblRegRider r on r.Main_ID=m.Main_ID and r.EventYear=h.EventYear
  left join tblSysCode c on c.CodeType='Event' and c.Code=r.Event
  where h.EventYear>='2000'
), a as (
  select ROW_NUMBER() over (partition by Main_ID order by Main_ID) as YearsRiding,
  main_ID,eventyear from
  (
    select Main_ID,eventyear
    from tblHistory h
    where h.Participant=1
    group by Main_ID,eventyear
  ) a
  group by main_ID,eventyear
)
select
  m.*, h.*, a.YearsRiding,
  case when a.YearsRiding >1 then 1 else 0 end as RodePreviously
from m
inner join h on h.Main_ID=m.Main_ID
left join a on a.Main_ID=m.Main_ID and a.EventYear=h.EventYear

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

----Riders Unpaved
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
WHERE m.MainType IS NULL and h.Participant = 1 and h.EventName = 'Unpaved';




