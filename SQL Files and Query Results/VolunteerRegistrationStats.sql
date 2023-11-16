-- Volunteer Registration Stats


Select * from tblRegVolunteer
WHERE EventYear = CONVERT(int, dbo.fn_GetYear())