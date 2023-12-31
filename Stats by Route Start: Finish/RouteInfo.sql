select distinct 
h.Main_ID,
r3.RegDate,
r3.EventYear,
s6.Description as Route
from dbo.tblMain h
inner join tblRegRider r3 on r3.Main_ID = h.Main_ID --and r3.EventYear = 2023
left join dbo.tblSysCode s6 on s6.code = r3.Event and s6.CodeType = 'Event'
where r3.Event is not null