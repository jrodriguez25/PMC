Select m.Main_ID, m.FirstName, m.LastName, m.Email, h.Raised, h.Commitment, h.EventYear, c.Description as eventdesc
From dbo.tblMain m
Inner Join dbo.tblHistory h on h.Main_ID = m.Main_ID and h.EventName = 'pmc' 
Inner Join dbo.tblSysCode c on c.CodeType = 'event' and c.code = h.event
Where m.MainType is null and m.Deceased = 0
and h.participant = 1