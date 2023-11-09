SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW dbo.TenFRHistory
--
--	Build up a crosstab based on the
--	FR History Function
as

	Select 
		left(cPayDate,5) as PayDate,
		Sum(Case when cYear='C0' then SumAmount else 0 end) as C0,
		Sum(Case when cYear='C1' then SumAmount else 0 end) as C1,
		Sum(Case when cYear='C2' then SumAmount else 0 end) as C2
		from dbo.fn_10YRFRHistory()
		group by left(cPayDate,5)
GO
vol


--drop view TenFRHistory


select * from TenFRHistory