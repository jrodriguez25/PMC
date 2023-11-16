SET QUOTED_IDENTIFIER ON
GO
SET ANSI_NULLS ON
GO
CREATE     function dbo.fn_10YRFRHistory()
--
--	This is the source for the Cross tab query
--	v_rptFRHistory
--
--	select * from v_rptFRHistory
--
--	2005-10-27 hmw Modified to exclude Company Matching Payments
--

returns @FRHistoryTab table
	(
		SumAmount	money,
		cPayDate	Varchar(10),
		cYear		Varchar(10),
		EventYear	char(4),
		PayYear		Char(4)
	)
as
begin

declare @EventYear int
declare @FirstYear char(4)

	--	Grab the current year
	--
	set @EventYear = convert(int,2017)
	set @FirstYear = convert(char(4),@EventYear-2)

	--	Grab all of the fundraising Detail records fo rthe 
	--	past 3 years
	--
	insert @FRHistoryTab
		select Sum(d.TotalAmount) as SumAmount, 
		convert(varchar,h.PayDate,1) as cPaydate, 
		'C' + convert(varchar, @EventYear - convert(int,h.EventYear)) as cYear,
		h.EventYear, convert(char(4), datepart(yyyy,h.PayDate))
		From tblPaymentHeader h
		Inner Join tblPaymentDetail d on h.PaymentID = d.PaymentID
		Where h.EventYear >= @FirstYear 
		  and d.PC_Type = 'D'
		  or (h.CompanyID is not null and h.p_type in ('4','5','6','7'))
		Group by convert(varchar,h.PayDate,1), 'C' + convert(varchar, @EventYear - convert(int,h.EventYear)), h.EventYear, convert(char(4), datepart(yyyy,h.PayDate))
		order by cPayDate

		--	Take care of the Payments that were made in the next year
		--
		update @FRHistoryTab set cPayDate = '13/31/99' where PayYear > EventYEar

		--	Take care of the Payments that were made in the prior year
		--
		update @FRHistoryTab set cPayDate = '00/00/00' where PayYear < EventYEar

	return

end
GO


--drop function dbo.fn_10YRFRHistory