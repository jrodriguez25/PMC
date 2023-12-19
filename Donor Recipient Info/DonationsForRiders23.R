library(tidyverse)
library(writexl)

# Read CSV files
tblMain <- read_csv("Desktop/PMC Github/tblMain.csv")
tblDonation23 <- read_csv("Desktop/PMC Github/tblDonation23.csv") ##TOO LARGE, Download all donation data from 2023 and replace here
tblGrandTotals <- read_csv("Desktop/PMC Github/tblGrandTotals.csv")
TblHistory2023 <- read_csv("Desktop/PMC Github/TblHistory2023.csv")

# Calculate Raised_All in TblHistory2023
TblHistory2023 <- TblHistory2023 %>%
  group_by(Main_ID) %>%
  mutate(Raised_All = sum(Raised))

# Filter TblHistory2023 for PMC events
tblHistory2023_TotalRaised <- TblHistory2023 %>%
  filter(EventName == "PMC")

# Aggregate data for tblDonation23
aggDonation <- tblDonation23 %>%
  group_by(Main_ID) %>%
  summarise(
    TotalAmountRecieved = sum(Amount),
    TotalDonations = n()
  )

# Join aggregated donation data with tblHistory2023_TotalRaised
DonationAndHistory <- aggDonation %>%
  left_join(tblHistory2023_TotalRaised, by = "Main_ID") %>%
  select(Main_ID, TotalAmountRecieved, TotalDonations, EventYear, Participant, Volunteer, Virtual, Commitment, Raised_All)

# Join with tblGrandTotals
with_grandTotals <- DonationAndHistory %>%
  left_join(tblGrandTotals, by = "Main_ID") %>%
  select(Main_ID, TotalAmountRecieved, TotalDonations, EventYear, Participant, Volunteer, Virtual, Commitment, Raised_All, P_Years, V_Years)

# Join with tblMain
final1 <- with_grandTotals %>%
  left_join(tblMain, by = "Main_ID") %>%
  select(eGiftID, FirstName, LastName, DOB, TotalAmountRecieved, TotalDonations, EventYear, Participant.x, Volunteer.x, Virtual, Commitment, Raised_All, P_Years, V_Years)

# Calculate Age and perform final renaming and selection
final <- final1 %>%
  mutate(
    Age = interval(DOB, Sys.time()) %/% duration(num = 1, units = "years"),
    Participant = Participant.x,
    Volunteer = Volunteer.x,
    Raised_AllEvents2023 = Raised_All,
    YearsRiding = P_Years,
    YearsVolunteering = V_Years
  ) %>%
  select(-DOB)

write_xlsx(final, "Total Amount and Number of Donations for Each Rider 2023.xlsx")



