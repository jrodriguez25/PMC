

# Average Raised ----------------------------------------------------------


Winter_Average_Rider_Donation <- tblHistory %>% 
  filter(EventYear == 2023) %>% 
  filter(EventName == "Winter-Cycle-Boston") %>% 
  filter(Participant == 1) %>% 
  select(Main_ID, Raised) %>%
  filter(Raised > 0) %>% 
  filter(!(Main_ID %in% Exclude_MainIDs)) %>% 
  distinct(Main_ID, .keep_all = TRUE) %>% 
  summarize(mean = mean(Raised), 
            sum = sum(Raised))

#GOOD#

# Average Age -------------------------------------------------------------

Average_AgeWinter <- tblHistory %>%
  left_join(Main_ID_DOB, by = "Main_ID") %>%
  filter(EventName == "Winter-Cycle-Boston") %>% 
  filter(EventYear == 2023) %>% 
  filter (Participant == 1) %>% 
  mutate(Age = round(Age, 0)) %>% 
  summarize(mean = mean(Age, na.rm = TRUE))


# Volunteers --------------------------------------------------------------



WC_Volunteers_2023 <- tblHistory %>% 
  filter(EventYear == 2023) %>% 
  filter(EventName == "Winter-Cycle-Boston") %>% 
  filter(Volunteer == 1) %>% 
  filter(Virtual == 0) %>% 
  summarize(count = n())

# Gender Info -------------------------------------------------------------

WC_Riders_2023 <- tblHistory %>% 
  filter(EventYear == 2023) %>% 
  filter(Participant == 1) %>% 
  filter(EventName == "Winter-Cycle-Boston") %>% 
  filter(!(Fundraiser == 0 & NonFund == 0.00 & Collected == 0 & Raised == 0 & MinFund == 0 & Commitment == 0))
  


Male_Female <- tblMain2023 %>% 
  select(Main_ID, Gender)

WC_Male_Female <- WC_Riders_2023 %>% 
  left_join(Male_Female, by = "Main_ID")

#Calculation for percentages

WC_Male_Female %>% 
  group_by(Gender) %>% 
  summarize(count= n()) %>% 
  mutate(pct = (count/sum(count)) *100) %>% 
  arrange(desc(pct))

