library(tidyverse)
library(readxl)

tblMain <- read_excel("~/Desktop/PMC Github/median_mean_info/tblMain.xlsx")

tblMain2023 <- read_excel("Desktop/PMC Github/Year Book 2023 Fun Facts/tblMain2023.xlsx")

# merge egiftID and Main --------------------------------------------------

tblMain2023 <- tblMain2023 %>% 
  select(Main_ID, eGiftID)

# First Year Riders -------------------------------------------------------

first_year_riders <- tblMain %>% 
  filter(Participant == 1) %>% 
  filter(EventName == "PMC") %>% 
  group_by(Main_ID) %>% 
  summarize(First_Year = min(EventYear))

n_first_year_riders <- first_year_riders %>% 
  group_by(First_Year) %>% 
  tally()


# First Year Volunteers ---------------------------------------------------


first_year_volunteers <- tblMain %>% 
  filter(Volunteer == 1) %>% 
  filter(EventName == "PMC") %>% 
  group_by(Main_ID) %>% 
  summarise(First_Year = min(EventYear))

n_first_year_vol <- first_year_volunteers %>% 
  group_by(First_Year) %>% 
  tally()


# Rode every year ---------------------------------------------------------

total_years <- n_distinct(tblMain$EventYear)

every_year <- tblMain %>% 
  filter(Participant == 1) %>% 
  group_by(Main_ID) %>% 
  summarize(distinct_years = n_distinct(EventYear)) %>% 
  filter(distinct_years == 44) %>% 
  summarize(total_individuals = n())



# Truly Every Year --------------------------------------------------------

truly_every_year <- tblMain %>% 
  filter(Participant == 1) %>% 
  filter(Virtual == 0) %>% 
  group_by(Main_ID) %>% 
  summarize(distinct_years = n_distinct(EventYear)) %>% 
  filter(distinct_years == 44) %>% 
  summarize(total_individuals = n())


# Heavy Hitters -----------------------------------------------------------

heavy_hitters <- tblMain %>% 
  filter(EventYear == 2023) %>% 
  filter(Raised >=10000)

# Rode Classic Route ------------------------------------------------------
Rider_Route_Info <- read_excel("Desktop/PMC Github/Tracking Data/data/Rider Route Info.xlsx")

classic_riders <- Rider_Route_Info %>% 
  filter(`2023 Route...11`%in% c("Sturbridge to Provincetown Monument (2-Day)", "Sturbridge to Provincetown Inn (2-Day)"))

# rode PMC and Unpaved ----------------------------------------------------

rode_pmc <- tblMain %>%
  filter(Participant == 1) %>% 
  filter(EventYear == 2023) %>% 
  filter(EventName == "PMC") %>% 
  select(Main_ID)

rode_unpaved <- tblMain %>% 
  filter(Participant == 1) %>% 
  filter(EventYear == 2023) %>% 
  filter(EventName == "Unpaved") %>% 
  select(Main_ID )

rode_both <- rode_unpaved %>% inner_join(rode_pmc, by= "Main_ID")



# Longest Serving Volunteer -----------------------------------------------

longest_serving_vols <- tblMain %>% 
  filter(Volunteer == 1) %>% 
  group_by(Main_ID) %>% 
  summarize(times = n())


  
