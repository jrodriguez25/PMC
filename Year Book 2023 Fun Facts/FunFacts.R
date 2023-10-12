library(tidyverse)
library(readxl)

tblHistory <- read_excel("~/Desktop/PMC Github/median_mean_info/tblHistory.xlsx")

tblMain2023 <- read_excel("~/Desktop/PMC Github/Year Book 2023 Fun Facts/tblMain2023.xlsx")

# merge egiftID and Main --------------------------------------------------



# First Year Riders -------------------------------------------------------

first_year_riders <- tblHistory %>% 
  filter(Participant == 1) %>% 
  filter(EventName == "PMC") %>% 
  group_by(Main_ID) %>% 
  summarize(First_Year = min(EventYear))

n_first_year_riders <- first_year_riders %>% 
  group_by(First_Year) %>% 
  tally()


rd# First Year Volunteers ---------------------------------------------------


first_year_volunteers <- tblHistory %>% 
  filter(Volunteer == 1) %>% 
  filter(EventName == "PMC") %>% 
  group_by(Main_ID) %>% 
  summarise(First_Year = min(EventYear))

n_first_year_vol <- first_year_volunteers %>% 
  group_by(First_Year) %>% 
  tally()


# Rode every year ---------------------------------------------------------

total_years <- n_distinct(tblHistory$EventYear)

every_year <- tblHistory %>% 
  filter(Participant == 1) %>% 
  group_by(Main_ID) %>% 
  summarize(distinct_years = n_distinct(EventYear)) %>% 
  filter(distinct_years == 44) %>% 
  summarize(total_individuals = n())



# Truly Every Year --------------------------------------------------------

truly_every_year <- tblHistory %>% 
  filter(Participant == 1) %>% 
  filter(Virtual == 0) %>% 
  group_by(Main_ID) %>% 
  summarize(distinct_years = n_distinct(EventYear)) %>% 
  filter(distinct_years == 44) %>% 
  summarize(total_individuals = n())


# Heavy Hitters -----------------------------------------------------------

heavy_hitters <- tblHistory %>% 
  filter(EventYear == 2023) %>% 
  filter(Raised >=10000)

# Rode Classic Route ------------------------------------------------------
Rider_Route_Info <- read_excel("Desktop/PMC Github/Tracking Data/data/Rider Route Info.xlsx")

classic_riders <- Rider_Route_Info %>% 
  filter(`2023 Route...11`%in% c("Sturbridge to Provincetown Monument (2-Day)", "Sturbridge to Provincetown Inn (2-Day)"))

# rode PMC and Unpaved ----------------------------------------------------

rode_pmc <- tblHistory %>%
  filter(Participant == 1) %>% 
  filter(EventYear == 2023) %>% 
  filter(EventName == "PMC") %>% 
  select(Main_ID)

rode_unpaved <- tblHistory %>% 
  filter(Participant == 1) %>% 
  filter(EventYear == 2023) %>% 
  filter(EventName == "Unpaved") %>% 
  select(Main_ID )

rode_both <- rode_unpaved %>% inner_join(rode_pmc, by= "Main_ID")



# Longest Serving Volunteer -----------------------------------------------

longest_serving_vols <- tblHistory %>% 
  filter(Volunteer == 1) %>% 
  group_by(Main_ID) %>% 
  summarize(times = n())




# Number of Cyclist -------------------------------------------------------

Num_Riders_2023 <- tblHistory %>% 
  filter(Participant == 1) %>% 
  filter(EventName == "PMC") %>% 
  filter(EventYear == 2023) %>% 
  filter(Virtual == 0)%>% 
  select(Main_ID) %>% 
  distinct()

##6154 Off the Line Not Virtual##



# Num Volunteers 2023 -----------------------------------------------------

Num_Vols_2023 <- tblHistory %>% 
  filter(Volunteer == 1) %>% 
  filter(EventName == "PMC") %>% 
  filter(EventYear == 2023) %>% 
  select(Main_ID) %>% 
  distinct()

#2982



# Percent Male/Female -----------------------------------------------------

Riders_2023 <- tblHistory %>% 
  filter(EventYear == 2023) %>% 
  filter(Participant == 1) %>% 
  filter(EventName == "PMC")


Male_Female <- tblMain2023 %>% 
  select(Main_ID, Gender)

Male_Female <- Riders_2023 %>% 
  left_join(Male_Female, by = "Main_ID")

#Calculation for percentages

Male_Female %>% 
  filter(Gender == "M" | Gender == "F") %>% 
  group_by(Gender) %>% 
  summarize(count= n()) %>% 
  mutate(pct = (count/sum(count)) *100) %>% 
  arrange(desc(pct))



# Average Age -------------------------------------------------------------

Main_ID_DOB <- tblMain2023 %>% 
  select(Main_ID, DOB) %>% 
  mutate(DOB = as.Date(DOB)) %>% 
  mutate(Age = floor(interval(DOB, Sys.Date())/years(1)))

#check for outliers
current_date <- Sys.Date()

Main_ID_DOB <- Main_ID_DOB %>%
  rowwise() %>%
  mutate(Age = as.numeric(difftime(current_date, DOB, units = "weeks")/52.25),
         Outlier = ifelse(Age > 120 | DOB > current_date, "Yes", "No")) %>%
  ungroup()  

#count outliers

Main_ID_DOB %>% 
  filter(Outlier == "Yes") %>% 
  nrow()

#53 outliers

Main_ID_DOB %>% 
  filter(Outlier == "No") %>% 
  filter(Age>13) %>% 
  mutate(Age = round(Age, 0)) %>% 
  summarize(mean = mean(Age))

# Increments of Riders ----------------------------------------------------

#40, 30, 20, 10


tblHistory_enhanced <- tblHistory %>% 
  left_join(first_year_riders, by = "Main_ID")

Num_Years_Riding_2023 <- tblHistory_enhanced %>% 
  mutate(Years_Riding = 2023-First_Year+1) %>% 
  mutate(Riding_Group = case_when(
    Years_Riding >= 40 ~ "40+",
    Years_Riding >= 30 ~ "30+",
    Years_Riding >= 20 ~ "20+",
    Years_Riding >= 10 ~ "10+",
    TRUE ~ "Less than 10"
  )) %>% 
  filter(EventYear == 2023) %>% 
  filter(EventName == "PMC") %>% 
  filter(Participant == 1)

#Calculation
count(Num_Years_Riding_2023, Riding_Group) %>% 
  arrange(desc(Riding_Group))



# By State ----------------------------------------------------------------


         
#From MA, Other NE States, 42 Remaining, Country

us_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
               "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
               "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
               "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
               "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

new_england_states <- c("CT",  
                        "ME",  
                        "MA", 
                        "NH",  
                        "RI",  
                        "VT")  


Main_ID_State <- tblMain2023 %>% 
  select(Main_ID, Address1, Address2, State, CountryCode, City) %>% 
  mutate(State = toupper(State)) %>% 
  filter(!(State == "NULL" & CountryCode == "NULL" & City == "NULL")) %>% 
  filter((State %in% us_states))

showedup_non_us_ <- Main_ID_State %>% 
  filter(!(State %in% us_states)) %>%
  distinct()

unique_states <- 
  Main_ID_State %>% 
  select(State) %>% 
  distinct()

Riders <- tblHistory_enhanced %>% 
  filter(EventYear == 2023) %>% 
  filter(Participant == 1) %>% 
  filter(EventName == "PMC")

MainID_State2023 <- Riders %>% 
  left_join(Main_ID_State, by = "Main_ID") %>% 
  select(Main_ID, Address1, Address2, State, CountryCode, City) %>% 
  filter(City != "Amsterdam") %>% 
  filter()
  

