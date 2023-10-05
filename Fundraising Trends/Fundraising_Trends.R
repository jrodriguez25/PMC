library(tidyverse)
library(readxl)
rm(list = ls())

tblMain <- read_excel("~/GitHub/PMC/median_mean_info/tblMain.xlsx")


# Question 1  -------------------------------------------------------------

#What is the median AND mean (avg) number of years riding of all riders since 1980 by year

colnames(tblMain)


years <- data.frame(EventYear=1980:2023)

Num_YR_riding <- tblMain %>%
  filter(Participant == 1) %>%
  filter(EventName == "PMC") %>%
  select(Main_ID, EventYear, Collected)


# For each Main_ID and EventYear, compute how many years they've ridden before that EventYear

past_rides <- Num_YR_riding %>%
  left_join(Num_YR_riding, by = "Main_ID") %>%
  filter(EventYear.x > EventYear.y) %>%
  group_by(Main_ID, EventYear.x) %>%
  summarise(past_years = n()) %>%
  ungroup()

# So we need to ensure those riders are accounted for with a value of 0 for past_years.
first_rides <- Num_YR_riding %>%
  anti_join(past_rides, by = c("Main_ID", "EventYear" = "EventYear.x")) %>%
  mutate(past_years = 0)

# Combine both datasets and then compute the average past years ridden by year.
median_mean_rides_per_year <- bind_rows(past_rides, first_rides) %>%
  group_by(EventYear.x) %>%
  summarise(average_years_ridden = mean(past_years, na.rm = TRUE),
            median_years_ridden = median(past_years, na.rm = TRUE)) %>%
  rename(EventYear = EventYear.x)





# Questions 2 -------------------------------------------------------------

# What is the median AND mean (avg) number of years riding of all riders who rode =>2 year since 1980 by year

Num_YR_riding_Greater2 <- tblMain %>%
  filter(Participant == 1) %>%
  filter(EventName == "PMC") %>%
  select(Main_ID, EventYear, Collected) %>% 
  group_by(Main_ID) %>% 
  filter(n()>=2)  #shows up more than 2 times aka rode +2 times

#Same process

past_rides_2Plus <- Num_YR_riding_Greater2 %>%
  left_join(Num_YR_riding_Greater2, by = "Main_ID") %>%
  filter(EventYear.x > EventYear.y) %>%
  group_by(Main_ID, EventYear.x) %>%
  summarise(past_years = n()) %>%
  ungroup()

# So we need to ensure those riders are accounted for with a value of 0 for past_years.
first_rides_2plus <- Num_YR_riding_Greater2 %>%
  anti_join(past_rides_2Plus, by = c("Main_ID", "EventYear" = "EventYear.x")) %>%
  mutate(past_years = 0)

# Combine both datasets and then compute the average past years ridden by year.
median_mean_rides_per_year_2plus <- bind_rows(past_rides_2Plus, first_rides_2plus) %>%
  group_by(EventYear.x) %>%
  summarise(average_years_ridden = mean(past_years, na.rm = TRUE),
            median_years_ridden = median(past_years, na.rm = TRUE)) %>%
  rename(EventYear = EventYear.x)



#Prints exactly same output, but for 3 it prints different results. Lets me know that function is correct. Why is this so?


# Question 3 --------------------------------------------------------------

#What is the median AND mean (avg) Total Lifetime fundraising of all riders since 1980 by year

Total_Fundraising <- tblMain %>% 
  filter(Participant == 1) %>% 
  filter(EventName == "PMC") %>% 
  select(Main_ID, EventYear, Collected) 

# Calculate lifetime fundraising total for each rider by year
lifetime_funds <- Total_Fundraising %>%
  group_by(Main_ID) %>%
  arrange(Main_ID, EventYear) %>%
  mutate(cumulative_funds = cumsum(Collected)) %>%
  ungroup()

# Calculate average and median lifetime fundraising by year
median_mean_Totalfundraised_per_year  <- lifetime_funds %>%
  group_by(EventYear) %>%
  summarise(
    average_lifetime_fundraising = mean(cumulative_funds, na.rm = TRUE),
    median_lifetime_fundraising = median(cumulative_funds, na.rm = TRUE)
  )




# Question 4 --------------------------------------------------------------

# What is the median AND mean (avg) Total Lifetime fundraising all riders who rode =>2 year since 1980 by year



Total_Fundraising2Plus <- tblMain %>% 
  filter(Participant == 1) %>% 
  filter(EventName == "PMC") %>% 
  select(Main_ID, EventYear, Collected) %>% 
  group_by(Main_ID) %>% 
  filter(n()>=2)


# Calculate lifetime fundraising total for each rider by year
lifetime_funds2Plus <- Total_Fundraising2Plus %>%
  group_by(Main_ID) %>%
  arrange(Main_ID, EventYear) %>%
  mutate(cumulative_funds = cumsum(Collected)) %>%
  ungroup()

# Calculate average and median lifetime fundraising by year
median_mean_Totalfundraised_per_year_2plus <- lifetime_funds2Plus %>%
  group_by(EventYear) %>%
  summarise(
    average_lifetime_fundraising = mean(cumulative_funds, na.rm = TRUE),
    median_lifetime_fundraising = median(cumulative_funds, na.rm = TRUE)
  )


lifetime_funds_selectedYear <- lifetime_funds %>% 
  filter(EventYear == 1988)


# 3+  Fundraising -----------------------------------------------
Total_Fundraising3Plus <- tblMain %>% 
  filter(Participant == 1) %>% 
  filter(EventName == "PMC") %>% 
  select(Main_ID, EventYear, Collected) %>% 
  group_by(Main_ID) %>% 
  filter(n()>=3)


# Calculate lifetime fundraising total for each rider by year
lifetime_funds3Plus <- Total_Fundraising3Plus %>%
  group_by(Main_ID) %>%
  arrange(Main_ID, EventYear) %>%
  mutate(cumulative_funds = cumsum(Collected)) %>%
  ungroup()

# Calculate average and median lifetime fundraising by year
median_mean_Totalfundraised_per_year_3Plus <- lifetime_funds3Plus %>%
  group_by(EventYear) %>%
  summarise(
    average_lifetime_fundraising = mean(cumulative_funds, na.rm = TRUE),
    median_lifetime_fundraising = median(cumulative_funds, na.rm = TRUE)
    
  )


# 3 Plus Riding -----------------------------------------------------------


Num_YR_riding_Greater3 <- tblMain %>%
  filter(Participant == 1) %>%
  filter(EventName == "PMC") %>%
  select(Main_ID, EventYear, Collected) %>% 
  group_by(Main_ID) %>% 
  filter(n()>=3)  #shows up more than 2 times aka rode +2 times

#Same process

past_rides_Greater3 <- Num_YR_riding_Greater3 %>%
  left_join(Num_YR_riding_Greater3, by = "Main_ID") %>%
  filter(EventYear.x > EventYear.y) %>%
  group_by(Main_ID, EventYear.x) %>%
  summarise(past_years = n()) %>%
  ungroup()

# So we need to ensure those riders are accounted for with a value of 0 for past_years.
first_rides_Greater3 <- Num_YR_riding_Greater3 %>%
  anti_join(past_rides_Greater3, by = c("Main_ID", "EventYear" = "EventYear.x")) %>%
  mutate(past_years = 0)

# Combine both datasets and then compute the average past years ridden by year.
median_mean_rides_per_year_Greater3 <- bind_rows(past_rides_Greater3, first_rides_Greater3) %>%
  group_by(EventYear.x) %>%
  summarise(average_years_ridden = mean(past_years, na.rm = TRUE),
            median_years_ridden = median(past_years, na.rm = TRUE)) %>%
  rename(EventYear = EventYear.x)





