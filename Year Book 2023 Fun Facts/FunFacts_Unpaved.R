library(tidyverse)
library(readxl)
library(lubridate)

# Load Data ---------------------------------------------------------------



tblHistory <- read_excel("~/Desktop/PMC Github/Year Book 2023 Fun Facts/tblHistory.xlsx")



tblMain2023 <- read_excel("Desktop/PMC Github/Year Book 2023 Fun Facts/tblMain2023.xlsx", 
                          col_types = c("text", "text", "numeric", 
                                        "text", "text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "text", "numeric", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "date", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "text", "numeric", 
                                        "numeric", "numeric", "text", "text", 
                                        "text", "text", "text", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "numeric", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "text", "text", "text", "text", 
                                        "text", "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "text", "text", "text", 
                                        "text", "text", "text"))


# Number of Unpaved Riders ------------------------------------------------


Unpaved_Riders_2023 <- tblHistory %>% 
  filter(EventYear == 2023) %>% 
  filter(EventName == "Unpaved") %>% 
  filter(Participant == 1) 

# First Year Riders -------------------------------------------------------


Unpaved_ALL <- tblHistory %>% 
  filter(EventName == "Unpaved") %>% 
  filter(Participant == 1) %>% 
  filter(Virtual == 0) %>% 
  group_by(Main_ID) %>% 
  summarize(First_Year = min(EventYear))

Num_FY_Unpaved <- Unpaved_ALL %>% 
  group_by(First_Year) %>% 
  tally()

tblMain2023$MainType

# Average Age -------------------------------------------------------------
Main_ID_DOB <- tblMain2023 %>% 
  filter(!(is.null(MainType))) %>% 
  select(Main_ID, DOB) %>% 
  # mutate(DOB = as.Date(DOB)) %>% 
  mutate(Age = floor(interval(DOB, Sys.Date())/years(1)))




Average_AgeUnpaved <- tblHistory %>%
  left_join(Main_ID_DOB, by = "Main_ID") %>%
  filter(EventName == "Unpaved") %>% 
  filter(EventYear == 2023) %>% 
  filter (Participant == 1) %>% 
  mutate(Age = round(Age, 0)) %>% 
  summarize(mean = mean(Age, na.rm = TRUE))



# Average Raised ----------------------------------------------------------
UnpavedAvg_Rider_Donation <- tblHistory %>% 
  filter(EventYear == 2023) %>% 
  filter(EventName == "Unpaved") %>% 
  filter(Participant == 1) %>% 
  filter(Virtual == 0) %>% 
  select(Main_ID, Raised) %>%
  filter(Raised > 0) %>% 
  distinct(Main_ID, .keep_all = TRUE) %>% 
  summarize(mean = mean(Raised))

  



# Gender Info -------------------------------------------------------------

Unpaved_Riders_2023 <- tblHistory %>% 
  filter(EventYear == 2023) %>% 
  filter(Participant == 1) %>% 
  filter(EventName == "Unpaved") %>% 
  filter(Virtual == 0)

Male_Female <- tblMain2023 %>% 
  filter(!is.null(MainType)) %>% 
  select(Main_ID, Gender)

Unpaved_Male_Female <- Unpaved_Riders_2023 %>% 
  left_join(Male_Female, by = "Main_ID")

#Calculation for percentages

Unpaved_Male_Female %>% 
  group_by(Gender) %>% 
  summarize(count= n()) %>% 
  mutate(pct = (count/sum(count)) *100) %>% 
  arrange(desc(pct))



# AverageRaised -----------------------------------------------------------

Average_Rider_Raised_Unpaved <- tblHistory %>% 
  filter(EventYear == 2023) %>% 
  filter(EventName == "Unpaved") %>% 
  filter(Participant == 1) %>% 
  select(Main_ID, Raised) %>%
  filter(Raised > 0) %>%
  distinct(Main_ID, .keep_all = TRUE) %>% 
  summarize(mean = mean(Raised),
            sum = sum(Raised))



# Volunteers --------------------------------------------------------------


Unpaved_Volunteers_2023 <- tblHistory %>% 
  filter(EventYear == 2023) %>% 
  filter(EventName == "Unpaved") %>% 
  filter(Volunteer == 1) %>% 
  filter(Virtual == 0) %>% 
  summarize(count = n())
