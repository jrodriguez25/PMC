library(tidyverse)
library(readxl)
library(writexl)


# Load Files --------------------------------------------------------------

tblMain2023 <-read_excel("Desktop/PMC Github/Year Book 2023 Fun Facts/tblMain2023.xlsx", 
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
tblHistory <- read_excel("Desktop/PMC Github/Year Book 2023 Fun Facts/tblHistory.xlsx")



# Rider By State ----------------------------------------------------------

us_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
               "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
               "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
               "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
               "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")



tblMain2023_cleaned <- tblMain2023 %>% 
  select(Main_ID, eGiftID, Display, Deceased, Participant, Volunteer, DOB, Gender, Address1, City, State, CountryCode, Cancer, MainType) %>% 
  filter(MainType=="NULL")


tblHistory_cleaned <- tblHistory %>% 
  select(Main_ID, EventYear, EventName, Collected, Raised, NonFund, Donated, Participant, Volunteer, Fundraiser, MinFund, Commitment, Virtual) 

tblMain2023_cleanedIDS <- tblMain2023_cleaned %>% 
  select(Main_ID,Display, eGiftID, MainType, Deceased, DOB, Gender, Address1, City, State, CountryCode, Cancer)

tblHistory_cleaned <- tblHistory_cleaned %>% 
  left_join(tblMain2023_cleanedIDS, by = "Main_ID")

tblHistory_cleaned <- tblHistory_cleaned %>% 
  filter(!is.na(eGiftID)) %>% 
  filter(Collected >=0, Raised >= 0, NonFund >=0)




names_to_remove <- c("????????????Com, ??????", "?30?10 ??APP?28?, www.tfa188.com?50?58",
                     "(Nimmo) Palantoni, Kate", "(Sheehan) Bakaur, Patricia", 
                     "], Helena T.", "~, .", "~, A", "~, Larry", "~, Tim", "~, Toni", 
                     "~, Yim Tan (Lisa)", "~K, D", "~Sacks, Ron", 
                     "~Walden, Harry", "~Walden, Harry", "A  Connerty, Maureen", 
                     "A  Connerty, Maureen", "A Jesdale, Todd", "A Jesdale, Todd", 
                     "A Jesdale, Todd", "A Jesdale, Todd", "A Novelli, Vincenzo", 
                     "A WHITE, ROBERT", "A WHITE, ROBERT", "A WHITE, ROBERT", "A, A", 
                     "A, A", "A., Brendan", ";jlkhjkhkjh, hlhjpoj;jk")


tblHistory_cleaned <- tblHistory_cleaned %>% 
  filter(!Display %in% names_to_remove) %>% 
  filter(EventName != "pmc") %>% 
  filter(EventName != "winter-cycle-boston") %>% 
  filter(EventName != "The-Res-Boston") %>% 
  filter(EventName != "The-Res-SanFran")


# DOB sheet ---------------------------------------------------------------

Dob_df <- tblHistory_cleaned %>% 
  mutate(DOB = as.Date(DOB),  
         Age = floor(as.numeric(Sys.Date() - DOB) / 365.25)) %>% 
  mutate(Age_Group = case_when(
    Age < 20 ~ "<20",
    Age >= 20 & Age <= 29 ~ "20-29",
    Age >= 30 & Age <= 39 ~ "30-39",
    Age >= 40 & Age <= 49 ~ "40-49",
    Age >= 50 & Age <= 59 ~ "50-59",
    Age >= 60 & Age <= 69 ~ "60-69",
    Age >= 70 & Age <= 79 ~ "70-79",
    Age >= 80 ~ "80+"
  )) %>% 
  filter(Age >= 13, Age <= 100)



# Gender Sheet ------------------------------------------------------------

Gender_df <- tblHistory_cleaned %>% 
  mutate(Gender = ifelse(Gender == "N", "NULL", Gender)) %>% 
  filter(Gender != "m")







  
write_xlsx(list("tblHistory" = tblHistory_cleaned, "Age" = Dob_df, "Gender" = Gender_df), "Desktop/PMC Github/Dashboard_1027.xlsx")


