

# Load Libraries ----------------------------------------------------------


library(tidyverse)
library(readxl)
library(writexl)



# Read In Data ------------------------------------------------------------


raw_tracking <- read_csv("Desktop/PMC Github/PMC Raw Data.csv", 
                         col_names = FALSE)
raw_tracking <- raw_tracking %>% 
  rename(Bib_Tag = X1, eGift_ID = X2, Passing_ID = X3, Location_In = X4,  Time = X5)


reg_TY <-  read_csv("Desktop/PMC Github/KPI Dashboard/RidersRegTY.csv") %>% 
  select(eGiftID, `2023 Route`) %>% 
  rename(eGift_ID= eGiftID)




# Assign corresponding locations ------------------------------------------


mapping_dict <- c(
  "SS1" = "Sturbridge Start",
  "WEL-out-Sat" = "Wellesley Saturday Start",
  "MMA-in" = "Bourne Finish",
  "W01-in" = "Whitinsville",
  "W01-out" = "Whitinsville",
  "W02-in" = "Franklin",
  "W02-out" = "Franklin",
  "W03-in-Sat" = "Dighton-Rehoboth Saturday",
  "W03-out-Sat" = "Dighton-Rehoboth Saturday",
  "W04-in" = "Lakeville",
  "W04-out" = "Lakeville",
  "W05-In" = "Wareham",
  "W05-out" = "Wareham",
  "W09-in-Sat" = "Wrentham Saturday",
  "W09-out-Sat" = "Wrentham Saturday",
  "BB" = "Bourne Start",
  "WEL-Out-Sun" = "Wellesley Sunday Start",
  "WEL-in" = "Wellesley Finish",
  "PTI-In" = "Provincetown Inn",
  "FAM-in" = "Provincetown Monument (Family Finish)",
  "W06-in" = "Barnstable",
  "W06-out" = "Barnstable",
  "W07-in" = "Brewster",
  "W07-out" = "Brewster",
  "W08-in" = "Wellfleet",
  "W08-out" = "Wellfleet",
  "W11-in" = "Foxboro/Patriot Place",
  "W12-in" = "Medfield",
  "W12-out" = "Medfield",
  "W13-in" = "Walpole",
  "W13-out" = "Walpole",
  "W15-In" = "Taunton",
  "W15-Out" = "Taunton",
  "W03-in-Sun" = "Dighton-Rehoboth Sunday",
  "W03-out-Sun" = "Dighton-Rehoboth Sunday",
  "W09-in-Sun" = "Wrentham Sunday",
  "W09-out-Sun" = "Wrentham Sunday"
)



raw_tracking <- raw_tracking %>% 
  mutate(location_cleaned = case_when(
    Location_In %in% names(mapping_dict) ~ mapping_dict[Location_In],
    TRUE ~ NA_character_
  ))


# Create indicator of time in and time out --------------------------------


raw_tracking <- raw_tracking %>% 
  mutate(
    time_indicator = if_else(grepl("in", Location_In), "in", "out", missing = ""))
    


# clean location name -----------------------------------------------------

raw_tracking <- raw_tracking %>% 
  mutate(location_cleaned = if_else(!is.na(location_cleaned), paste(location_cleaned, time_indicator, sep = "-"), NA_character_))


raw_tracking <- raw_tracking %>% 
  mutate(location_cleaned = gsub("-in|-out", "", location_cleaned))

# tally net rider ---------------------------------------------------------


raw_tracking <- raw_tracking %>%
  mutate(Tally = ifelse(time_indicator == "in", 1, -1))

# remove duplicates per rider per location --------------------------------

raw_tracking2.0 <- raw_tracking %>%
  distinct(eGift_ID, location_cleaned, time_indicator, .keep_all = TRUE)



# Join with route information ---------------------------------------------

raw_tracking2.0<- inner_join(raw_tracking2.0, reg_TY, by = "eGift_ID")

raw_tracking3.0 <- raw_tracking2.0 %>%
  mutate(Route_Start = case_when(
    grepl("^Wellesley", `2023 Route`) ~ "Wellesley",
    grepl("^Sturbridge", `2023 Route`) ~ "Sturbridge",
    TRUE ~ NA_character_
  ))


raw_tracking4.0 <- raw_tracking3.0 %>% 
  mutate(Route_Start=case_when(`2023 Route` %in% c("Wellesley Century", "Wellesley to Patriot Place Teens (25 mile Sun)", 
                                   "Wellesley to Wellesley Teens (50 mile Sunday)", 
                                   "Bourne to Provincetown Inn (1-Day, Sun)", 
                                   "Bourne to Provincetown Monument (1-Day, Sun)", 
                                   "Wellesley to Patriot Place (25 mile Sunday)", 
                                   "Wellesley to Wellesley (50 mile Sunday)") ~ `2023 Route`,
                               TRUE ~ Route_Start))

write_xlsx(raw_tracking4.0, "raw_tracking_cleaned.xlsx")

