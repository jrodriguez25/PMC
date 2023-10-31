library(writexl)
library(tidyverse)
library(readxl)


# Load Data ---------------------------------------------------------------


df <- read_csv("Desktop/PMC Github/Tracking Data/code/clean_tracking.csv")
route_info <- read_excel("Desktop/PMC Github/Tracking Data/data/Rider Route Info.xlsx")


tidy_df <- df %>%
  gather(key = "Event_Type", value = "Time", `Time In`, `Time Out`)


# Create new column
tidy_df <- tidy_df %>%
  mutate(Net_Riders = case_when(
    Event_Type == "Time In" ~ 1,
    Event_Type == "Time Out" ~ -1,
    TRUE ~ NA_integer_  # Default case, return NA
  )) %>% 
  rename(eGiftID= eGIFTID)


#merge on for route info for each rider
tidy_df <- tidy_df %>% 
  left_join(route_info, by = "eGiftID")


tidy_df <- tidy_df %>% 
  select(-...23,-...22,-...21,-...20,-...18,-...17,-...16,-`2023 Route...19`)

#Wellesley or Sturbridge Start 

tidy_df <- tidy_df %>% 
  mutate(Start_Location_Saturday = ifelse(
    grepl("Sat|2-Day", `2023 Route...11`) & grepl("Wellesley", `2023 Route...11`),
    "Wellesley start on Saturday",
    ifelse(
      grepl("Sat|2-Day", `2023 Route...11`) & grepl("Sturbridge", `2023 Route...11`),
      "Sturbridge start on Saturday",
      NA  # or any other value for routes that don't match the condition
    )
  ))

length(unique(tidy_df$eGiftID))


write_xlsx(tidy_df,"Desktop/PMC Github/Tracking Data/tidy_df1027.xlsx")
