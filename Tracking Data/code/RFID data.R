library(writexl)
library(tidyverse)

df <- read_csv("Desktop/PMC Github/Tracking Data/code/clean_tracking.csv")
write_xlsx(data, "clean_tracking.xlsx")


tidy_df <- df %>%
  gather(key = "Event_Type", value = "Time", `Time In`, `Time Out`)


# Create new column
tidy_df <- tidy_df %>%
  mutate(Net_Riders = case_when(
    Event_Type == "Time In" ~ 1,
    Event_Type == "Time Out" ~ -1,
    TRUE ~ NA_integer_  # Default case, return NA
  ))

write_xlsx(tidy_df, "tidy_df.xlsx")
