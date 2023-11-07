library(tidyverse)
library(writexl)
library(readxl)




# Load Packages -----------------------------------------------------------


routes_2022 <- read_excel("Desktop/PMC Github/RiderReg23/Jarrett_StatsbyRouteStart.xlsx", 
                          sheet = "2022 Routes Ridden")

rotues_2023 <- read_excel("Desktop/PMC Github/RiderReg23/Jarrett_StatsbyRouteStart.xlsx", 
                          sheet = "2023 Routes Ridden")

year_started <- read_excel("Desktop/PMC Github/RiderReg23/Jarrett_StatsbyRouteStart.xlsx", 
                           sheet = "MainID & Year Started")

all_egift_mainID <- read_excel("Desktop/PMC Github/RiderReg23/Jarrett_StatsbyRouteStart.xlsx", 
                               sheet = "allEgiftID & Main ID")


gender <- read_excel("Desktop/PMC Github/RiderReg23/Jarrett_StatsbyRouteStart.xlsx", 
                     sheet = "Gender")

living_proof <- read_excel("Desktop/PMC Github/RiderReg23/Jarrett_StatsbyRouteStart.xlsx", 
                           sheet = "Living Proof")

regDate_and_status <- read_excel("Desktop/PMC Github/RiderReg23/Jarrett_StatsbyRouteStart.xlsx", 
              sheet = "2023 reg date")



# Merge Data --------------------------------------------------------------

merged_data <- rotues_2023 %>%
  left_join(year_started, by = "Main_ID") %>%
  left_join(all_egift_mainID, by = "Main_ID") %>% 
  left_join(gender, by = "Main_ID") %>% 
  left_join(living_proof, by = "Main_ID") %>% 
  left_join(regDate_and_status, by = "Main_ID")

#Get Necessary Columns

merged_data <- merged_data %>% 
  select(-egiftID.x, -egiftID.y, -EventYear.x)



# Aggregate the Data ------------------------------------------------------

merged_data<- merged_data %>% 
  mutate(`Tenure`= 2023 - YearStarted+1) %>% 
  mutate(
    # Create the "Route Start" column
    `Route Start` = case_when(
      str_detect(Description, "2-Day") & str_detect(Description, "Sturbridge") ~ "Sturbridge Start",
      str_detect(Description, "2-Day|Sat") & str_detect(Description, "Wellesley") ~ "Wellesley Start",
      str_detect(Description, "Wellesley") & str_detect(Description, "Sunday") ~ "Babson Sunday",
      TRUE ~ NA_character_
    ),
    
    `Route Finish` = case_when(
      str_detect(Description, "Provincetown Inn") ~ "Provincetown Inn Finish",
      str_detect(Description, "Provincetown Monument") ~ "Provincetown Monument Finish",
      TRUE ~ NA_character_
    ),
    
    `Saturday only` = ifelse(str_detect(Description, "1-Day, Sat"), "Saturday Only", NA_character_),
    
    # Create the "Reimagined" and "Virtual" columns
    Reimagined = ifelse(Description == "Reimagined", "Reimagined", NA_character_),
    Virtual = ifelse(Description == "Virtual Ride", "Virtual", NA_character_),
  
    ) %>% 
  mutate(TeamID = ifelse(grepl("^\\d+$", TeamID), as.numeric(TeamID), NA))

unique(merged_data$Description)
    


# Count the number of non-null values in the field

# Starts ------------------------------------------------------------------


aggregate_start_routes <- merged_data %>% 
  group_by(`Route Start`) %>% 
  summarize(`Number of Riders` = n(),
            `Average Raised` = mean(FundraisedAmount),
            `Median Raised` = median(FundraisedAmount), 
            `Average Tenure` = mean(Tenure, na.rm = TRUE),
            `Percent Living Proof` = mean(Med_Cancer, na.rm = TRUE) * 100,
            `Average Reg Date` = mean(RegDate, na.rm = TRUE),
            `Percent Male` = (sum(Gender == "M")/n())*100,
            `Percent Team` = (sum(!is.na(TeamID))/n())*100,
            `Average Year Started`= mean(YearStarted, na.rm =TRUE)
            ) %>% 
  filter(!is.na(`Route Start`)) %>% 
  rename(Route = `Route Start`)


# Finishes ----------------------------------------------------------------


aggregate_finish_routes <-merged_data %>% 
  group_by(`Route Finish`) %>% 
  summarize(`Number of Riders` = n(),
            `Average Raised` = mean(FundraisedAmount),
            `Median Raised` = median(FundraisedAmount), 
            `Average Tenure` = mean(Tenure, na.rm = TRUE),
            `Percent Living Proof` = mean(Med_Cancer, na.rm = TRUE) * 100,
            `Average Reg Date` = mean(RegDate, na.rm = TRUE),
            `Percent Male` = (sum(Gender == "M")/n())*100,
            `Percent Team` = (sum(!is.na(TeamID))/n())*100,
            `Average Year Started`= mean(YearStarted, na.rm =TRUE)
  ) %>% 
  filter(!is.na(`Route Finish`)) %>% 
  rename(Route = `Route Finish`)





# saturday Only -----------------------------------------------------------


aggregated_saturday <-merged_data %>% 
  group_by(`Saturday only`) %>% 
  summarize(`Number of Riders` = n(),
            `Average Raised` = mean(FundraisedAmount),
            `Median Raised` = median(FundraisedAmount), 
            `Average Tenure` = mean(Tenure),
            `Percent Living Proof` = mean(Med_Cancer, na.rm = TRUE) * 100,
            `Average Reg Date` = mean(RegDate, na.rm = TRUE),
            `Percent Male` = (sum(Gender == "M")/n())*100,
            `Percent Team` = (sum(!is.na(TeamID))/n())*100,
            `Average Year Started`= mean(YearStarted, na.rm =TRUE)
  ) %>% 
  filter(!is.na(`Saturday only`)) %>% 
  rename(Route = `Saturday only`)


# Reimagined --------------------------------------------------------------

aggregated_reimagined <-merged_data %>% 
  group_by(Reimagined) %>% 
  summarize(`Number of Riders` = n(),
            `Average Raised` = mean(FundraisedAmount),
            `Median Raised` = median(FundraisedAmount), 
            `Average Tenure` = mean(Tenure),
            `Percent Living Proof` = mean(Med_Cancer, na.rm = TRUE) * 100,
            `Average Reg Date` = mean(RegDate,na.rm = TRUE),
            `Percent Male` = (sum(Gender == "M")/n())*100,
            `Percent Team` = (sum(!is.na(TeamID))/n())*100,
            `Average Year Started`= mean(YearStarted, na.rm =TRUE)
  ) %>% 
  filter(!is.na(Reimagined)) %>% 
  rename(Route = Reimagined)

# Virtual -----------------------------------------------------------------

aggregate_virtual <- merged_data %>% 
  group_by(Virtual) %>% 
  summarize(`Number of Riders` = n(),
            `Average Raised` = mean(FundraisedAmount),
            `Median Raised` = median(FundraisedAmount), 
            `Average Tenure` = mean(Tenure),
            `Percent Living Proof` = mean(Med_Cancer, na.rm = TRUE) * 100,
            `Average Reg Date` = mean(RegDate, na.rm = TRUE),
            `Percent Male` = (sum(Gender == "M")/n())*100,
            `Percent Team` = (sum(!is.na(TeamID))/n())*100,
            `Average Year Started`= mean(YearStarted, na.rm =TRUE)
  ) %>% 
  filter(!is.na(Virtual)) %>% 
  rename(Route = Virtual)
  
    

# Combine the aggregate Dataframes ----------------------------------------

final_table <- bind_rows(aggregate_start_routes, aggregate_finish_routes, aggregated_saturday, aggregated_reimagined, aggregate_virtual)

# write Final table -------------------------------------------------------

write_xlsx(final_table,"Desktop/PMC Github/RiderReg23/StatsBySimplifiedRoute2.xlsx" )
