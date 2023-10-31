# Welcome back, Jennifer! If he's able to today, I'd like to ask Jeremy (copied) to analyze the following data set: 2023 Registered Riders (a query of which I have attached) but with the 2022 Route added as a field. What I'm trying to learn is:
# What was the average fundraising amount by route in 2023?
# What was the median fundraising amount by route in 2023?
# Of those 2023 riders who Reimagined in 2022, what routes did they ride in 2023?
# How much did those 2022 Reimagined riders raise, on average, in 2022?
# By route, how much did the 2022 Reimagined riders raise in 2023?
# 
# 

library(tidyverse)
library(readxl)
library(writexl)



# load --------------------------------------------------------------------


Reg_Riders23 <- read_csv("Desktop/PMC Github/2023 Reg riders 36 .csv",col_types = cols(`2023 $ Raised` = col_number(),    `2022 $ Raised` = col_number(), `Lifetime Raised` = col_number()),skip = 4)
X2022_Main_ID_and_Route <- read_csv("Desktop/PMC Github/RiderReg23/2022 Main ID and Route.csv") %>% 
  rename(Main_id = Main_ID)

allTimeEgiftandMain <- read_csv("Desktop/PMC Github/RiderReg23/allTimeEgiftandMain.csv")
rm(all_egift_id)
# Average/ Median Fundraising Amount by Route 2023 --------------------------------

Avg_by_route <- Reg_Riders23 %>% 
  group_by(`2023 Route`) %>%
  summarize(`2023 Avg Fundraising` = mean(`2023 $ Raised`, na.rm = TRUE),
            `2022 Avg Fundraising` = mean(`2022 $ Raised`, na.rm = TRUE),
            `2023 Median Fundraising` = median(`2023 $ Raised`, na.rm = TRUE),
            `2022 Median Fundraising` = median(`2022 $ Raised`, na.rm = TRUE)
            )



# # Of those 2023 riders who Reimagined in 2022, what routes did they ride in 2023 --------

#join Egift for 2022 route


egift_2022_routes <- inner_join(allTimeEgiftandMain, X2022_Main_ID_and_Route, by = "Main_id")



egift_2022_routes <- egift_2022_routes %>% 
  rename(`2022 Route` = Description) %>% 
  select(egiftID, `2022 Route`)

#how many in 2022 reimagined (corrupted))
egift_2022_routes %>% 
  group_by(`2022 Route`) %>% 
  summarize(count = n())

Reg_Riders23 <- Reg_Riders23 %>% 
  rename(egiftID =eGiftID ) %>% 
  left_join(egift_2022_routes, by ="egiftID")

reimagined_route_2022 <- Reg_Riders23 %>%
  filter( `2022 Route` == "Reimagined" | `2022 Route` == "Reimagined Teen")




routes_ridden<- Reg_Riders23 %>%
  filter(egiftID %in% reimagined_route_2022$egiftID) %>%
  distinct(egiftID, `2023 Route`) %>% 
  group_by(`2023 Route`) %>% 
  summarize(Count_by_Route = n())


# # How much did those 2022 Reimagined riders raise, on average, in 2022 --------

Avg_Reim_raised2022 <- Reg_Riders23 %>% 
  filter(`2022 Route` == "Reimagined" | `2022 Route`== "Reimagined Teen") %>% 
  group_by(`2022 Route`) %>% 
  summarize(`2022 Reimagined Avg Fundraising`= mean(`2022 $ Raised`, na.rm = TRUE),
            `2022 Reimagined Median Fundraising` = median(`2022 $ Raised`, na.rm = TRUE))

Avg_Reim_raised2023 <- Reg_Riders23 %>% 
  filter(egiftID %in% reimagined_route_2022$egiftID) %>% 
  group_by(`2023 Route`) %>% 
  summarize(`Average Fundraising 2023 (Reimagined 2022)` = mean(`2023 $ Raised`),
            `Median Fundraising 2023 (Reimagined 2022)` = median(`2023 $ Raised`),
            `Total` = sum(`2023 $ Raised`))
  


# create sheet ------------------------------------------------------------

write_xlsx(
  path = "Desktop/PMC Github/RiderReg23/JarrettOct30.xlsx", # Specify the path and filename for the Excel file
  sheets = c(
    Fundraising_Average_and_Median_by_Route_2023 = Avg_by_route,   # Define the sheet name and the data frame
    Reimagined_RoutesRidden_2023 = routes_ridden,
    Avg_Median_Raised_Reimagined_2022 = Avg_Reim_raised2022,
    By_Route_Raised_2023_From_2022_Reimagined = Avg_Reim_raised2023
  )
)

write_xlsx(
    list("Fundraising_Average_and_Median_by_Route_2023" = Avg_by_route,   # Define the sheet name and the data frame
    "Reimagined_RoutesRidden_2023" = routes_ridden,
    "Avg_Median_Raised_Reimagined_2022" = Avg_Reim_raised2022,
    "By_Route_Raised_2023_From_2022_Reimagined" = Avg_Reim_raised2023),
    "Desktop/PMC Github/RiderReg23/JarrettOct30.xlsx",
  )

