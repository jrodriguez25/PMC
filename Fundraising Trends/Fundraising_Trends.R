library(tidyverse)
library(readxl)


tblMain <- read_excel("tblMain.xlsx")


# Question 1  -------------------------------------------------------------


colnames(tblMain)


years <- data.frame(EventYear=1980:2023)

Num_YR_riding <- tblMain %>%
  filter(Participant == 1) %>%
  filter(EventName == "PMC") %>%
  select(Main_ID, EventYear, Collected)


median_mean_df 
# 
# median_mean_df <- Num_YR_riding %>% 
#   group_by(Main_ID, EventYear) %>% 
#   mutate(years_ridden_up_to_now = n_distinct(filter(Num_YR_riding, Main_ID == first(Main_ID) & EventYear <= first(EventYear))$EventYear)) %>% 
#   ungroup() %>% 
#   
#   right_join(years, by = "EventYear") %>% 
#   
#   group_by(EventYear) %>% 
#   summarize(
#     median_years_ridden = median(years_ridden_up_to_now, na.rm = TRUE), 
#     mean_years_ridden = mean(years_ridden_up_to_now, na.rm = TRUE)) %>% 
#   arrange(EventYear)
# 
# print(median_mean_df)
#   

results <- Num_YR_riding %>%
  # For each Main_ID and EventYear, compute the number of prior years the person has participated
  group_by(Main_ID, EventYear) %>%
  summarise(prior_years = sum(Num_YR_riding$EventYear[Num_YR_riding$Main_ID == Main_ID & Num_YR_riding$EventYear < EventYear])) %>%
  # Now, group by EventYear to compute the average of the counts
  group_by(EventYear) %>%
  summarise(average_years_ridden = mean(prior_years, na.rm = TRUE))

print(results)




