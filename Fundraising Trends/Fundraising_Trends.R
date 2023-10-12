library(tidyverse)
library(readxl)
library(ggthemes)
library(ggplot2)
library(patchwork)
library(scales)
library(openxlsx)


tblMain <- read_excel("~/Desktop/PMC Github/median_mean_info/tblHistory.xlsx")

tblHistory <- read_excel("Desktop/PMC Github/Year Book 2023 Fun Facts/tblMain2023.xlsx")




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


# Number of People Riding -------------------------------------------------

Num_riders_per_year <- tblMain %>% 
  filter(Participant == 1) %>%
  filter(EventName == "PMC") %>% 
  group_by(EventYear) %>% 
  summarize(riders =n())

write.xlsx(Num_riders_per_year, "Number_Riders_Per_Year.xlsx")

Amount_fundraised_YR <- tblMain %>% 
  filter(Participant == 1) %>% 
  filter(EventName == "PMC") %>% 
  group_by(EventYear) %>% 
  summarize(total_amount_collected = sum(Collected))

write.xlsx(Amount_fundraised_YR, "Amount_Funraised_by_Year.xlsx")

plot_fundraising <- ggplot(fundraising_long, aes(x = EventYear, y =Median_value, color = Category))+
  geom_line(size =1) +
  labs(
    title = "Comparison of Median Fundraised across Categories",
    x = "Event Year",
    y = "Median Fundraised",
    color = "Category"
  ) +
  scale_color_discrete(labels=c("2+ year Rider", "3+ Year Rider", "All Riders"))+
  theme_linedraw()

plot_fundraising

#Comparison of Number of Riders and Total Fundraised

Amount_fundraised_YR <- Amount_fundraised_YR %>% left_join(Num_riders_per_year, by = "EventYear")

riders<- ggplot(Amount_fundraised_YR, aes(x = EventYear, y =riders )) +
  geom_bar(stat = "identity", fill = "skyblue")+
  labs(x = "Year", y = "Riders", title = "Number of Riders Each Year")+
  theme_linedraw()

fundraising <- ggplot(Amount_fundraised_YR, aes(x= EventYear )) +
  geom_line(aes(y =total_amount_collected), size = 1) +
  scale_y_continuous(labels = dollar)+
  labs(x = "Year", y = "Amount Raised", title = "Total Raised Each Year" )+
  theme_linedraw()

#Plots 
fundraising + riders




# Percentage Raised above the minimum -------------------------------------

pct_above_minimum <- tblMain %>% 
  filter(Participant == 1) %>% 
  filter(EventName == "PMC") %>% 
  select(Main_ID, EventYear, Collected,Raised, MinFund, Commitment) %>% 
  mutate(pct_above_raised = ((Raised-Commitment)/ Commitment) *100)

pct_above_minimum <- pct_above_minimum%>%   
  mutate(pct_above_raised = case_when(
  MinFund == 0 & Commitment == 0 ~ NA,
  MinFund != 0 & Commitment == 0 ~ ((Raised-MinFund)/ MinFund) *100))

write


#Maybe start thinking of certain routes and certain cohorts of riders

mark_OnD <- tblMain %>% 
  filter(Participant == 1) %>% 
  filter(EventName == "PMC") %>% 
  group_by(Main_ID) %>% 
  mutate(appear_count = n()) %>% 
  ungroup() %>% 
  mutate(One_Timer = ifelse(appear_count == 1, "Yes", "No")) %>% 
  select(Main_ID, EventYear, Collected, Raised, MinFund, Commitment, One_Timer)



pct_above_minimum <-bind_rows(pct_above_minimum,  mark_OnD)

pct_above_minimum_OND<- pct_above_minimum %>% 
  filter(One_Timer == "Yes")


  
  
# One and Dones -----------------------------------------------------------

One_and_Dones <-tblMain %>% 
  filter(Participant == 1) %>% 
  filter(EventName == "PMC") %>% 
  group_by(Main_ID) %>%
  filter(n() == 1) %>% 
  group_by(EventYear) %>% 
  summarize(count =n())

write.xlsx(One_and_Dones, "One and Dones Graph.xlsx")


OnD_graph <- ggplot(One_and_Dones, aes(x = EventYear, y= count))+
  geom_bar(stat = "identity", fill = 'darkred') +theme_linedraw() +
  labs(x = "Year", y = "Number of Riders", title = "Number of One and Dones each Year")

OnD_graph





# Visualize ---------------------------------------------------------------


#Comparison of Median Fundraised Across Categories
fundraising <- ggplot(median_mean_Totalfundraised_per_year, aes(EventYear, median_lifetime_fundraising))+
  geom_line()

fundraising_2plus <- ggplot(median_mean_Totalfundraised_per_year_2plus, aes(EventYear, median_lifetime_fundraising))+
  geom_line()

fundraising_3plus <- ggplot(median_mean_Totalfundraised_per_year_3Plus, aes(EventYear, median_lifetime_fundraising))+
  geom_line()

fundriasing_overall_list <- list(median_mean_Totalfundraised_per_year, median_mean_Totalfundraised_per_year_2plus, median_mean_Totalfundraised_per_year_3Plus)

fundriasing_overall_list <- fundriasing_overall_list %>% reduce(full_join, by ="EventYear")

fundriasing_overall_list <- fundriasing_overall_list %>% 
  rename(median_overall = median_lifetime_fundraising.x,
         median_2plus = median_lifetime_fundraising.y,
         median_3plus = median_lifetime_fundraising)

fundraising_long <- fundriasing_overall_list %>% 
  gather(key = "Category", value = "Median_value", -EventYear ) %>% 
  filter(Category != "average_lifetime_fundraising.x") %>% 
  filter(Category != "average_lifetime_fundraising.y") %>% 
  filter(Category != "average_lifetime_fundraising")

clean_table <- Num_riders_per_year %>% left_join(fundraising_long, by = "EventYear")
# Correlation rider & fundraising amount -------------------------------------------------------------------

model <- lm(Median_value ~ riders, data = clean_table)

summary(model)


# Visualizations  ---------------------------------------------------------




