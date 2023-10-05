library(tidyverse)
library(readxl)


tblMain <- read_excel("tblMain.xlsx")


# Question 1 --------------------------------------------------------------

#What is the median AND mean (avg) number of years riding of all riders since 1980 by year


num_years_per_rider <- tblMain %>% 
  group_by(Main_ID) %>% 
  summarize(num_YR_riding = n()) %>% 
  ungroup()


#Checked Billy for Accuracy
num_years_per_rider %>% 
  filter(Main_ID== "02409-9")

#Average Number of Years Riding

mean(num_years_per_rider$num_YR_riding) # = 3.794596

#Median "
median(num_years_per_rider$num_YR_riding) # = 2



# Question 2 --------------------------------------------------------------

#What is the median AND mean (avg) number of years riding of all riders who rode =>2 year since 1980 by year

tblMain <- tblMain %>% 
  left_join(num_years_per_rider, by = "Main_ID")

