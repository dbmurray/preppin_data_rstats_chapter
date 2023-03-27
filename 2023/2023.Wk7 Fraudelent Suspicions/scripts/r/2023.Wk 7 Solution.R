# PREPPIN' DATA | 2023 | Week 7 | Flagging Fraudulent Suspicions  | Rstats Solution
# https://preppindata.blogspot.com/2023/02/2023-week-7-flagging-fraudulent.html
#
# Solution prepared by Darragh Murray.
# Last updated: 20 February 2023

# INSTALL PACKAGES (if required)
install.packages("tidyverse")

# INITIALISE LIBRARIES
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# IMPORT DATA
dsb_customer_survey <- read_csv("2023/2023.Wk6 DSB Ratings/data/DSB Customer Survery.csv")

# TIDY DATA

dsb_customer_survey_tidy <- dsb_customer_survey %>%
  pivot_longer(!`Customer ID`, names_to = "Preference", values_to = "Total") %>% # change orientation of data
  mutate(Platform = str_trim(word(`Preference`,1,sep = "\\-"))) %>% 
  mutate(Feature = str_trim(word(`Preference`,2,sep = "\\-"))) %>% # Splits the preference string out and trims all whitespace
  filter(Feature != "Overall Rating") %>% # remove overall rating
  group_by(`Customer ID`, Platform) %>% # group up data to calculate averages
  mutate(`Average Platform Rating` = mean(Total)) %>% # calculate average per group 
  distinct(`Customer ID`, Platform, `Average Platform Rating`) %>% # distinct values for required columns
  pivot_wider(names_from = Platform, values_from = `Average Platform Rating`) %>%
  mutate(platform_difference = `Mobile App` - `Online Interface`) %>%
  mutate(Preference = if_else(abs(platform_difference)>=0 && abs(platform_difference)<1,
                                     "Neutral",
                                     if_else(platform_difference<=-2,
                                             "Online Interface Superfan",
                                             if_else(platform_difference>=2,
                                                     "Mobile App Superfan",
                                                     if_else(platform_difference>=1,
                                                             "Mobile App Fan","Online Interface Fan"))))) %>%
  group_by(Preference) %>% # group data by platform preferemce
  summarise(customer_count = n()) %>% # each row is a customer, so count rows
  mutate(`% of Total` = round((customer_count/sum(customer_count)*100),1)) %>% # calculate the proportions
  select(Preference, `% of Total`) # select the correct output columns

# Output data - write a new CSV file(s) to the outputs directory.
write.csv(dsb_customer_survey_tidy, "2023/2023.Wk6 DSB Ratings/outputs/dsb_customer_survey.csv", row.names=FALSE)
