# PREPPIN' DATA | 2023 | Week 5 | DSB Rankings  | Rstats Solution
# https://preppindata.blogspot.com/2023/02/2023-week-5-dsb-ranking.html
#
# Solution prepared by Darragh Murray.
# Last updated: 19 February 2023

# INSTALL PACKAGES (if required)
install.packages("tidyverse")

# INITIALISE LIBRARIES
library(dplyr)
library(readr)
library(stringr) # for string manipulation

# IMPORT DATA
data_source_bank <-read_csv("2023/2023.Wk5 DSB Ranking/data/PD 2023 Wk 1 Input.csv", 
                            col_types = cols(Value = col_integer(), 
                                             `Customer Code` = col_integer(), 
                                             `Online or In-Person` = col_character(),
                                             `Transaction Date` = col_datetime(format = "%d/%m/%Y %H:%M:%S")))

# TIDY DATA
data_source_bank_tidy <- data_source_bank %>%
  rename(transaction_date_time = `Transaction Date`) %>% # renaming as this makes it a bit easier later.
  mutate(Bank = word(`Transaction Code`,1,sep = "\\-")) %>% # Splits out bank code from transaction code
  mutate(`Transaction Date` = months(transaction_date_time)) %>% # Change the date to month NAMES
  group_by(Bank, `Transaction Date`) %>% # To summarise values by bank and month, we need to group by first
  summarise(Value = sum(Value)) %>% # calculate the aggregate sume pe rbank and month
  ungroup() %>% #ungroup the data
  group_by(`Transaction Date`) %>% #now group by just the transaction date/month name
  mutate(`Bank Rank per Month` = order(order(Value, decreasing=TRUE))) %>% # We create a ranking variable
  ungroup() %>%
  group_by(Bank) %>% # to find out mean rank per bank and month, we group by bank
  mutate(`Avg Rank per Bank` = mean(`Bank Rank per Month`)) %>% # calculate the average rank
  ungroup() %>%
  group_by(`Bank Rank per Month`) %>% # now group by the bank rank
  mutate(`Avg Transaction Value per Rank` = mean(Value)) #calculate the average transaction value by rank..and voila!
         

# Output data - write a new CSV file(s) to the outputs directory.
write.csv(data_source_bank_tidy, "2023/2023.Wk5 DSB Ranking/outputs/dsb_ranking.csv", row.names=FALSE)
