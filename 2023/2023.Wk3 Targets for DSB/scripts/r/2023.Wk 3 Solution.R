# PREPPIN' DATA | 2023 | Week 3 | Targets for DSB  | Rstats Solution
# https://preppindata.blogspot.com/2023/01/2023-week-3-targets-for-dsb.html
#
# Solution prepared by Darragh Murray.
# Last updated: 24 January


# INSTALL PACKAGES (if required)
install.packages("tidyverse")
install.packages("readr")

# INITIALISE LIBRARIES
library(tidyverse)
library(stringr) # for string manipulation
library(lubridate)

# IMPORT DATA
targets <- read_csv("2023/2023.Wk3 Targets for DSB/data/Targets.csv")
transactions <- read_csv("2023/2023.Wk3 Targets for DSB/data/PD 2023 Wk 1 Input.csv", 
                         col_types = cols(`Online or In-Person` = col_character())) #import as a character as we're going to change this later.
  
  
# TIDY DATA

# TRANSACTIONS
# For the transactions file:
#   Filter the transactions to just look at DSB (help) - These will be transactions that contain DSB in the Transaction Code field
#   Rename the values in the Online or In-person field, Online of the 1 values and In-Person for the 2 values
#   Change the date to be the quarter (help)
#   Sum the transaction values for each quarter and for each Type of Transaction (Online or In-Person) (help)

transactions_filtered <- transactions %>%
  filter(substr(`Transaction Code`,1,3)=="DSB") %>% #filter for DSB by analysing first three letters of each transaction code
  mutate(`Online or In-Person`= recode(`Online or In-Person`, 
                  `1`="Online",
                  `2`="In-Person")) %>%
  mutate(`Quarter` = quarter(`Transaction Date`, with_year = FALSE)) %>% #lubridate will change dates to quarters with ease!
  group_by(`Quarter`, `Online or In-Person`) %>% # group our data by quarters and online/in-person
  summarise(Value = sum(Value)) # sum up transaction values by our group!

# TARGETS
#   For the targets file:
#   Pivot the quarterly targets so we have a row for each Type of Transaction and each Quarter (help)
#   Rename the fields
#   Remove the 'Q' from the quarter field and make the data type numeric (help)


targets_filtered <- targets %>%
  pivot_longer(!`Online or In-Person`, names_to = "Quarter", values_to = "Quarterly Target") %>% # Turn our targets dataframe from wide to long AND rename fields
  mutate(Quarter = as.character(substr(Quarter,2,2))) #remove first character of the quarter field as coerce to a string

  
# JOIN DATASETS
transaction_v_target <- merge(transactions_filtered, targets_filtered, by=c("Online or In-Person","Quarter")) %>% #Join dataframes on common fields
  mutate(`Variance to Target` = Value - `Quarterly Target`)


# Output data - write a new CSV file(s) to the outputs directory.
write.csv(transaction_v_target, "2023/2023.Wk3 Targets for DSB/outputs/progress_to_quarterly_targets.csv", row.names=FALSE)
