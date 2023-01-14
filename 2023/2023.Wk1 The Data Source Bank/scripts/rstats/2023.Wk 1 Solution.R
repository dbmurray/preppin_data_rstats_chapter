# PREPPIN' DATA | 2023 | Week 1 | The Data Source Bank | Rstats Solution
# https://preppindata.blogspot.com/2023/01/2023-week-1-data-source-bank.html
#
# Solution prepared by Darragh Murray.
# Last updated: 5 January


# INSTALL PACKAGES (if required)
install.packages("tidyverse")
install.packages("readr")

# INITIALISE LIBRARIES
library(tidyverse)
library(readxl) # for data import
library(stringr) # for string manipulation

# IMPORT DATA
data_source_bank <-read_csv("2023/2023.Wk1 The Data Source Bank/data/PD 2023 Wk 1 Input.csv", 
                            col_types = cols(Value = col_integer(), 
                                             `Customer Code` = col_integer(), 
                                             `Online or In-Person` = col_character(),
                                             `Transaction Date` = col_datetime(format = "%d/%m/%Y %H:%M:%S")))

# TIDY DATA

data_source_bank_tidy <- data_source_bank %>%
  rename(transaction_date_time = `Transaction Date`) %>% # renaming as this makes it a bit easier later.
  mutate(Bank = word(`Transaction Code`,1,sep = "\\-")) %>% # Splits out bank code from transaction code
  mutate(`Online or In-Person` = 
           if_else(`Online or In-Person`=="1", "Online", "In-Person")) %>% # Conditionally recodes the Online/In Person variable
  mutate(`Transaction Date` = weekdays(transaction_date_time) ) # Convert transaction date to the day of the week

# The rest of the challenge is straight forward using group_by and summarise
  
# Prep output 1 - Total Values of Transactions by each bank

transactions_by_bank <- data_source_bank_tidy  %>%
  group_by(Bank) %>%
  summarise(Value = sum(Value))


# Prep output 2 - Total Values by Bank, Day of the Week and Type of Transaction
transactions_by_bank_day_type <- data_source_bank_tidy  %>%
  group_by(Bank,`Transaction Date`, `Online or In-Person`) %>%
  summarise(Value = sum(Value))

# Prep output 3 - Total Values by Bank and Customer Code
transactions_by_bank_customer_code <- data_source_bank_tidy  %>%
  group_by(Bank, `Customer Code`) %>%
  summarise(Value = sum(Value))


# 2. Output data - write a new CSV file(s) to the outputs directory.
write.csv(transactions_by_bank, "2023/2023.Wk1 The Data Source Bank/outputs/total_transaction_by_bank.csv", row.names=FALSE)
write.csv(transactions_by_bank_day_type, "2023/2023.Wk1 The Data Source Bank/outputs/total_transaction_by_bank_day_type.csv", row.names=FALSE)
write.csv(transactions_by_bank_customer_code, "2023/2023.Wk1 The Data Source Bank/outputs/total_transaction_by_bank_customer_code.csv", row.names=FALSE)
