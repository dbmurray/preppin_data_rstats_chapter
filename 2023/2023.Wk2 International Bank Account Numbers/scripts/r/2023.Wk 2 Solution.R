# PREPPIN' DATA | 2023 | Week 2 | International Bank Account Number | Rstats Solution
# https://preppindata.blogspot.com/2023/01/2023-week-2-international-bank-account.html
#
# Solution prepared by Darragh Murray.
# Last updated: 15 January


# INSTALL PACKAGES (if required)
install.packages("tidyverse")
install.packages("readr")

# INITIALISE LIBRARIES
library(tidyverse)
library(stringr) # for string manipulation

# IMPORT DATA
transactions <- read_csv("2023/2023.Wk2 International Bank Account Numbers/data/Transactions.csv")
swift_codes <- read_csv("2023/2023.Wk2 International Bank Account Numbers/data/Swift Codes.csv")
  
  
# TIDY DATA

# 1. Remove dashes in transaction swift codes

transactions_merged <- merge(transactions,swift_codes,by=c("Bank")) 

transactions_by_IBAN <- transactions_merged %>%
  mutate(`Sort Code` = str_remove_all(`Sort Code`, "-")) %>% # remove all dashes
  mutate(`Country Code` = "GB", #add the country code field in 
         IBAN = str_c(`Country Code`,`Check Digits`,`SWIFT code`,`Sort Code`,`Account Number`)) %>% #concatenate into IBAN
  select(`Transaction ID`,`IBAN`) # select the required columns

# All the above can be simplified. Because the addition the country column, only then to remove it at the end, is
# redundant, we can basically can cut down the code

transactions_by_IBAN_alternative <- transactions_merged %>%
  mutate( IBAN = str_c("GB",`Check Digits`,
                       `SWIFT code`,
                       str_remove_all(`Sort Code`, "-"),
                       `Account Number`)) %>% # concatenate into IBAN
  select(`Transaction ID`,`IBAN`) # select the required columns

# 2. Output data - write a new CSV file(s) to the outputs directory.
write.csv(transactions_by_IBAN, "2023/2023.Wk2 International Bank Account Numbers/outputs/total_transaction_by_IBAN.csv", row.names=FALSE)
