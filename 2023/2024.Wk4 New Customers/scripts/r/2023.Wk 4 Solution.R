# PREPPIN' DATA | 2023 | Week 4 | Targets for DSB  | Rstats Solution
# https://preppindata.blogspot.com/2023/01/2023-week-4-new-customers.html
#
# Solution prepared by Darragh Murray.
# Last updated: 8 February 2023

# INSTALL PACKAGES (if required)
install.packages("tidyverse")
install.packages("rio")

# INITIALISE LIBRARIES
library(dplyr)
library(rio)
library(lubridate)

# IMPORT DATA

# using the rio package we can import all worksheets in the excel file into a tibble and row bind them.
# dply allows us to bind rows, using the sheet name as a column
new_customers <- bind_rows(
  import_list("2023/2024.Wk4 New Customers/data/New Customers.xlsx", setclass = "tbl"),
  .id = "sheet"
)

# TIDY DATA

# there are obviously some sheets that had mispelled the word demographic. We can tidy this up using dplyr
# fairly easily. 

new_customers$Demographic <- ifelse(!(is.na(new_customers$Demographiic)), new_customers$Demographiic, new_customers$Demographic )
new_customers$Demographic <- ifelse(!(is.na(new_customers$Demagraphic)), new_customers$Demagraphic, new_customers$Demographic )

# The rest of the requirements are fariyl straight forward. We simply create a new date field using
# the fields we have, then pivot the table from long to wide and we arrive at our solution

new_customers <- new_customers %>%
  mutate(`Joining Date` = make_date(2023, match(sheet, month.name), `Joining Day`)) %>% # Use lubridate and match function to create a nice date!
  select(-c('Demographiic', 'Demagraphic',`Joining Day`, `sheet`)) %>% # remove unneeded columns
  pivot_wider(names_from = `Demographic`, values_from = Value) # switch the table orientation from long to wide


# Output data - write a new CSV file(s) to the outputs directory.
write.csv(new_customers, "2023/2024.Wk4 New Customers/outputs/new_customers.csv", row.names=FALSE)
