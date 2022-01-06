# PREPPIN' DATA | 2022 | Week 1 | The Prep School - Parental Contact Details 
# https://preppindata.blogspot.com/2022/01/2022-week-1-prep-school-parental.html
#
# Solution prepared by Darragh Murray.
# Last updated: 6 January 2021

# INSTALL PACKAGES (if required)
install.packages("tidyverse")
install.packages("readr")

# INITIALISE LIBRARIES
library(tidyverse)
library(readr) # for data import
library(lubridate) # for data operations

# IMPORT DATA
parental_contact_details <- read_csv("2022/2022.Wk1 Parental Contact Details/data/PD 2022 Wk 1 Input - Input.csv")

# TIDY DATA

# 1. Form the pupil's name correctly for the records in the format Last Name, First Name
# Lets use str_c from the stringr package to do the concatenation (used within Tidyverse) - we will join together the last name and first
# name with a comma character between the two strings. 

parental_contact_details$pupils_name <- str_c(parental_contact_details$`pupil last name`, ", ", 
                                                  parental_contact_details$`pupil first name`) # 

# 2. Do the same as step 1 but this time for the parental contact
parental_contact_details$parental_contact_full_name <- str_c(parental_contact_details$`pupil last name`, ", ", 
                                                  parental_contact_details$`Parental Contact Name_1`) 

# 3. Create the email address to contact the parent using the format Parent First Name.Parent Last Name@Employer.com
# This is pretty much the same approach at the previous two steps - concantenation of existing columns into a new column

parental_contact_details$parental_contact_email <- str_c(parental_contact_details$`pupil last name`, ".", 
                                      parental_contact_details$`Parental Contact Name_1`,"@",
                                      parental_contact_details$`Preferred Contact Employer`,".com")

# 4. Create the academic year the pupils are in (help)
# Each academic year starts on 1st September.
# Year 1 is anyone born after 1st Sept 2014 
# Year 2 is anyone born between 1st Sept 2013 and 31st Aug 2014 etc

# first we must tell R what the data format of the 'date of birth' field is in the dataset using the strptime function and push that to 
# a new column
parental_contact_details$standard_dob <- strptime(as.character(parental_contact_details$`Date of Birth`), "%m/%d/%Y")

# Once we have the date of birth in an R-readable format, we create YEAR and MONTH variables so we can then determine
# what academic year the pupil belongs in. This is achieved using MUTATE from Tidyverse - which essentially creates new variables.
parental_contact_details <- mutate(parental_contact_details, dob_year = year(standard_dob)) %>%
  mutate(parental_contact_details, dob_month = month(standard_dob))

# Now that we have the dob year, we can create an index which calculates based on the dob_month value
# If the month is between January and August, we take the DOB year and -1, else it stays the same.
# We use mutate again to create the year_adjustment and then generate a birth_year_class.
# Given that 2014 is year 1, according to the spec, 2015 is year 0. We can do some simple maths to generate academic_year
# Specifically minus the birth_year_class from 2015 to get the new variable (using mutate again)

parental_contact_details <- mutate(parental_contact_details, year_adjustment = ifelse(dob_month <=8,-1,0),
                                     birth_year_class = dob_year + year_adjustment, academic_year = 2015-birth_year_class)

# 5. Use the SELECT() function to select the needed columns as per specification
parental_contact_details_output <- select(parental_contact_details, academic_year, pupils_name, parental_contact_full_name, parental_contact_email)

# 6. Output data - write a new CSV file to the outputs directory.
write.csv(parental_contact_details_output, "2022/2022.Wk1 Parental Contact Details/outputs/updated_parental_contact_details.csv")



