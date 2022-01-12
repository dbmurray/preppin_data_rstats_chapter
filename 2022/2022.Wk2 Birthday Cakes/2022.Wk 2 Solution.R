# PREPPIN' DATA | 2022 | Week 2 | The Prep School - Birthday Cakes! 
# https://preppindata.blogspot.com/2022/01/2022-week-2-prep-school-birthday-cakes.html
#
# Solution prepared by Darragh Murray.
# Last updated: 12 January 2021

# INSTALL PACKAGES (if required)
install.packages("tidyverse")
install.packages("readr")

# INITIALISE LIBRARIES
library(tidyverse)
library(readr) # for data import
library(lubridate) # for data operations

# IMPORT DATA
student_contact_details <- read_csv("2022/2022.Wk2 Birthday Cakes/data/PD 2022 Wk 1 Input - Input.csv")

# TIDY DATA

# 1. Format the pupil's name in First Name Last Name format (ie Carl Allchin)
# Like the last challenge, lets use str_c from the stringr package to do the concatenation 
# (used within Tidyverse) - we will join together the last name and first
# name with a comma character between the two strings. 

student_contact_details$pupils_name <- str_c(student_contact_details$`pupil first name`, " ", 
                                                  student_contact_details$`pupil last name`) # 

# 2. Create the date for the pupil's birthday in calendar year 2022 (not academic year).
# 3. Work out what day of the week the pupil's birthday falls on.
#     Remember if the birthday falls on a Saturday or Sunday, we need to change the weekday to Friday
# 4. Work out what month the pupil's birthday falls within (as a string e.g 'January')
#
# Much of the answers 2-4 can be done in one large mutate call.
#
# The lubridate package will allow us to get the day and month of the pupil's date of birth.
# We can then concatenate this with the year '2022' and create a new date
# We can then extract the day number the birthday falls on using lubridate
# If the day number is 1 (Sunday) or 7 (Saturday), we can adjust to a new birthday on Friday.

student_contact_details <- mutate(student_contact_details, 
                                  birth_month = month(as.POSIXlt(`Date of Birth`, format="%m/%d/%Y")), #extract month
                                  birth_day = day(as.POSIXlt(`Date of Birth`, format="%m/%d/%Y")), #extract day
                                  this_years_birthday = as.Date(str_c("2022","-",birth_month,"-",birth_day)), #concatenate into new 2022 date
                                  birthday_2022_day_number = wday(this_years_birthday), # get the numeric date number
                                  adjust_birthday = ifelse(birthday_2022_day_number == 1, 2, 
                                                           ifelse(birthday_2022_day_number == 7, 1, 0)), # determine the amount of days to adjust
                                  cake_needed_on = weekdays(this_years_birthday - adjust_birthday),
                                  birth_month_label = month(this_years_birthday, label = TRUE, abbr = FALSE))

# 5. Count how many birthdays there are on each weekday in each month
# This is a bit of a confusing requirement. Looking at the required output we need to count how many birthday cakes
# per month and per day (e.g January Friday, 33) and then join this data against each student.
#
# First, lets get number of observations by cake_day and birth_month. dplyr makes this pretty easy
cake_month_count <- count(student_contact_details, cake_needed_on, birth_month_label)

# Now lets join this to each record in the original dataset so the number of observations sit against each record
student_contact_details <- left_join(student_contact_details, cake_month_count, by = c("birth_month_label","cake_needed_on"))


# 6. Use the SELECT() function to select the needed columns as per specification
student_contact_details_output <- select(student_contact_details, pupils_name, `Date of Birth`, 
                                         this_years_birthday, birth_month_label, cake_needed_on, n) 

# rename before output - I suspect there is a more efficient way of doing this. 
student_contact_details_output <- rename(student_contact_details_output,
                                         `Pupil Name` = pupils_name,
                                         `This Years Birthday` = this_years_birthday,
                                         Month = birth_month_label, 
                                         `Cake Needed On` = cake_needed_on,
                                         `BDs per Weekday and Month` = n)

# 7. Output data - write a new CSV file to the outputs directory.
write.csv(student_contact_details_output, "2022/2022.Wk2 Birthday Cakes/outputs/birthday_cakes.csv", row.names=FALSE)
