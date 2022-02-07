# PREPPIN' DATA | 2022 | Week 4 | The Prep School - Travel Plans
# https://preppindata.blogspot.com/2022/01/2022-week-4-prep-school-travel-plans.html
#
# Solution prepared by Darragh Murray.
# Last updated: 5 February 2022
# Solution delayed by the birth of my firstborn son!

# INSTALL PACKAGES (if required)
install.packages("tidyverse")
install.packages("readr")

# INITIALISE LIBRARIES
library(tidyverse)
library(readr) # for data import
library(stringr) # for data operations

# IMPORT DATA
# We don't actually need the student details data to complete this task, all we need
# is the student_travel dataset

student_travel <- read_csv("2022/2022.Wk4 Travel Plans/data/PD 2021 WK 1 to 4 ideas - Preferences of Travel.csv")

# TIDY DATA

#I don't like spaces in my column names

names(student_travel) <- gsub(" ", "_", names(student_travel))

# TRANSFORMATIONS

# 1.  The proposed task can actually all be done in one big slab of R code using pipes and using many dplyr functions

student_travel <- student_travel %>%
  pivot_longer(!Student_ID, names_to = "Weekday", 
               values_to = "Method_Of_Travel") %>% # trqnspose the data into the create format
  mutate(Method_Of_Travel = str_replace_all(Method_Of_Travel, c("Carr" = "Car", 
                                                                "Bycycle" = "Bicycle",
                                                                "Scoter" = "Scooter",
                                                                "Scootr" = "Scooter",
                                                                "Helicopeter" = "Helicopter",
                                                                "Walkk" = "Walk",
                                                                "Wallk" = "Walk",
                                                                "Waalk" = "Walk",
                                                                "WAlk" = "Walk"))) %>% #do all the spelling corrections
  select(., -Student_ID) %>% #remove the student ID column
  group_by(Method_Of_Travel, Weekday) %>% #create groups by method and day
  summarise(Number_Of_Trips = n()) %>% # Create a count for number of trips by grouping variables
  mutate(`Sustainable?` = ifelse(Method_Of_Travel %in% c("Aeroplane","Car","Van", "Helicopter"), "Non-Sustainable","Sustainable")) %>% #classify as sustainable or not by travel method
  mutate(Trips_Per_Day = 1000) %>% # could count trips per day, but this value never changes
  mutate(`%_Of_Trips_Per_Day` = round(Number_Of_Trips/Trips_Per_Day,2)) %>% #divide trips into trips per day
  select(`Sustainable?`, Method_Of_Travel, Weekday, Number_Of_Trips, Trips_Per_Day, `%_Of_Trips_Per_Day`) # change column order


# 2. Output data - write a new CSV file to the outputs directory.
write.csv(student_travel, "2022/2022.Wk4 Travel Plans/outputs/travel_plans.csv", row.names=FALSE)


