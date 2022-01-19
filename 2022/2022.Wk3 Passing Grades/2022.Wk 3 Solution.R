# PREPPIN' DATA | 2022 | Week 3 | The Prep School - Passing Grades
# https://preppindata.blogspot.com/2022/01/2022-week-3-prep-school-passing-grades.html
#
# Solution prepared by Darragh Murray.
# Last updated: 19 January 2021

# INSTALL PACKAGES (if required)
install.packages("tidyverse")
install.packages("readr")

# INITIALISE LIBRARIES
library(tidyverse)
library(readr) # for data import
library(lubridate) # for data operations

# IMPORT DATA
student_details <- read_csv("2022/2022.Wk3 Passing Grades/data/PD 2022 Wk 1 Input - Input.csv")
student_grades <- read_csv("2022/2022.Wk3 Passing Grades/data/PD 2022 WK 3 Grades.csv")

# TIDY DATA & PREP

# That's it! I'm sick of column names with spaces in them - it makes it hard to work with in R, so lets clean
# them all up!

names(student_grades) <- gsub(" ", "_", names(student_grades))
names(student_details) <- gsub(" ", "_", names(student_details))

#1. We're actually going to start with requirement 3 first
# Pivot the data to create one row of data per student and subject
# Let's use tidyverses powerful pivot() functions - in this case pivot_longer()

student_grades <- student_grades %>%
  pivot_longer(!Student_ID, names_to = "subject", 
                               values_to = "grade")


# 2. Join the data sets together to give us the grades per student & Drop unneeded parental data fields & determined if passed
# This can be done all in one command using %>% pipes.

student_results <- student_details %>%
  rename(Student_ID = id) %>% # create a common field
  merge(student_grades, by = "Student_ID", all.x=TRUE) %>% # join on the newly created common field
  select(-starts_with(c("Parental","Preferred"))) %>% #select only needed fields e.g not parental contact data
  mutate(passed_subject = ifelse(grade >= 75, "passed","failed" )) # determine if student passed subject

# 3. Create an average score per student based on all of their grades (rounded to 1 decimal place) 
# & count the number of subjects passed

student_avg_grades<- student_results %>%
  group_by(Student_ID) %>% # create groups in the data on studentID
  summarise(students_avg_score= round(mean(grade, na.rm=TRUE),1), passed_subjects = sum(passed_subject == "passed")) #summarise using an average function and also count number passed

# 4. Select the required fields for output and cleanup!

student_results <- student_results %>%
  merge(student_avg_grades, by = "Student_ID", all.x=TRUE) %>% # join together the aggregated subject data
  select(passed_subjects, students_avg_score, Student_ID, gender) %>% # select necessary fields
  distinct() #only return distinct rows (duplications caused by join field.)

# 5. Output data - write a new CSV file to the outputs directory.
write.csv(student_results, "2022/2022.Wk3 Passing Grades/outputs/passing_grades.csv", row.names=FALSE)
