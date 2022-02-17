# PREPPIN' DATA | 2022 | Week 5 | The Prep School - Setting Grades
# https://preppindata.blogspot.com/2022/02/2022-week-5-prep-school-setting-grades.html
#
# Solution prepared by Darragh Murray.
# Last updated: 17 February 2022
#
# This solution doesn't match the solution prepared by the Preppin' Data team and I suspect it's due to floating point differences in 
# in Tableau Prep v R meaning a few extra observations are included (around 20).
#
# Please reach out 

# INSTALL PACKAGES (if required)
install.packages("tidyverse")
install.packages("readr")

# INITIALISE LIBRARIES
library(tidyverse)
library(readr) # for data import

# IMPORT DATA

student_grades <- read_csv("2022/2022.Wk5 Setting Grades/data/PD 2022 Wk 5 Input.csv")

# TIDY DATA

#I don't like spaces in my column names

names(student_grades) <- gsub(" ", "_", names(student_grades))

# TRANSFORMATIONS


student_grades <- student_grades %>%
  pivot_longer(!Student_ID, names_to = 'subject', 
             values_to = 'subject_score') %>% # pivot data into one longer table.
  arrange(subject, -subject_score, Student_ID) %>% #reorder columns
  group_by(subject) %>% # group into subjects
  mutate(tile = ntile(subject_score, 6)) %>% # create a tiling scheme by subject score - splitting the distribution
                                             # into 6 neat groups
  rowwise() %>%
  mutate(grade = case_when(tile == 6 ~ 'A',
                                   tile == 5 ~ 'B',
                                   tile == 4 ~ 'C',
                                   tile == 3 ~ 'D',
                                   tile == 2 ~ 'E',
                                   TRUE ~ 'F')) %>% # using the tiling scheme, create grades
  mutate(points = case_when(tile == 6 ~ 10,
                                   tile == 5 ~ 8,
                                   tile == 4 ~ 6,
                                   tile == 3 ~ 4,
                                   tile == 2 ~ 2,
                                   TRUE ~ 1)) %>% # transpose those grades to scores
  group_by(Student_ID) %>%
  mutate('total_points' = sum(points),
         'student_total_score' = sum(subject_score),
         'a_student' = max(if_else(grade == 'A', 1,0))) %>% 
  #calculate the total scores of all grades based on grading scoring scheme. Also flag A students
  group_by(grade) %>%
  mutate('avg_student_total_points_per_grade' = round(mean(total_points),2)) %>% #calculate an average grade score
  ungroup() %>%
  mutate(a_student_average = max(avg_student_total_points_per_grade)) %>% # get the average total points per grade for A students
  filter(grade != 'A') %>% # filter out all A grades
  filter(total_points > a_student_average) %>% # filter out those results where the total points are LESS than the a student average
  select(c(avg_student_total_points_per_grade, total_points, grade, points, subject, Student_ID)) # select required columns. 

  
# 2. Output data - write a new CSV file to the outputs directory.
write.csv(student_grades, "2022/2022.Wk5 Setting Grades/output/setting_grades.csv", row.names=FALSE)
