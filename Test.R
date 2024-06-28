setwd("C:/Users/Justin/OneDrive/Documents/DevCodes/R programming/Midterm Project")

install.packages("readxl")

library(readxl)

study_load <- read_excel("stud_load.xlsx", sheet = 1)
subj_offerings <- read_excel("subj_offerings.xlsx", sheet = 1)
students <- read.csv("students.csv")

dept_code <- "BUSEN"
offer_code <- "3002"

gender1 <- "Male"
gender2 <- "Female"

# Function 1 test (Finished)
dissolve_rate(subj_offerings, dept_code)

# Function 2 test (Finished)
dept_course_listing(subj_offerings, dept_code)

# Function 3 test (Finished)
dept_density(subj_offerings, study_load, dept_code)

# Function 4 test 
special_class(subj_offerings, study_load)

# Function 5 test (Finished)
class_list(students, study_load, offer_code)

# Function 6 test (Finished)
grade_mean_by_gender(students, gender1)

# Function 7 test (Finished)
grade_IQ(students)

# Function 8 test (Finished)
grade_age(students)


# Part 2 Questions
#.1 (Finished)
withdraw(study_load)

#.2 (Finished)
most_dissolved(subj_offerings)

#.3
highest_special(subj_offerings, study_load)

#.4
college_lowest_density(subj_offerings,study_load)

#.5
time_spent(subj_offerings, study_load)

