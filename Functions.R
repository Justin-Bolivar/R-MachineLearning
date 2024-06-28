#install.packages(c("dplyr", "tidyr"))
library(dplyr)
library(tidyr)

# Part 1 Functions

# Function 1 (Finished)
dissolve_rate <- function(offerings, dept) {
  dept_offerings <- offerings[offerings$dept_code == dept,]
  all_classes <- nrow(dept_offerings)
  dissolved <- sum(dept_offerings$status == "DSLVD", na.rm =TRUE)
  rate <- (dissolved / all_classes) * 100
  
  return(rate)
}

#Function 2 (Finished)
dept_course_listing <- function(offerings, dept) {
  dept_offerings <- offerings[offerings$dept_code == dept,]
  
  return(dept_offerings$offer_no)
}

# Function 3 (Finished)
dept_density <- function(offerings, student_load, dept) {
  dept_offerings <- offerings[offerings$dept_code == dept, ]
  merged <- merge(dept_offerings, student_load, by = "offer_no")
  density <- table(merged$offer_no)
  mean_density <- mean(as.numeric(density))
  
  return(mean_density)
}

# Function 4 (Finished Need Double Checking)
special_class <- function(offerings, load) {
  active_classes <- subset(subj_offerings, status != "DSLVD")
  class_sizes <- table(load$offer_no)
  class_sizes_df <- as.data.frame(class_sizes)
  names(class_sizes_df) <- c("offer_no", "class_size")
  merged_data <- merge(active_classes, class_sizes_df, by = "offer_no")
  low_density_classes <- subset(merged_data, class_size < 20)
  
  
  return(low_density_classes)
}

# Function 5 (Finished)
class_list <- function(students,load, offer_code) {
  subjects <- load[load$offer_no == offer_code, ]
  student_names <- merge(subjects, students, by.x = "stud_id", by.y = "stud_no")
  
  return(paste(student_names$firstname, student_names$lastname))
}

# Function 6 (Finished)
grade_mean_by_gender <- function(students, gender) {
  students_gender <- students[students$gender == gender, ]
  
  return(mean(students_gender$avg_grade))
}

# Function 7 (Finished)
grade_IQ <- function(students) {
  correlation <- cor(students$avg_grade, students$IQ, use = "complete.obs")
  
  return(correlation)
}

# Function 8 (Finished)
grade_age<- function(students) {
  current_date <- Sys.Date()
  students$birthdate <- as.Date(students$birthdate, format = "%Y-%m-%d")
  students$age <- as.numeric(difftime(current_date, students$birthdate, units = "weeks")) / 52.2
  complete_cases <- complete.cases(students$avg_grade, students$age)
  filtered_students <- students[complete_cases, ]
  
  if (nrow(filtered_students) < 2) {
    return(NA)
  }
  
  return(cor(filtered_students$avg_grade, filtered_students$age, use = "complete.obs"))
}

# --------------------------------------------- Part 2 Questions ---------------------------------------------
#.1 (Finished)
withdraw <- function(load) {
  withdrawal <- load[load$wdw == 1, ]
  distinct <- length(unique(withdrawal$stud_id))
  
  return(distinct)
}

#.2 (Finished)
most_dissolved <- function(offerings) {
  dissolved_offerings <- offerings[offerings$status == 'DSLVD', ]
  dissolved_by_college <- table(dissolved_offerings$dept_code)
  most_dissolved_college <- names(which.max(dissolved_by_college))
  
  return(most_dissolved_college)
}


#.3
highest_special <- function(offerings, load) {
  special_classes <- special_class(subj_offerings, load)
  department_counts <- table(special_classes$dept_code)
  highest_dept <- names(which.max(department_counts))
  
  return(highest_dept)
}

#.4
college_lowest_density <- function(offerings, load) {
  class_sizes <- table(load$offer_no)
  class_sizes_df <- as.data.frame(class_sizes)
  names(class_sizes_df) <- c("offer_no", "class_size")
  merged_data <- merge(subj_offerings, class_sizes_df, by = "offer_no")
  density_data <- aggregate(class_size ~ dept_code, data = merged_data, mean)
  lowest_density_dept <- density_data$dept_code[which.min(density_data$class_size)]
  
  return(lowest_density_dept)
}


#.5
time_spent <- function(offerings, load) {
  merged_data <- merge(load, subj_offerings, by = "offer_no")
  merged_data$start_time <- as.POSIXct(strptime(sub(" -.*", "", merged_data$sch_time), format = "%I:%M %p"))
  merged_data$end_time <- as.POSIXct(strptime(sub(".*- ", "", merged_data$sch_time), format = "%I:%M %p"))
  merged_data$duration <- as.numeric(difftime(merged_data$end_time, merged_data$start_time, units = "hours"))
  duration_data <- aggregate(duration ~ dept_code, data = merged_data, mean)
  
  return(duration_data)
}