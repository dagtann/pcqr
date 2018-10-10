## Part A ==================================================
student_data[student_data$student == "Angela", "science"] <- 603
student_data[student_data$student == "Cheryl", "literature"] <- 28
student_data <- student_data[!student_data$student == "Joel", ]
filter <- "gender" != names(student_data)
student_data <- student_data[, filter]

# Part B ===================================================
# Scale grades
tmp <- student_data[, c("science", "arts", "literature")]
tmp <- scale(tmp)
grade <- (tmp[, 1] + tmp[, 2] + tmp[, 3]) / 3
# OR grade <- rowMeans(tmp)
# OR grade <- apply(tmp, 1, FUN = mean)
# ...
student_data <- data.frame(student_data, grade = grade)
rm(tmp, grade)

# Assign alphabetical grades
grade_quantiles <- quantile(
  student_data[, "grade"], probs = c(.2,.4,.6,.8)
)
# A: grade > .8
# B: .8 <= grade & grade > .6
# C: .6 <= grade & grade > .4
# D: .4 <= grade & grade > .2
# F: grade < .2
student_data[, "grade_alp"] <- "F"
student_data[student_data[, "grade"] > grade_quantiles["20%"], "grade_alp"] <- "D"
student_data[student_data[, "grade"] > grade_quantiles["40%"], "grade_alp"] <- "C"
student_data[student_data[, "grade"] > grade_quantiles["60%"], "grade_alp"] <- "B"
student_data[student_data[, "grade"] > grade_quantiles["80%"], "grade_alp"] <- "A"

student_data <- within(student_data, { # Alternatively
  grade_alp <- "F"
  grade_alp <- ifelse(grade > grade_quantiles["20%"], "D", grade_alp)
  grade_alp <- ifelse(grade > grade_quantiles["40%"], "C", grade_alp)
  grade_alp <- ifelse(grade > grade_quantiles["60%"], "B", grade_alp)
  grade_alp <- ifelse(grade > grade_quantiles["80%"], "A", grade_alp)
  }
)
rm(grade_quantiles)

# Fail Greg
student_data[student_data[, "student"] == "Greg", "grade_alp"] <- "F"

# Create office list
office_list <- student_data[, c("student_id", "grade_alp")]
office_list
