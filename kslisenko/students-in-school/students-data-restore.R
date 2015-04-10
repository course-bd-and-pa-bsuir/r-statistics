##### GET ALL DATA
student.mat <- read.csv("~/Documents/BD and PA Spring 2015/r-statistics/kslisenko/students-in-school/data/student-mat.csv", sep=";")
student.por <- read.csv("~/Documents/BD and PA Spring 2015/r-statistics/kslisenko/students-in-school/data/student-por.csv", sep=";")

allData = rbind(student.mat, student.por)

##### INSERT ERRORS TO DATA
# Function inserts empty values to randomly selected cells of specified data frame
addEmptyPlacesToData = function(data) {
  errorDataPercents = 0.9  # put errors to 90% of data
  
  # table rows number
  rows <- nrow(data)
  # sample row numbers in which we will insert empty values
  errorRowsIndexes <- sample(1:rows, rows * errorDataPercents)
  # take subset which will have errors from data
  errorData <- data[errorRowsIndexes,]
  errorDataRows <- nrow(errorData)
  errorDataColumns <- ncol(errorData)
  # calculate total amount of errors (at 10% of columns)
  problemsCount <- as.integer(errorDataRows * errorDataColumns / 10)
  # sample problem coordinates
  problemRowNumbers <- sample(1:errorDataRows,problemsCount,replace = TRUE)
  problemColumnNumbers <- sample(1:errorDataColumns,problemsCount,replace = TRUE)
  # insert empty values
  for(n in 1:problemsCount){
    errorData[problemRowNumbers[n],problemColumnNumbers[n]] <- NA
  }
  errorData
}

# View datasets after inserting empty values
View(addEmptyPlacesToData(student.mat))
View(addEmptyPlacesToData(student.por))

dataWithErrors = addEmptyPlacesToData(allData)

## TO THINK: может быть присвоить каждой записи ID-шник чтобы потом была возможность сравнить с оригиналом?
# Write data with errors to file
write.table(dataWithErrors, file = "~/Documents/BD and PA Spring 2015/r-statistics/kslisenko/students-in-school/data/student-with-errors.csv", sep=";", row.names = FALSE, na = "", col.names=FALSE)

##### Restore data with errors

## Dataset description
# Each row represents one student in school with information about: age, sex, exam results, parents, location, health status, etc... 
# Key data - marks for G1, G2, G3 exams
# Main idea of research, realated to dataset is predict marks based on student parameters
# Students study in 2 different schools
dataWithErrors <- read.table("~/Documents/BD and PA Spring 2015/r-statistics/kslisenko/students-in-school/data/student-with-errors.csv", sep=";", header = TRUE, na.strings = "")
View(dataWithErrors)

## Ideas how to restore data
# 1. Calculate correlation between different columns
# 1.1 Drop columns which do not correlate with others (this columns do not affect research results)
# 1.2 Find groups of columns which strobgly affect each other
# 1.2.1 Build regression models for correlated columns and predict missed values
# 2. For some columns we can take average values (average exam results in school)
# 3. Try decigion trees
# 4. Try random forest

# 1. Build corrgram
library(corrgram)
corrgram(dataWithErrors, order=TRUE)

## Columns which can affect each other
# 1. Failures~age~absences
group1 = c("failures", "age", "absences")
# 2. Dalc, freetime, Walc, goout
group2 = c("Dalc", "Walc", "freetime", "goout")
# 3. Fedu~Medu - G1~G2~G3 - studytime
group3 = c("Fedu", "Medu", "G1", "G2", "G3", "studytime")

## Calculate correlation using hetcor function
install.packages({"polycor"})
library(polycor)
hetcor(allData[c("school", "sex", "G1", "G2", "G3", "age", "Fedu", "Medu")])
hetcor(dataWithErrors[c("school", "sex", "G1", "G2", "G3", "age")])

##### Classification