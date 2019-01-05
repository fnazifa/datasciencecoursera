## The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.

Review criterialess 
The submitted data set is tidy.
The Github repo contains the required scripts.
GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
The README that explains the analysis files is clear and understandable.
The work submitted for this project is the work of the student who submitted it.
Getting and Cleaning Data Course Projectless 
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement.
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive variable names.
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Good luck!

# R Code

library(dplyr)

setwd("C:/Users/rimi2/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train")
trainingSubjects <- read.table(file.path("subject_train.txt"))
trainingValues <- read.table(file.path("X_train.txt"))
trainingActivity <- read.table(file.path("y_train.txt"))

setwd("C:/Users/rimi2/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test")
testingSubjects <- read.table(file.path("subject_test.txt"))
testingValues <- read.table(file.path("X_test.txt"))
testingActivity <- read.table(file.path("y_test.txt"))

setwd("C:/Users/rimi2/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")
features <- read.table(file.path("features.txt"))
activity <- read.table(file.path("activity_labels.txt"))


##### Step 1 - Merge the training and the test sets to create one data set
training <- cbind(trainingSubjects, trainingValues, trainingActivity)
testing <- cbind(testingSubjects, testingValues, testingActivity)
humanActivity <- rbind(training, testing)
x <- as.vector(features[,2])
colnames(humanActivity) <- c("Subject", x, "Activity")

##### Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
columnsToKeep <- grepl("Subject|Activity|mean|Mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]

##### Step 3:Uses descriptive activity names to name the activities in the data set

humanActivity$Activity <- factor(humanActivity$Activity, levels = activity[, 1], labels = activity[, 2])

##### Step 4 - Appropriately label the data set with descriptive variable names

renameHA_col <- colnames(humanActivity)
renameHA_col <- gsub("[\\(\\)-]", "", renameHA_col)
renameHA_col <- gsub("BodyBody", "Body", renameHA_col)
renameHA_col <- gsub("^f", "FrequencyDomain", renameHA_col)
renameHA_col <- gsub("^t", "TimeDomain", renameHA_col)
renameHA_col <- gsub("Acc", "Accelerometer", renameHA_col)
renameHA_col <- gsub("Gyro", "Gyroscope", renameHA_col)
renameHA_col <- gsub("Mag", "Magnitude", renameHA_col)
renameHA_col <- gsub("Freq", "Frequency", renameHA_col)
renameHA_col <- gsub("mean", "Mean", renameHA_col)
renameHA_col <- gsub("std", "Standard Deviation", renameHA_col)

colnames(humanActivity) <- renameHA_col

##### From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

humanActivityMeans <- humanActivity %>% 
        group_by(Subject, Activity) %>%
        summarise_all(funs(mean))
head(humanActivityMeans)

write.csv(humanActivityMeans, "tidy_data.csv", row.names = FALSE, quote = FALSE)
