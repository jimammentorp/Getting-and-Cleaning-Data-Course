# Getting-and-Cleaning-Data-Course
README.md

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.
One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project: 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

One script was create, run_analysis.R.  It does the following:
* Merges the training and the test sets to create one data set.
* Extracts only the measurements on the mean and standard deviation for each measurement. 
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive activity names. 
* Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

run_analysis.R

These are the details of the script:

Data Preparation
# #This part of the program prepares the data for the steps requsted for the run_analysis R Program

#Downloads the zip file
download.file(“https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip”, destfile=“dataset.zip”)

#Unzips the zip file
unzip(zipfile="./UCI HAR Dataset/dataset.zip")

#reads the test data
subject_test <- read.table(“./UCI HAR Dataset/test/subject_test.txt”, header = F) features_test <- read.table(“./UCI HAR Dataset/test/x_test.txt”, header = F) activity_test <- read.table(“./UCI HAR Dataset/test/y_test.txt”, header = F)

#reads the train data
subject_train <- read.table(“./UCI HAR Dataset/train/subject_train.txt”, header = F) features_train <- read.table(“./UCI HAR Dataset/train/X_train.txt”, header=F) activity_train <- read.table(“./UCI HAR Dataset/train/y_train.txt”, header = F)

#reads the feaure names and activity labels
feature_names <- read.table(“./UCI HAR Dataset/features.txt”, header=F) activity_labels <- read.table(“./UCI HAR Dataset/activity_labels.txt”, header=F)

#Now the required activities 1-5 from the aassignment begin 

1. Merges the training and the test sets to create one data set.(subject, features and activity data)

combined_subject <- rbind(subject_test, subject_train) 
combined_features <- rbind(features_test, features_train) 
combined_activity<- rbind(activity_test, activity_train)

2. Extracts only the measurments on the mean and standard deviation for each measurement

colnames(feature_names) <- c(“Number”, “Name”)
features_meansd <- feature_names[(grepl(“mean()”, feature_names$Name) | grepl(“std()”, feature_names$Name)), ]
data_meansd <- combined_features[, features_meansd$Number]
colnames(data_meansd) <- features_meansd$Name

3.Uses descriptive activity names to name the activities in the data set

#adds activity number and subject to data_meansd 
data_meansd$label <- combined_activity$V1 data_meansd$subject <- combined_subject$V1
#Data_meansd acquires activity number, activity labels from activity_labels
data_meansd <- merge(activity_labels, data_meansd, by.x=“V1”, by.y=“label”, all=TRUE)

4.Appropriately labels the data set with descriptive variable names
#Renames the columns acquired by the merge with activity_lables
colnames(data_meansd)[1] <- “activity_number” 
colnames(data_meansd)[2] <- “activity_label”

#Refining column names by information infeered by features_info.txt
names(data_meansd)<-gsub(“t”, “time”, names(data_meansd)) 
names(data_meansd)<-gsub(“f”, “frequency”, names(data_meansd)) 
names(data_meansd)<-gsub(“Acc”, “Accelerometer”, names(data_meansd)) 
names(data_meansd)<-gsub(“Gyro”, “Gyroscope”, names(data_meansd))

5.From the data set in step 4, creates a second, independent tidy data set with the

#convert to numeric functions to support ddply call
data_meansd$activity_number <- as.numeric(data_meansd$activity_number) 
data_meansd$activity_label <- as.numeric(data_meansd$activity_label) 
data_meansd$subject <- as.numeric(data_meansd$subject)

#load plyr library that contains ddply command
library(plyr)

#calculate the averages
data_meansd_averages <- ddply(data_meansd, .(activity_number, subject), function(x) colMeans(x[, 1:82]))

#reacquire the activity labels
tidy_dataset <- merge(data_meansd_averages, activity_labels, by.x=“activity_number”, by.y=“V1”, all=TRUE)

#cleaning up the final tidy_dataset
colnames(tidy_dataset)[83] <- “activity_label”

#writing out the final tiday_dataset
write.table(tidy_dataset, 'tidy_dataset.txt', row.name=FALSE)

