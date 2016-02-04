##Coursera Getting and Cleaning Data
##Course Project
##This script creates and prints to the console two tidy datasets. The first merges
##subject ID, activity, and measurements for the training and test subjects provided
##by UCI HAR. The tidydf object is a subset of this data that contains all mean and 
##standard deviation variables. The second dataset (groupmeans) contains the means 
##on each variable for each activity and subject. Please see Codebook.md and 
##README.md for more information on the dataset components, variables, and codings.

##load libraries
library(dplyr)
library(reshape2)
##download and read in data
#create URL object
URL <-
  "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
##download file to working directory
download.file(URL, destfile = "/Users/bretbarrowman/data working directory/Coursera Getting and Cleaning Data/Samsung.zip",
              method = "curl")
unzip("Samsung.zip")


##Step 1: Merge training and test datasets to create one dataset
##read in test and training datasets
subject.test <- read.table("subject_test.txt")
subject.train <- read.table("subject_train.txt")
test.x <- read.table("X_test.txt")
train.x <- read.table("X_train.txt")
test.y <- read.table("Y_test.txt")
train.y <- read.table("Y_train.txt")
##label variables in subject and y datasets for merging purposes
subject.test <- select(subject.test, subjectID = V1)
subject.train <- select (subject.train, subjectID = V1)
test.y <- select(test.y, activity = V1)
train.y <- select(train.y, activity = V1)
##merge datasets
testdf <- cbind(subject.test, test.y, test.x)
##convert to tbl_df object for easier viewing
testdf <- tbl_df(testdf)
trainingdf <- cbind(subject.train, train.y, train.x)
trainingdf <- tbl_df(trainingdf)
##merge into one dataset
fulldf <- rbind(testdf, trainingdf)
fulldf <- tbl_df(fulldf)
##remove smaller datasets to avoid confusion
rm("trainingdf", "testdf", "train.y", "test.y", "subject.train", "subject.test", 
   "train.x", "test.x")

##Step 2: Extract measurements on mean and standard deviation
##read in list of variable names
variables <- read.table("features.txt", colClasses = "character")
variables <- variables$V2
##find variables containing mean() or std()
meanvars <- grep("mean()", variables)
stdvars <- grep("std()", variables)
##subset to variables for mean and standard deviation for each measurement
##shift grep vectors above to account for two variables (subjectID and activity)
##added to front of dataset
meanvars <- meanvars + 2
stdvars <- stdvars + 2
tidydf <- select(fulldf, subjectID, activity, meanvars, stdvars)
rm("fulldf")

##Step 3: Uses descriptive activity names to name the activities in the data set
##label activities so that activity names are displayed instead of factor codings
##read in activity names
activities <- read.table("activity_labels.txt")
##convert activity variable in tidydf to factor and add labels
tidydf$activity <- factor(tidydf$activity, labels = activities$V2)
rm("activities")


##Step 4: Label dataset with descriptive variable names
##create vector of "mean()" variables
meanvarnames <- variables[grepl("mean()", variables)]
meanvarnames <- data.frame(meanvarnames)
colnames(meanvarnames) <- "names"
##create vector of "std()" variables
stdvarnames <- as.data.frame(variables[grepl("std()", variables)])
stdvarnames <- data.frame(stdvarnames)
colnames(stdvarnames) <- "names"
##combine into one vector with "mean()" first, since dplyr select function 
##above placed mean variables toward the front of the dataset
varnames <- rbind(meanvarnames, stdvarnames)
rm("meanvars", "meanvarnames", "stdvars", "stdvarnames", "variables")
##edit variable names to remove dashes and parentheses
varnames <- gsub("[-()]", "", varnames$names)
##add subjectID and activity variable names to vector
temp <- c("subjectID", "activity")
varnames <- c(temp, varnames)
##assign new variable names to tidydf
colnames(tidydf) <- varnames
rm("temp", "varnames")

##Step 5: creates a second, independent tidy data set with the average 
##of each variable for each activity and each subject.
groupmeans <- tidydf %>%
  group_by(subjectID, activity) %>%
  summarize_each(funs(mean))
##print datasets to console
tidydf
groupmeans
