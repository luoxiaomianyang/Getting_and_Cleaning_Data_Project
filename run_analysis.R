# **********************************************
# Ruoshi Li
# Date: May 6th, 2019
# Comments: R Programming -  Class 3 Porject
# **********************************************

# Load libraries
library(dplyr)
library(myDataCombine)
library(DBI)
library(quantmod)

# Clear all variables and prior sessions
rm(list = ls(all = TRUE))

# Set 'working directory'
wdir <- "/Users/Ruoshi/Documents/Study/Data Science/Coursera_Johns_Hopkins/Class 3/Practice/UCI HAR Dataset"
setwd(wdir)

# ------------------------- Part 1: Merges the training and the test sets to create one myData set -------------------------#
# read myData from test sets
x.test <- read.table("./test/X_test.txt")
y.test <- read.table("./test/y_test.txt")
# read myData from test sets
x.train <- read.table("./train/X_train.txt")
y.train <- read.table("./train/y_train.txt")
# merge x.test and y.test
test.data <- cbind(x.test,y.test)
# merge x.train and y.train
train.data <- cbind(x.train,y.train)
# merge test myData and train myData
myData <- rbind(test.data,train.data)

# ---------------- Part 2: Extracts only the measurements on the mean and standard deviation for each measurement ----------------#
# read features file
features <- read.table("./features.txt")
# extracts col# of mean measurements
mean.col <- grep("mean",features[, 2],value = FALSE)
# extracts col# of standard deviation measurements
std.col <- grep("std",features[, 2],value = FALSE)
# merge y.test and y.train
y <- rbind(y.test,y.train)
# combine mean and std cols and activity labels
col.num <- c(mean.col,std.col)
myData <- myData[ ,col.num]
myData <- cbind(myData,y)
#------------------------- Part 3: Uses descriptive activity names to name the activities in the dataset -------------------------#
activity.labels <- read.table("./activity_labels.txt")
colnames(myData)[ncol(myData)] <- "activity"
for (i in 1:nrow(myData)) {
  myData[i,ncol(myData)] = toString(myData[i,ncol(myData)])
}
replace <- data.frame(from = activity.labels[, 1], to = activity.labels[, 2])
new.myData <- FindReplace(data = myData, Var = "activity", replaceData = replace, from = "from", to = "to",exact = FALSE)
new.myData$activity
#------------------------- Part 4: ppropriately labels the data set with descriptive variable names -------------------------#
# name the data
names(new.myData) <- features[col.num, 2]

#------- Part 5: Create another tidy myData set with the average of each variable for each activity and each subject-------#
colnames(new.myData)[ncol(new.myData)] <- "activity"
sub.test <- read.table("./test/subject_test.txt")
sub.train <- read.table("./train/subject_train.txt")
subject <- rbind(sub.test,sub.train)
new.myData2 <- cbind(new.myData,subject)
colnames(new.myData2)[ncol(new.myData2)] <- "subject"
features.list <- as.list(features[col.num, 2])
new.myData3 <- aggregate(new.myData2[, 1:(ncol(new.myData2)-2)],list(new.myData2$subject,new.myData2$activity),mean)
