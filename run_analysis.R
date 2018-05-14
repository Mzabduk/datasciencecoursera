
## This is a peer-reviewed assignment for Getting and Cleaning Data

## The goal is to create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
## ... variable for each activity and each subject.



## Installing and retrieving the package

install.packages("dplyr")

library(dplyr)

install.packages("plyr")

library(plyr)

 

## Downloading the archive

zipFile <- "UCI HAR Dataset.zip"

zipURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

 

download.file(zipURL, zipFile, mode = "wb")

 

## Unpacking the archive

dataPath <- "UCI HAR Dataset"

unzip(zipFile)

 

 

## Reading the training data

trainingSubjects <- read.table(file.path(dataPath,
"train", "subject_train.txt"))

trainingValues <- read.table(file.path(dataPath,
"train", "X_train.txt"))

trainingActivity <- read.table(file.path(dataPath,
"train", "y_train.txt"))

 

## Reading the test(validation) data

testSubjects <- read.table(file.path(dataPath,
"test", "subject_test.txt"))

testValues <- read.table(file.path(dataPath,
"test", "X_test.txt"))

testActivity <- read.table(file.path(dataPath,
"test", "y_test.txt"))

 

## Reading features without converting text labels to
factors

features <- read.table(file.path(dataPath,
"features.txt"), as.is =
TRUE)

 

## Reading the activities

activities <- read.table(file.path(dataPath,
"activity_labels.txt"))

colnames(activities) <- c("activityId", "activityLabel")

 

 

## Merging the two datasets (train and test)

 

## Concatenating individual data tables to make the single
data table and checking the result

mergedTrainTest <- rbind(cbind(trainingSubjects,
trainingValues, trainingActivity),

               
         cbind(testSubjects, testValues,
testActivity))

head(mergedTrainTest)

 

## Assigning column names

colnames(mergedTrainTest) <- c("subject",
features[, 2], "activity")

 

## Extracting only the mean and standard deviation for each
measurement

 

## Determining which columns to keep based on column name

columnsNeeded <-
grepl("subject|activity|mean|std", colnames(mergedTrainTest))

mergedTrainTest <- mergedTrainTest[, columnsNeeded]

 

 

## Using descriptive activity names 

 

## Replacing activity values with factor levels

mergedTrainTest$activity <-
factor(mergedTrainTest$activity, levels = activities[, 1], labels =
activities[, 2])

 

## Labeling the variables 

 

## Getting the column names

mergedTrainTestCols <- colnames(mergedTrainTest)

 

## Removing some unnecessary characters

mergedTrainTestCols <- gsub("[\\(\\)-]",
"", mergedTrainTestCols)

 

## Replacing shorthand with good descriptions

mergedTrainTestCols <- gsub("^f",
"frequencyDomain", mergedTrainTestCols)

mergedTrainTestCols <- gsub("^t",
"timeDomain", mergedTrainTestCols)

mergedTrainTestCols <- gsub("Acc",
"Accelerometer", mergedTrainTestCols)

mergedTrainTestCols <- gsub("Gyro",
"Gyroscope", mergedTrainTestCols)

mergedTrainTestCols <- gsub("Mag",
"Magnitude", mergedTrainTestCols)

mergedTrainTestCols <- gsub("Freq",
"Frequency", mergedTrainTestCols)

mergedTrainTestCols <- gsub("mean",
"Mean", mergedTrainTestCols)

mergedTrainTestCols <- gsub("std",
"StandardDeviation", mergedTrainTestCols)

 

 

## Using new labels as column names

colnames(mergedTrainTest) <- mergedTrainTestCols

 

 

## Creating a new dataset with the mean of each variable for
each activity and each person

 

## Grouping by subject and activity and summarize using mean

#mergedTrainTestMeans <- mergedTrainTest %>%
group_by(subject, activity) %>% summarise_each(funs(mean))

mergedTrainTestMeans <- ddply(mergedTrainTest, .(subject,
activity), function(x) colMeans(x[, 1:66]))

 

## Checking the number of rows and columns in original
dataset and means dataset

dim(mergedTrainTest)

dim(mergedTrainTestMeans)

 

## Exporting the results to a file

write.table(mergedTrainTestMeans,
"Tidy_Dataset_Means.txt", row.names = FALSE, quote = FALSE)

