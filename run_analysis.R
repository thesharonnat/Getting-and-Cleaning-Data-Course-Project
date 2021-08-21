library(dplyr)

# Download and unzip dataset 
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# Read training data 
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# Read test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# Read features 
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# Read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

# Step 1- Merge the training and the test sets to create one data set
humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

colnames(humanActivity) <- c("subject", features[, 2], "activity")

# Step 2- Extract only the measurements on the mean and standard deviation for each measurement
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]

# Step 3- Use descriptive activity names to name the activities in the data set
humanActivity$activity <- factor(humanActivity$activity, levels = activities[, 1], labels = activities[, 2])

# Step 4- Appropriately label the data set with descriptive variable names
humanActivityCols <- colnames(humanActivity)
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

colnames(humanActivity) <- humanActivityCols

# Step 5 - Create a second, independent tidy set with the average of each variable for each activity and each subject
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)












