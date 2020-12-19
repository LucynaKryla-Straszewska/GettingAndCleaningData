# Getting and cleaning data final Assignment

# Input data: data collected from the accelerometers from the Samsung Galaxy S smartphone for 30 persons (subjects) and 6 types of activities.
# Output data: tidy dataset with the average of each provided variable for each activity and each subject.
# For details see: README.Rmd

# Loading libraries
library(dplyr)
library(mgsub)


# Downloading and reading data
if (!file.exists("./data")) {dir.create("./data")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/Dataset.zip", method = "curl")

# Unziping files, overwrite = TRUE to replace files if previously extracted
unzip("./data/Dataset.zip", overwrite = TRUE)

# Reading training and test data
subjectsTrainingData <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
valuesTrainingData <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
activityTrainingData <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)

subjectsTestData <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
valuesTestData <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
activityTestData <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)

# Reading activities labels
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE, col.names = c("code", "label"))

# Reading features
# Note: These are variables of testData and trainingData (number of variables 561 quals number of columns in testData and trainingData)
variables <- read.table("./UCI HAR Dataset/features.txt", header = FALSE, col.names = c("id", "featureName"))


# Step1: Merging the trainingData and testData

trainingSet <- cbind(subjectsTrainingData, activityTrainingData, valuesTrainingData)
testSet <- cbind(subjectsTestData, activityTestData, valuesTestData)
mergedSet <- rbind(trainingSet, testSet)
colnames(mergedSet) <- c("subjectId", "activityType", variables$featureName)


# Step 2: Extracting only mean and std measurements for each Subject / Activity
#Note: Some stats are capitalized so using [Mm] and [Ss]
#Note: The below reduces number of variables from 563 to 88

selectedStatsSet <- mergedSet[, grepl("subjectId|activityType|[Mm]ean|[Ss]td", colnames(mergedSet))]

# Step3: Descriptive activity names used to name the activities
# Note: Changing Subjects and Activities to Factors
# Note: Tidying the activities names: lowercases

selectedStatsSet$subjectId <- as.factor(selectedStatsSet$subjectId)
selectedStatsSet$activityType <- factor(selectedStatsSet$activityType, levels = as.character(activityLabels$code), labels = tolower(activityLabels$label))


# Step4: Adding labels with descriptive variable names
# Note: Using mgsub function from mgsub for multiple replacements

selectedStatsSetColNames <- colnames(selectedStatsSet)
wrongDescriptions <- c("\\(","\\)", "\\,","\\-", "^t", "^f", "Acc", "Gyro", "Mag", "Freq", "mean", "std", "BodyBody", "gravity")
correctDescriptions <- c("", "", "", "", "timeDomain", "frequencyDomain", "Acceleration", "Gyroscope", "Magnitude", "Frequency", "Mean", "StandardDeviation", "Body", "Gravity")

selectedStatsSetColNames <- mgsub(selectedStatsSetColNames, wrongDescriptions, correctDescriptions)
colnames(selectedStatsSet) <- selectedStatsSetColNames


# Step5: Creating independent/tidy dataset with the mean values per each activity and each subject
# Note: chaining with group_by and summarize across all mean

selectedStatsSetMeans <- selectedStatsSet %>%
  group_by(subjectId, activityType) %>%
  summarise(across(everything(), list(mean)))

# Writing to output file
write.table(selectedStatsSetMeans, "tidy_data.txt", row.names = FALSE, col.names = TRUE)
