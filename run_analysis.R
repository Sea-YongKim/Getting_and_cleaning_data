library(dplyr)
library(data.table)

#read all data
featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

#1. merge data
# combine training and test data  
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)
#naming the column and merge combined data 
colnames(features) <- t(featureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)


#2. extracts  the measurements on the mean and standard deviation for each measurement.
#extract the colummn that has mean or std 
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
extracted <- completeData[,requiredColumns]
dim(extracted)


#3.Uses descriptive activity names to name the activities in the data set
#change the numertic type of activity in extracted to the character
extracted$Activity <- as.character(extracted$Activity)
for (i in 1:6){
      extracted$Activity[extracted$Activity == i] <- as.character(activityLabels[i,2])
}
extracted$Activity <- as.factor(extracted$Activity)

#Appropriately labels the data set with descriptive variable names.
#check the names of variables in extracted
names(extracted)
#replace the acroyms to full names 
names(extracted)<-gsub("Acc", "Accelerometer", names(extracted))
names(extracted)<-gsub("Gyro", "Gyroscope", names(extracted))
names(extracted)<-gsub("BodyBody", "Body", names(extracted))
names(extracted)<-gsub("Mag", "Magnitude", names(extracted))
names(extracted)<-gsub("^t", "Time", names(extracted))
names(extracted)<-gsub("^f", "Frequency", names(extracted))
names(extracted)<-gsub("tBody", "TimeBody", names(extracted))
names(extracted)<-gsub("-mean()", "Mean", names(extracted), ignore.case = TRUE)
names(extracted)<-gsub("-std()", "STD", names(extracted), ignore.case = TRUE)
names(extracted)<-gsub("-freq()", "Frequency", names(extracted), ignore.case = TRUE)
names(extracted)<-gsub("angle", "Angle", names(extracted))
names(extracted)<-gsub("gravity", "Gravity", names(extracted))
names(extracted)

#creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.
extracted$Subject <- as.factor(extracted$Subject)
extracted <- data.table(extracted)

tidyD <- aggregate(. ~Subject + Activity, extracted, mean)
tidyD <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyD, file = "tidy_data.txt", row.names = FALSE)

