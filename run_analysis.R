library(plyr);
library(data.table);


#Download zip file if text data does not exist on current folder
if(!file.exists("UCI HAR Dataset")) { 
  temp <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)
  file <- unzip(temp)
  unlink(temp)
}

# Read feature names and activity labels
featuresNames <- read.table(file.path("UCI HAR Dataset", "features.txt"),head=FALSE)
activityLabels <- read.table(file.path("UCI HAR Dataset", "activity_labels.txt"),header = FALSE)

# Read test data
activityTest  <- read.table(file.path("UCI HAR Dataset","test" , "Y_test.txt" ),header = FALSE)
subjectTest  <- read.table(file.path("UCI HAR Dataset", "test" , "subject_test.txt"),header = FALSE)
featuresTest  <- read.table(file.path("UCI HAR Dataset", "test" , "X_test.txt" ),header = FALSE)


# Read train data
activityTrain <- read.table(file.path("UCI HAR Dataset", "train", "Y_train.txt"),header = FALSE)
subjectTrain <- read.table(file.path("UCI HAR Dataset", "train", "subject_train.txt"),header = FALSE)
faturesTrain <- read.table(file.path("UCI HAR Dataset", "train", "X_train.txt"),header = FALSE)

# 1 Merge the training and the test sets to create one data set.
subject <- rbind(subjectTrain, subjectTest)
activity<- rbind(activityTrain, activityTest)
features<- rbind(faturesTrain, featuresTest)

#Update column names
colnames(subject)<-c("Subject")
colnames(activity)<- c("Activity")
colnames(features)<- featuresNames$V2

#Combine into one big data frame
Data <- cbind(subject, activity, features)

#2 Extracts only the measurements on the mean and standard deviation for each measurement.

subdataFeaturesNames<-featuresNames$V2[grep("mean|std", featuresNames$V2, ignore.case=TRUE)]
selectedNames<-c("Subject", "Activity", as.character(subdataFeaturesNames))
selectedData<-subset(Data,select=selectedNames)


#3 Uses descriptive activity names to name the activities in the data set

#change to character type so we can update the name
selectedData$Activity <- as.character(selectedData$Activity)
#replace with correct activity labels
for (i in 1:6){
  selectedData$Activity[selectedData$Activity == i] <- as.character(activityLabels[i,2])
}
#change to factor type
selectedData$Activity <- as.factor(selectedData$Activity)

#4 Appropriately labels the data set with descriptive variable names. 

# list names before update
names(selectedData)

#Make updates based on what's seen

names(selectedData)<-gsub("^t", "Time", names(selectedData))
names(selectedData)<-gsub("^f", "Frequency", names(selectedData))
names(selectedData)<-gsub("Acc", "Accelerometer", names(selectedData))
names(selectedData)<-gsub("Gyro", "Gyroscope", names(selectedData))
names(selectedData)<-gsub("Mag", "Magnitude", names(selectedData))
names(selectedData)<-gsub("BodyBody", "Body", names(selectedData))
names(selectedData)<-gsub("-mean()", "Mean", names(selectedData))
names(selectedData)<-gsub("-std()", "STD", names(selectedData))
names(selectedData)<-gsub("-freq()", "Frequency", names(selectedData))
names(selectedData)<-gsub("angle", "Angle", names(selectedData))
names(selectedData)<-gsub("gravity", "Gravity", names(selectedData))
names(selectedData)<-gsub("tBody", "TimeBody", names(selectedData))

#Names after update
names(selectedData)

#5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

finalData<-aggregate(. ~Subject + Activity, selectedData, mean)
finalData<-finalData[order(finalData$Subject,finalData$Activity),]
write.table(finalData, file = "tidydata.txt",row.name=FALSE)



