#Get_Clean_data_Assignment.R

######################### Getting and Cleaning Data Project #############################################
fileUrl<-"http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones"  #<<<<<INTERESTING: HOW DATA WAS CAPTURED: 
#sampling in fixed widrg sliding window of 2.56sec & 50% overlap, with 12 readings/window
#acceleration signal separated using Butterworth low-pass filter with 0.3 hz cutoff frequency
#From each window, a vector of features was obtained by calculating variables from the time and frequency domain

#File: Get_Clean_data_Assignment.R

##########################################################################################################
library(dplyr)

#Download data
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"  
downloaded_file<-"./data/WearIoT.zip"
if (!file.exists(downloaded_file)) {
        download.file(fileUrl, destfile = "./data/WearIoT.zip", method="curl")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
        unzip(downloaded_file)
}

############################################################################################
#       Activity 0: Read Data
############################################################################################
#read the name for the columns
features<-read.table("./data/WearIoT/UCI HAR Dataset/features.txt", header=FALSE)
#read the activity labels
activity_labels<-read.table("./data/WearIoT/UCI HAR Dataset/activity_labels.txt", header=FALSE)
colnames(activity_labels) <- c("activityId", "activityLabel")

#read train data ----------------------------------------------------------------------
WearIoT_train_values<-read.table("./data/WearIoT/UCI HAR Dataset/train/X_train.txt", header=FALSE)
names(WearIoT_train_values)<-features$V2
WearIoT_train_activity<-read.table("./data/WearIoT/UCI HAR Dataset/train/y_train.txt", header=FALSE)
names(WearIoT_train_activity)<-c("activity")
WearIoT_train_subjects<-read.table("./data/WearIoT/UCI HAR Dataset/train/subject_train.txt", header=FALSE)
names(WearIoT_train_subjects)<-c("subject")


#read test data ----------------------------------------------------------------------
WearIoT_test_values<-read.table("./data/WearIoT/UCI HAR Dataset/test/X_test.txt", header=FALSE)
names(WearIoT_test_values)<-features$V2
WearIoT_test_activity<-read.table("./data/WearIoT/UCI HAR Dataset/test/y_test.txt", header=FALSE)
names(WearIoT_test_activity)<-c("activity")
WearIoT_test_subjects<-read.table("./data/WearIoT/UCI HAR Dataset/test/subject_test.txt", header=FALSE)
names(WearIoT_test_subjects)<-c("subject")

############################################################################################
#       Activity 1: Merges the training and the test sets to create one data set.
############################################################################################

#add the label and subject columns to the test data --------------------------------
WearIoT_test2<-cbind(WearIoT_test_values,WearIoT_test_activity)      #adding column label
WearIoT_test2<-cbind(WearIoT_test2,WearIoT_test_subjects)      #adding column label

#add the label and subject columns to the train data
WearIoT_train2<-cbind(WearIoT_train_values,WearIoT_train_activity)      #adding column label
WearIoT_train2<-cbind(WearIoT_train2,WearIoT_train_subjects)      #adding column label

#combine 2 data sets (add rows)
WearIoT_data<-rbind(WearIoT_test2, WearIoT_train2)
WearIoT_data2<-WearIoT_data


############################################################################################
#       Activity 2: Extracts only the measurements on the mean and standard deviation for each measurement. (selet columns)
############################################################################################
#identify columns
list_of_ave_cols<-grep("mean()|std()|activity|subject",colnames(WearIoT_data2)) 
#create dataset with only selected columns
WearIoT_data_aves<-WearIoT_data2[,list_of_ave_cols]


############################################################################################
#       Activity 3: Uses descriptive activity names to name the activities in the data set
############################################################################################
#assign factors for activity
WearIoT_data_aves$activity<-factor(WearIoT_data_aves$activity, levels=activity_labels$activityId, labels=activity_labels$activityLabel)
#assign factors for subjects
WearIoT_data_aves$subject <- as.factor(WearIoT_data_aves$subject)


############################################################################################
#       Activity 4: Appropriately labels the data set with descriptive variable names.
############################################################################################
names_of_cols<-colnames(WearIoT_data_aves)
#perform name character substitutions 
names_of_cols<-gsub("[\\(\\)-]", "", names_of_cols)     #characters: (, ), -
names_of_cols<-gsub("^t","timeDomain",names_of_cols)
names_of_cols<-gsub("^f","frequencyDomain",names_of_cols)
names_of_cols<-gsub("Acc","Accelerometer",names_of_cols)
names_of_cols<-gsub("Gyro","Gyroscope",names_of_cols)
names_of_cols<-gsub("Freq","Frequency",names_of_cols)
names_of_cols<-gsub("Body","Body",names_of_cols)
names_of_cols<-gsub("Gravity","Gravity",names_of_cols)
names_of_cols<-gsub("Mag","Magnitude",names_of_cols)
names_of_cols<-gsub("mean()","Mean",names_of_cols)
names_of_cols<-gsub("std()","StardardDeviation",names_of_cols)
names_of_cols<-gsub("BodyBody","Body",names_of_cols)    #repeated
names_of_cols
#assign back the names of the cols
colnames(WearIoT_data_aves)<-names_of_cols
str(WearIoT_data_aves)

############################################################################################
#       Activity 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
############################################################################################
# group data by the 2 factors activity, & subject
WearIoT_data_aves2<-group_by(WearIoT_data_aves,subject, activity)
# summarize for each, getting the mean for each factor: activity & subject
WearIoT_data_aves3<-summarise_each(WearIoT_data_aves2, funs(mean))

############################################################################################
#       Output: Save to file
############################################################################################
#save to txt
write.table(WearIoT_data_aves3, "tidy_data.txt", row.names = FALSE, quote = FALSE)
#save to csv
write.csv(WearIoT_data_aves3, "tidy_data.csv", row.names = FALSE, quote = FALSE)
