# This script has the objective to download a file from the internet, to later clean the data set. 
# This cleaned data set will be more easy to manipulate for further analysis

#Prepare the enviroment
library(plyr)
library(tidyverse)


#Create a folder to download the zip file
dir.create(path = "Getting&Cleaning/")
setwd("Getting&Cleaning/")

file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(file_url,'./UCI HAR Dataset.zip', mode = 'wb')
unzip("UCI HAR Dataset.zip", exdir = getwd())


#Read the text file
#features & activities
features <- read.table("UCI HAR Dataset/features.txt", col.names=c("number","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names =c("number","activity"))

#from test folder
test_sub <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
test_x <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
test_y <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
test <- cbind(test_sub,test_x,test_y)
head(test)

#from train folder
train_sub <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
train_x <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
train_y <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
train <- cbind(train_sub,train_x,train_y)
head(train)
dim(train)

#1. Merges the training and the test sets to create one data set.
df <- rbind(test,train)
head(df)
dim(df)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
df_meansd <- df%>%
        select(subject,code,contains("mean"), contains("sd"))%>%
        print()

#3.Uses descriptive activity names to name the activities in the data set
df_meansd$code <- activities[df_meansd$code,2]

#4.Appropriately labels the data set with descriptive variable names. 
names(df_meansd)[2] = "activity"
names(df_meansd)<-gsub("Acc", "Accelerometer", names(df_meansd))
names(df_meansd)<-gsub("Gyro", "Gyroscope", names(df_meansd))
names(df_meansd)<-gsub("BodyBody", "Body", names(df_meansd))
names(df_meansd)<-gsub("Mag", "Magnitude", names(df_meansd))
names(df_meansda)<-gsub("^t", "Time", names(df_meansd))
names(df_meansd)<-gsub("^f", "Frequency", names(df_meansd))
names(df_meansd)<-gsub("tBody", "TimeBody", names(TidyData))
names(df_meansd)<-gsub("-mean()", "Mean", names(df_meansd), ignore.case = TRUE)
names(df_meansd)<-gsub("-std()", "STD", names(df_meansd), ignore.case = TRUE)
names(df_meansd)<-gsub("-freq()", "Frequency", names(df_meansd), ignore.case = TRUE)
names(df_meansd)<-gsub("angle", "Angle", names(df_meansd))
names(df_meansd)<-gsub("gravity", "Gravity", names(df_meansd))
str(df_meansd)

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
mean_activity <- df_meansd%>%
        group_by(subject,activity)%>%
        summarise_all(funs(avg=mean
                           ))%>%
        print()

write.table(mean_activity,"Getting and Cleaning Data Course Project.csv",row.names = FALSE)
