setwd("C:/Users/Shi/Desktop/DATA SCIENCE HOMEWORK/JH data science/UCI HAR Dataset")

#You should create one R script called run_analysis.R that does the following. 


#####################################################################
## 1. Merges the training and the test sets to create one data set.##
#####################################################################

trainData_x <-read.table("X_train.txt")
trainData_y <-read.table("y_train.txt")
trainsubject<- read.table("subject_train.txt")
 
testData_x  <- read.table("X_test.txt")
testData_y  <- read.table("y_test.txt")
testsubject <- read.table("subject_test.txt")
 
allData_x   <-rbind(trainData_x,testData_x)
allData_y    <-rbind(trainData_y,testData_y) 
allsubject     <-rbind(trainsubject,testsubject)

###############################################################################################
##2. Extracts only the measurements on the mean and standard deviation for each measurement. ##
###############################################################################################

features <- read.table("features.txt")[, 2]
## convert features to tidyfeatures 
tidyfeatures <- gsub("^f", "frequency", features)
tidyfeatures <- gsub("^t", "time", tidyfeatures)
tidyfeatures <- gsub("Acc", "acceleration", tidyfeatures)
tidyfeatures <- gsub("Gyro", "gyroscope", tidyfeatures)
tidyfeatures <- gsub("mean[(][)]", "mean", tidyfeatures)
tidyfeatures <- gsub("meanFreq[(][)]", "meanFrequency", tidyfeatures)
tidyfeatures <- gsub("std[(][)]", "standardDeviation", tidyfeatures)
tidyfeatures <- gsub("-", ".", tidyfeatures)
tidyfeatures <- gsub("BodyBody", "body", tidyfeatures)
tidyfeatures <-tolower(tidyfeatures)

index <- grep("mean|standardDeviation", tidyfeatures) #get the mean and std out of tidyfeatures

selectedData_x <- allData_x[,index] # select only the mean and std out of All x data

colnames(selectedData_x) <- tidyfeatures[index] # name V1 to V561 with conrresponding tidyfeatures.

#############################################################################
##3. Uses descriptive activity names to name the activities in the data set##
#############################################################################

activity<- read.table("activity_labels.txt")

tidyactivity <- gsub("_", ".", activity[,2]) 
tidyactivity <-data.frame(tolower(cbind(v1=1:6,tidyactivity))) 

activity_name<-tidyactivity[allData_y[,1],2] # convert the number in allData_y to the corresponding activities

allData_y[,1] <- activity_name 

##########################################################################
##4. Appropriately labels the data set with descriptive variable names. ##
##########################################################################

names(allData_y) <- "activity"
names(allsubject) <- "subject"

combined_data <-cbind(selectedData_x, allData_y, allsubject)

write.table(combined_data, file ="combined_data.txt") # write the fist verison of data

######################################################################################################
##5. independent tidy data set with the average of each variable for each activity and each subject.##
######################################################################################################

final_output <- aggregate(. ~ activity + subject,data = combined_data, FUN = mean) # get the mean 

write.table(final_output, file="final_output.txt",row.name=FALSE) #get the final required output

