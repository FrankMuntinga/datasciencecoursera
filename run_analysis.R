#################################################################################################################
## Script:      run_analysis.R                                                                                  #
## Name:        Frank Muntinga                                                                                  #
## Date:        23-01-2015                                                                                      #
## Goal:        You should create one R script called run_analysis.R that does the following.                   #
##              1. Merges the training and the test sets to create one data set.                                #
##              2. Extracts only the measurements on the mean and standard deviation for each measurement.      #
##              3. Uses descriptive activity names to name the activities in the data set                       #
##               4. Appropriately labels the data set with descriptive variable names.                          #
##               5. From the data set in step 4, creates a second, independent tidy data set                    #
##                 with the average of each variable for each activity and each subject.                        #
##                                                                                                              #
#################################################################################################################
## get data from the web and download file
dir.create("c:/work/R/downloads")
dir.create("c:/work/R/data")
file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(file_url, destfile = "c:/work/R/downloads/smartphone.zip")
##unzip file
unzip("c:/work/R/downloads/smartphone.zip", exdir = "c:/work/R/data")
## create dataframes for necessary files
setwd("c:/work/R/data/UCI HAR Dataset/")
activities <-  read.table("./activity_labels.txt")
features <-    read.table("./features.txt")
train_x <-     read.table("./train/x_train.txt")
train_y <-     read.table("./train/Y_train.txt")
train_subj <-  read.table("./train/subject_train.txt")
test_x <-      read.table("./test/X_test.txt")
test_y <-      read.table("./test/Y_test.txt")
test_subj <-   read.table("./test/subject_test.txt")
######################################################################################################
##        1. Merges the training and the test sets to create one data set.
######################################################################################################
train_test_x <- rbind(train_x, test_x)
## changes column headers for the train_test_x data frame
## make.names(..., unique=TRUE) needed to avoid duplicate column names during summarizing in step 5
headers <- as.character(features[,2])
headers <- make.names(headers,unique=TRUE)
names(train_test_x) <- headers
#######################################################################################################
##       2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## ####################################################################################################
col_names <- names(train_test_x)
train_test_x_mean <- grep("mean",col_names,fixed=TRUE)
train_test_x_stddev <- grep("std",col_names,fixed=TRUE)
result_mean <- train_test_x[train_test_x_mean]
result_stddev <- train_test_x[train_test_x_stddev]
result_total <- cbind(result_mean, result_stddev)
#######################################################################################################
##       4. Appropriately labels the data set with descriptive variable names. 
#######################################################################################################
col_names <- names(result_total)
col_headers <- gsub("fBody", "Body",col_names)
col_headers <- gsub("tBody", "Body",col_headers)
col_headers <- gsub("tGravity", "Gravity",col_headers)
col_headers <- gsub("\\(|\\)", "",col_headers)
names(result_total) <- col_headers
########################################################################################################
##       3. Uses descriptive activity names to name the activities in the data set
########################################################################################################
## bind the train_y and test_y data frames
train_test_y <- rbind(train_y, test_y)
## bind the subject_train and subject_test data frames
train_test_subject <- rbind(train_subj, test_subj)
## merge the activity descriptions from activities to the activity numbers in train_test_y
train_test_activities <- merge(train_test_y, activities,by.x= "V1",by.y="V1")
## bind the subject variable to the resul_total dataframe and rename header to "subject"
result_total_incl_subj <- cbind(result_total,train_test_subject)
col_names <- names(result_total_incl_subj)
col_headers <- gsub("V1", "Subject",col_names)
## make.names(..., unique=TRUE) needed to avoid duplicate column names during summarizing in step 5
col_headers <- make.names(col_headers,unique=TRUE)
names(result_total_incl_subj) <- col_headers
## bind the activity variable to the result_total_incl_subj dataframe and rename activity headers
result_total_incl_subj_act <- cbind(result_total_incl_subj,train_test_activities)
col_names <- names(result_total_incl_subj_act)
col_headers <- gsub("V1", "Activity-id",col_names)
col_headers <- gsub("V2", "Activity",col_headers)
## make.names(..., unique=TRUE) needed to avoid duplicate column names during summarizing in step 5
col_headers <- make.names(col_headers,unique=TRUE)
names(result_total_incl_subj_act) <- col_headers
#########################################################################################################
##       5. From the data set in step 4, creates a second, independent tidy data set 
##          with the average of each variable for each activity and each subject.
#########################################################################################################
install.packages("dplyr")
library(dplyr)
result_dplyr <- tbl_df(result_total_incl_subj_act)
Group_by_activity_subject <- group_by(result_dplyr,Activity, Subject)
result_tidy <- summarise_each(Group_by_activity_subject, funs(mean))
##write the tidy data-file and the base data-file to a locally stored TXT file
setwd("c:/work/R/data/")
write.table(result_tidy, file = "./tidy_dataset.txt", row.names=FALSE)
write.table(result_dplyr, file = "./integrated_dataset.txt", row.names=FALSE)