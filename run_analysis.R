##################################################################################
#This file contains the code required for Getting and Cleaning Data project as part of Data Scientist toolbox course.
##################################################################################

##################################################################################
#1. Merges the training and the test sets to create one data set
##################################################################################
#Loading required packages
library("plyr")
library("dplyr")

# Downloading the source files to the current working directory.
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl,destfile="getdata_Fprojectfile_FUCI_HAR_Dataset.zip",method="libcurl")
filenames <- unzip(zipfile="getdata_Fprojectfile_FUCI_HAR_Dataset.zip")

# Using column bind function to read and combine multiple files.
test_op <- cbind(read.table(file=filenames[grep("subject_test",filenames)],header=FALSE),(read.table(file=filenames[grep("/[Yy]_[Tt][Ee][Ss][Tt]",filenames)],header=FALSE)),(read.table(file=filenames[grep("/[Xx]_[Tt][Ee][Ss][Tt]",filenames)],header=FALSE)))
test_op$src <- "test"

train_op <- cbind(read.table(file=filenames[grep("subject_train",filenames)],header=FALSE),(read.table(file=filenames[grep("/[Yy]_[Tt][Rr][Aa][Ii][Nn]",filenames)],header=FALSE)),(read.table(file=filenames[grep("/[Xx]_[Tt][Rr][Aa][Ii][Nn]",filenames)],header=FALSE)))
train_op$src <- "train"

# Using rowbind function to combine both the inputs into a single output
data_op <- rbind(test_op,train_op)

# Reading the features and activity labels file 
activity_labels <- read.table(file=filenames[grep("activity_labels",filenames)],header=FALSE)
features <- read.table(file=filenames[grep("features.txt",filenames)],header=FALSE)

# Naming the columns in the dataset.
colnames(data_op) <- c("Person_Identifer","Activity_Name",as.character(features[[2]]),"Source_Identifier")

#### Making the columns unique because it is not there in the data set
colnames(data_op) <- make.unique(colnames(data_op))

##################################################################################

##################################################################################
#2. Extracting only the measurements on the mean and standard deviation for each measurement
##################################################################################

Extract_op <- select(data_op,c(Person_Identifer,Activity_Name,Source_Identifier,grep("mean|std",colnames(data_op))))

##################################################################################
#3. Getting the Descriptive Activity Name in place of Activity number.
##################################################################################
data_op$Activity_Name <- factor(data_op$Activity_Name)
data_op <- mutate(data_op,Activity_Name=activity_labels[match(Activity_Name,activity_labels[,1]),2])

##################################################################################
#4. Renaming the columns
##################################################################################
colnames(data_op) <- gsub(pattern="-",replacement="_",x=colnames(data_op))
colnames(data_op) <- gsub(pattern="^t",replacement="Time_Domain_",x=colnames(data_op))
colnames(data_op) <- gsub(pattern="^f",replacement="Frequency_Domain_",x=colnames(data_op))
colnames(data_op) <- gsub(pattern="BodyBody",replacement="Body_",x=colnames(data_op))
colnames(data_op) <- gsub(pattern="),",replacement=",",x=colnames(data_op))
colnames(data_op) <- gsub(pattern="\\.2$",replacement="_Duplicate_2",x=colnames(data_op))
colnames(data_op) <- gsub(pattern="\\.1$",replacement="_Duplicate_1",x=colnames(data_op))

##################################################################################
#5. Independent tidy data set with the average of each variable for each activity and each subject
##################################################################################
summary_op <- ddply(data_op,.(Person_Identifer,Activity_Name,Source_Identifier),numcolwise(mean))
write.table(summary_op,"tidy_data_set.txt",row.names=FALSE)
##################################################################################
