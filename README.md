#Getting and Cleaning Data Course Project

Project of [Getting and Cleaning Data course on Coursera](https://class.coursera.org/getdata-014/human_grading/view/courses/973501/assessments/3/submissions), May 2015 edition.

## Project Description
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers
on a series of yes/no questions related to the project.

You will be required to submit:

1. a tidy data set as described below
2. a link to a Github repository with your script for performing the analysis, and
3. a code book that describes the variables, the data, and any transformations or
   work that you performed to clean up the data called CodeBook.md. You should also
   include a README.md in the repo with your scripts. This file explains how all
   of the scripts work and how they are connected. 

One of the most exciting areas in all of data science right now is wearable computing.
Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced
algorithms to attract new users. The data linked to from the course website represent
data collected from the accelerometers from the Samsung Galaxy S smartphone.
A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/HumanActivityRecognitionUsingSmartphones

Here are the data for the project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names. 
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

## What you find in this repository

* __CodeBook.md__: information about raw and tidy data set and elaboration made to
  transform them
* __README.md__: this file
* __run_analysis.R__: R script to transform raw data set in a tidy one
* __tidy_data_set.txt__: tidy data set that is created as part of this step 5 in this project.

## How to create the tidy data set

1. download [compressed raw data](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)
2. unzip raw data and copy the directory `UCI HAR Dataset` to your current working directory in R.
3. open a R console and set the working directory (use setwd()).
4. source run_analisys.R script (it requires the plyr and dplyr package): `source('run_analysis.R')`.

