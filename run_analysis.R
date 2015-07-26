## This script transforms the UCI Human Activity Recognition Using 
## Smartphones Dataset in a way that satisfies the following requirements:
##
## You should create one R script called run_analysis.R that does the following
## 1) Merges the training and the test sets to create one data set.
## 2) Extracts only the measurements on the mean and standard deviation for 
##    each measurement. 
## 3) Uses descriptive activity names to name the activities in the data set
## 4) Appropriately labels the data set with descriptive variable names. 
## 5) From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.
##
## The script assumes that the extracted dataset can be found in the current 
## working directory, i.e. the current working directory contains the folder
## "UCI HAR Dataset".
##
## The transformed data is written to a file called "tidy_data.txt". This
## file can be read into a data.frame via read.table("tidy_data.txt", header = T).
## 
## This script requires the package dplyr.

library(dplyr)

# the name of the directory that contains the data.
data_dir <- "UCI HAR Dataset"

# Loads a full dataset (test or train) which consists of subject identifiers, activity labels
# and feature data. After loading only features with std() and mean() in their name are selected.
# This takes care of the requirement 2): "Extracts only the measurements on the mean and 
# standard deviation for each measurement." 
# The activity names are converted to a factor with descriptive labels as 
# stated in requirement 3): "Uses descriptive activity names to name the 
# activities in the data set."
#
# Args:
#   subjects_path: the path to the subjects file
#   labels_path: the path to the activity labels file
#   data_path: the path to the feature data file
#   col_names: the column names for the features
#   activity_names: a list of human readable activity lables for the activites 1 to 6
#
# Returns:
#   The merged datset with the mean() and std() features
load.dataset <- function(subjects_path, labels_path, data_path, col_names, activity_names) {
  
  # load intput files into one dataframe
  data <- cbind(
    read.table(subjects_path),
    read.table(labels_path),
    read.table(data_path)                    
  )
  
  # set column names
  colnames(data) <- col_names
  
  # add activity labels
  data$activityname <- factor(data$activityname, labels = activity_names)
  
  # select mean and std dev features
  select(data, subjectid, activityname, matches("mean\\(|std\\("))
}

# Loads the columnames for a dataset from the file features.txt into a vector and adds
# the names "subjectid" and "activityname" for the first tow columns.
# Duplicate column names are made unique with make.unique.
#
# Returns:
#   A vector with unique column names
load.colnames <- function() {
  feature_names_path <- file.path(data_dir, "features.txt")
  names_table <- read.table(feature_names_path, row.names=1, stringsAsFactors = FALSE)
  make.unique(c("subjectid", "activityname", names_table$V2))
}

# Loads the human readable list of activity names in the order of activity ids 1 to 6.
#
# Returns:
#   An ordered character vector of activity names
load.activitynames <- function() {
  activity_names_path <- file.path(data_dir, "activity_labels.txt")
  read.table(activity_names_path, stringsAsFactors = FALSE)$V2    
}

# Transforms the input column names into descriptive variable names by:
#   * Fixing variable names that contain 'BodyBody' insted of 'Body'
#   * expanding abbreviations
#   * removing hyphens
#
# Since the resulting variable names are very long, we do not convert them to 
# all lower case but create a CamelCase version of the variables for better 
# readability.
# This takes care of requirment 4): "Appropriately labels the data set with 
# descriptive variable names."
#
# Returns:
#   A chararcter vector with transformed variable names.
expand.colnames <- function(col_names) {
  replacements <- list(
    c("BodyBody", "Body"), 
    c("^t", "time"), 
    c("^f", "frequency"),
    c("-mean\\(\\)", "Mean"),
    c("-std\\(\\)", "Standarddeviation"),
    c("Gyro", "Gyroscope"),
    c("Acc", "Acceleration"),
    c("Mag", "Magnitude"),
    c("-(X|Y|Z)", "\\1")
  )
  for (replacement in replacements) {
    col_names <- sub(pattern = replacement[1], replacement = replacement[2], x = col_names)
  }
  col_names
}





# load column names
colnames <- load.colnames()

# load activity names
activitynames <- load.activitynames()

# load test data set
data.test <- load.dataset(
  file.path(data_dir, "test", "subject_test.txt"),
  file.path(data_dir, "test", "Y_test.txt"),
  file.path(data_dir, "test", "X_test.txt"),
  colnames,
  activitynames
)

# load training data set
data.train <- load.dataset(
  file.path(data_dir, "train", "subject_train.txt"),    
  file.path(data_dir, "train", "Y_train.txt"),
  file.path(data_dir, "train", "X_train.txt"),
  colnames,
  activitynames
)

# combine training and test data as stated in requirement 1):
# "Merges the training and the test sets to create one data set."
data.all <- rbind_list(data.train, data.test)

# expand column names
colnames(data.all) <- expand.colnames(colnames(data.all))

# Satisfy requirement 5): "From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable for each 
# activity and each subject."
means <- data.all %>% 
  group_by(subjectid, activityname) %>% 
  summarise_each(funs(mean), matches("mean|standarddeviation"))


# write the result to a file
write.table(means, file="tidy_data.txt",  row.name=FALSE)