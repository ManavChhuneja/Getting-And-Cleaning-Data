# Installing the required packages 
install.packages("tidyverse")

library(tidyverse)
#  Downloading and Extracting the data into the current directory

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName <- "UCI Data Set.zip"

if(!file.exists(fileName)){
  download.file(url,fileName, method = "curl") 
}
unzip(fileName)


#  Reading the data and assigning to variables

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt") 


#  Pre Processing the data as per the instructions

# Merging the main training and test data sets
data <- rbind(x_train,x_test)

# Extracting mesurements on only mean and standard deviation for each value

meanStd <- grep("mean()|std()", features[, 2])
data <- data[, meanStd]

# Cleaning features data set to remove the '()' in the names 
# and assigning the names to variables in the main data set

properFeatNames <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})
names(data) <- properFeatNames[meanStd]

# Removing floating variables that are not going to be used in the script any further
rm("properFeatNames")
rm("meanStd")

# merging the subject and activity data sets and naming the activity as per ID
subjects <- rbind(subject_train, subject_test)
activity <- rbind(y_train, y_test)
names(subjects) <- 'subject'
names(activity) <- 'activity'
activity <- inner_join(activity, activity_labels, by = c('activity' = 'V1'))[,2]

# combining the subjects, activity and the data to create the final data set
data <- cbind(subjects, activity, data)
rm("subjects")
rm("activity")

# Final process of lengthing data and saving it 

final_data <- data %>%
  pivot_longer(cols =  3:length(data), names_to = 'measurement', values_to = 'value') %>%
  group_by(measurement, activity, subject) %>%
  summarise(mean_value = mean(value)) %>%
  select(subject, activity, measurement, mean_value) %>%
  pivot_wider(names_from = measurement, values_from = mean_value)

names(final_data)[-c(1,2)] <- paste("mean of: " , names(final_data)[-c(1:2)])

# saving as csv because csv is more interpretable visially than text files
# and since the number of rows is small enough to fit into a spreadsheet
write.csv(final_data, "tidy_UCI_data.csv")


