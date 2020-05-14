

library(readr)
library(data.table)
library(dplyr)
library(tidyr)

# Set Up File and Folder values ===========================================================================

#Data folder Location
data_folder = "./UCIHARDataset"

#Common files
features_file = "/features.txt"
activity_label_file = "/activity_labels.txt"


#Train and Test Datasets
train_x_data = "/train/X_train.txt"
test_x_data = "/test/X_test.txt"

train_y_label = "/train/y_train.txt"
test_y_label = "/test/y_test.txt"

train_subjects = "/train/subject_train.txt"
test_subjects = "/test/subject_test.txt"





# Requirement 1: Function to Merge training and test datasets. =============================================

rbind_datasets = function(folder_location, file1, file2){
  
  # Using function to read two similar dataset files to bind them
    if(!file.exists(folder_location)){ return("folder path error")}
  fullpath_file1 = file.path(folder_location,file1)
  fullpath_file2 = file.path(folder_location,file2)
  data1 = data2 = NULL
  
  if(!file.exists(fullpath_file1) | !file.exists(fullpath_file2)){
    return("file name errors")
  }
  
  data1 = read.table(fullpath_file1)
  data2 = read.table(fullpath_file2) 
  
  if(length(data1)!=length(data2)){
    return("The datasets have different number of columns")
  }
  rbind(data1,data2)
}


# Use rbind_datasets function to merge similar files

#subjects 
total_subjects = rbind_datasets(data_folder,train_subjects,test_subjects)

#labels
total_y_labels = rbind_datasets(data_folder, train_y_label,test_y_label)

#observations
total_x_data = rbind_datasets(data_folder, train_x_data,test_x_data)






# Requirement 2: Extracts only the observations on the mean and standard deviation for each observation. ===========

# Load Features
data_features <- read.csv(paste0(data_folder, features_file), header = FALSE, sep = " ")

# Only features with mean and std have been isolated
features_col <- data_features[grepl("std()",data_features$V2) ==TRUE | grepl("mean()",data_features$V2) ==TRUE,1]
features_col_desc <- data_features[grepl("std()",data_features$V2) ==TRUE | grepl("mean()",data_features$V2) ==TRUE,2]

# select only data based on features filter 
total_x_data = total_x_data %>% select(features_col)



# Requirement 3: Uses descriptive activity names to name the activities in the data set =============================

# Load Activity Labels
data_activity_labels <- read.csv(paste0(data_folder, activity_label_file), header = FALSE, sep = " ")                         
# head(activity_labels) 
 

# change integers labels into text values
total_y_labels = data_activity_labels[match(total_y_labels[,1],data_activity_labels[,1]),2]



# Requirement 4: Appropriately labels the data set with descriptive variable names. ================================


# bind labels with observations
total_dataset = cbind(total_x_data,total_y_labels,total_subjects)

# Give descriptive names to variables
names(total_dataset) =c(as.character(features_col_desc),"activitylabel","subject")



# Requirement 5: From the data set in step 4, creates a second, independent tidy data set =========================
# with the average of each variable for each activity and each subject. ===========================================


total_dataset_avg_by_activity_subject = total_dataset %>% group_by(activitylabel,subject) %>% summarize_all(funs(mean))


write.table(total_dataset_avg_by_activity_subject, file = "total_dataset_avg_by_activity_subject.txt", row.names=FALSE)
