
#RUN ANALYSIS PROJECT
_author:"Zack S. Baker"_


## Overview

As part of Getting and Cleaning Data, this project entails downloading accelerometer data collected from a number of galaxy smartphones.


## run_analysis.R Script



### Data preparation

The information was downloaded from the following site link to the [HAR Using Smartphones  Dataset](http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip).

Folder structure once unzipped, included a train and test fold paths with equal sets of data for labels, observations and subjects. 

Within the main folder, the activity labels and features was used to discern results observed. 

The following folder paths were represented by character vectors:



#### Data folder Location
**data_folder**= parent folder of all data files used 



#### Common files
**features_file** = file with feature names

**activity_label_file** = file with activity labels



#### Train and Test Datasets
**train_x_data** = train observation data

**test_x_data** = test observation data

**train_y_label** = train label data

**test_y_label** = test label data

**train_subjects** = train subjects

**test_subjects** = test subjects




### Function Call

The rbind_datasets function was used to merge train and test data with similar structure. 
This was used to bind subjects, labels and observations

```{r}
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
```




### Variables



#### Features and Activity Labels

**data_features** = read in the features files which lists all the "columns" of data observed

**features_col** = Only features with mean and std have been isolated, this is the numeric value which will be used against the labels data

**features_col_desc** = Only features with mean and std have been isolated, this data will be used to translate the column names into a readable format 

**data_activity_labels** : contains tabel mapping between activity code and text label



#### Joined data

**total_x_data** : contains joined train and test observations (without labels or subjects)

**total_y_labels** : contains joined labels for train and test observations

**total_subjects** : contains joined list of subjects from train and test observations



#### Final tidy datasets

**total_dataset** : contains total set of observations with activity labels and subject numbers included

**total_dataset_avg_by_activity_subject** : dataset that takes the total_dataset and groups by activity and subject; and _ultimately_ obtaining the mean value of each features by grouping.




### Summary

For a review of the codebook for the total_dataset with a sample of 5 variable outputs, see the [Brief Total Dataset Codebook](http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip).


