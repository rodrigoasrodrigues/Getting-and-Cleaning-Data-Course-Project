run_analysis <- function(){
    
    library(dplyr)
    
    # Acitivy Labels
    activity_labels <- read.table("activity_labels.txt")
    names(activity_labels) = c("ID","NAME")
    
    # Features Labels
    feature_labels <-  read.table("features.txt")
    names(feature_labels) = c("ID","NAME")
    
    # Training
    training_set  <- read.table("train/X_train.txt")
    #Appropriately labels the data set with descriptive variable names. 
    names(training_set) = feature_labels$NAME
        #activity
        #read IDS
        training_activity.ids <- read.table("train/y_train.txt")
        training_activity.ids <- as.numeric(training_activity.ids[[1]])
        
        #Uses descriptive activity names to name the activities in the data set
        #get activity names
        training_activity.names <- activity_labels[training_activity.ids,"NAME"]
        training_activity.subject <- read.table("train/subject_train.txt")
    # merge training data
    training_set <- cbind(training_activity.subject,training_activity.names,training_set)
    names(training_set)[1] <- "Subject"
    names(training_set)[2] <- "Activity"
    
    # Testing
    testing_set  <- read.table("test/X_test.txt")
    #Appropriately labels the data set with descriptive variable names. 
    names(testing_set) = feature_labels$NAME
        #activity
        #read IDS
        testing_activity.ids <- read.table("test/y_test.txt")
        testing_activity.ids <- as.numeric(testing_activity.ids[[1]])
        
        #Uses descriptive activity names to name the activities in the data set
        #get activity names
        testing_activity.names <- activity_labels[testing_activity.ids,"NAME"]
        testing_activity.subject <- read.table("test/subject_test.txt")
        # merge Testing data
        testing_set <- cbind(testing_activity.subject,testing_activity.names,testing_set)
    names(testing_set)[1] <- "Subject"
    names(testing_set)[2] <- "Activity"
    
    #Merges the training and the test sets to create one data set.
    mergeddata_set <- rbind(training_set,testing_set)
    
    #Extracts only the measurements on the mean and standard deviation for each measurement
    #added subject and activity (1:2)
    valid_columns <- c(1:2,grep("std()|mean()",names(mergeddata_set)))
    mergeddata_set <- mergeddata_set[,valid_columns]
    
    #new dataset with the means
    mergeddata_set %>% group_by(Subject,Activity) %>% summarise_each(funs(mean))
    
}