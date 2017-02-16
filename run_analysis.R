## reading the X-Training Dataset(70% of the volunteers participated)
x_trainingDS <- read.table("X_train.txt")

## readind the X-Test Dataset(30% of the volunteers participated)
x_testDS <- read.table("X_test.txt")

## reading the Training Label
y_trainingLabel <- read.table("y_train.txt")

## reading the Test Label
y_testLabel <- read.table("y_test.txt")

## reading the subject_train.txt indicating the subject performed each activity in the Training Dataset
trainingSubjects <- read.table("subject_train.txt")

## reading the subject_test.txt to indicating the subject performed each activity in the Test Dataset
testSubjects <- read.table("subject_test.txt")

## combine the entire Dataset for our measurments
entire_data <- rbind(x_trainingDS, x_testDS)

## combine entire Dataset of the Labels
entire_labels <- rbind(y_trainingLabel, y_testLabel)

## combine entire Dataset of the Subjects who performed the activities
entire_subjects <- rbind(trainingSubjects, testSubjects)

## reading the features.txt containg the information of Mean and Standard Deviation
features <- read.table("features.txt")

## find the rows where Mean and Standard Deviation in spotted and assign them to MeanStd_spots
MeanStd_spots <- grep("mean|std", features[,2])

## acquire the subsetted Dataset where Mean and Standard Deviation was measured
SubData <- entire_data[,MeanStd_spots]

## reading the label corresponding to each activity
activity_labels <- read.table("activity_labels.txt")

## apply a series of function to rename variables names to descriptive activity names e.g WALKING_UPSTAIRS to Walking upstairs
descriptive_activity_labels <- activity_labels
descriptive_activity_labels[,2] <- gsub("_", " ", activity_labels[,2])
substr(descriptive_activity_labels[1,2],2,7) <- tolower(substr(descriptive_activity_labels[1,2],2,7))
substr(descriptive_activity_labels[2,2],2,16) <- tolower(substr(descriptive_activity_labels[2,2],2,16))
substr(descriptive_activity_labels[3,2],2,18) <- tolower(substr(descriptive_activity_labels[3,2],2,18))
substr(descriptive_activity_labels[4,2],2,7) <- tolower(substr(descriptive_activity_labels[4,2],2,7))
substr(descriptive_activity_labels[5,2],2,8) <- tolower(substr(descriptive_activity_labels[5,2],2,8))
substr(descriptive_activity_labels[6,2],2,6) <- tolower(substr(descriptive_activity_labels[6,2],2,6))

## appropriate label all the data with descriptive names and create a unique tidy data
entire_labels[,1] <- descriptive_activity_labels[entire_labels[,1],2]
names(entire_labels) <- "Activity"
names(entire_subjects) <- "SubjectNum"
unique_tidy_data <- cbind(entire_subjects, entire_labels, SubData)
names(unique_tidy_data)[3:81] <- as.character(features[MeanStd_spots,2])

## order my data by activity and then by the number of subject
ordered_tidy_data <- unique_tidy_data[order(unique_tidy_data$Activity, unique_tidy_data$SubjectNum),]

## create my second tidy Dataset
second_tidyDS <- aggregate(. ~SubjectNum + Activity, ordered_tidy_data, mean)

## write my second data set in a text format
write.table(second_tidyDS, "SecondTidyData.txt", row.names = FALSE, quote = FALSE)