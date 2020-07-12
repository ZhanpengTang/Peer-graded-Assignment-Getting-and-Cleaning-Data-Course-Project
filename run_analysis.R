# Coursera: Getting and Cleaning Data Course Project
## Peer Graded Assignment

## Step 1: Merging the training and test sets
### loading feature data
feature <- read.table("./UCI HAR Dataset/features.txt", col.names = c("NO.","Feature"))
head(feature)

### loading activity data
activity <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("Label","Activity"))
head(activity)

### loading train data
rm(X.train)

X.train <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = feature$Feature)
head(X.train)

Y.train <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = c("Label"))
head(Y.train)

subject.train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = c("Subject"))
head(subject.train)

dim(X.train)
dim(Y.train)
dim(subject.train)


names(X.train)
names(Y.train)
names(subject.train)

merged.train <- cbind(subject.train,Y.train,X.train)
dim(merged.train)
head(merged.train)


### loading test data
subject.test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = c("Subject"))
head(subject.test)

X.test <- read.table("./UCI HAR Dataset/test/X_test.txt", col.names = feature$Feature)
head(X.test)

Y.test <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = c("Label"))
head(Y.test)

dim(X.test)
dim(Y.test)
dim(subject.test)

names(X.test)
names(Y.test)
names(subject.test)

merged.test <- cbind(subject.test,Y.test,X.test)
names(merged.test)

### combine test and train data set
tidy.data <- rbind(merged.test, merged.train)
names(tidy.data)

## Step 2: Extracting mean and standard deviation 
mean.std.data <- select(tidy.data, 
                        contains ("Subject"), 
                        contains("Label"), 
                        contains("mean"), 
                        contains("std"))
names(mean.std.data)

mean.std.data$Subject <- as.factor(mean.std.data$Subject)
mean.std.data$Label <- as.factor(mean.std.data$Label)

tidy.avg.data <- mean.std.data %>%
  group_by(Subject, Label) %>%
  summarise_each(funs(mean))

## Step 3: Renaming the activities
names(tidy.avg.data)
names(activity)

Activity.name.tidy.avg.data <- merge(tidy.avg.data, activity, by = "Label")
head(Activity.name.tidy.avg.data)

## Step 4: Labeling the data set

#This step has been done on previous steps. All variables have been labelled as clear names. 

## Step 5: Creating a second, independent tidy data set

Final.data <- aggregate(. ~Subject + Activity + Label, Activity.name.tidy.avg.data, mean)
Final.data <- Final.data[order(Final.data$Subject, Final.data$Label),]

write.table(Final.data, "FinalData.txt", row.name=FALSE)
