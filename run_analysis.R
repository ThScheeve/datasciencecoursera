directory <- "UCI HAR Dataset"

## Get the activity labels.
activity.labels <- read.table(paste(directory, "/activity_labels.txt", sep = ""))
activity.labels <- as.vector(t(activity.labels[2]))


## Load the data from the train(ing) set.
train.set <- read.table(paste(directory, "/train/X_train.txt", sep = ""))
train.labels <- read.table(paste(directory, "/train/y_train.txt", sep = ""))
train.subject <- read.table(paste(directory, "/train/subject_train.txt", sep = ""))
train.data.set <- cbind(train.subject, train.labels, train.set)


## Load the data from the test(ing) set.
test.set <- read.table(paste(directory, "/test/X_test.txt", sep = ""))
test.labels <- read.table(paste(directory, "/test/y_test.txt", sep = ""))
test.subject <- read.table(paste(directory, "/test/subject_test.txt", sep = ""))
test.data.set <- cbind(test.subject, test.labels, test.set)

## (1) Merge the data from the train(ing) and test(ing) set into 
## one large data set.
data.set <- rbind(train.data.set, test.data.set)


## (3) Name the activities in the data set using descriptive activity names.
activity.id <- data.set[2]
for (i in 1:length(activity.labels)) {
  activity.id[activity.id == i] <- activity.labels[i]
}
data.set[2] <- activity.id


## (4) Label the data set with descriptive variable or feature (column) names.
features <- read.table(paste(directory, "/features.txt", sep = ""))
features <- as.vector(t(features[2]))
colnames(data.set) <- c("subjectID", "activity", features)


## (2) Extract only the columns which are the mean and standard deviation 
## for a measurement.
mean.columns <- grep(pattern = "-mean()", features, value = TRUE, fixed = TRUE)
std.columns <- grep(pattern = "-std()", features, value = TRUE, fixed = TRUE)
data.set <- data.set[c("subjectID", "activity", sort(c(mean.columns, std.columns)))]


## (5) Return the tidy data set with the average of each variable for each 
## activity and each subject.
ids <- sort(unique(data.set$subjectID))
activities <- sort(unique(data.set$activity))

id.and.activity <- data.frame(x = rep(ids, each = 6), y = activities)
means <- data.frame()
for (subject in ids) {
  for (activity in activities) {
    n <- apply(subset(data.set[sort(c(mean.columns, std.columns))], data.set[, 1] == subject & data.set[, 2] == activity), 2, mean)
    means <- rbind(means, n)
  }
}
tidy.data.set <- cbind(id.and.activity, means)
names(tidy.data.set) <- c("subjectID", "activity", sort(c(mean.columns, std.columns)))

write.table(tidy.data.set, "./tidy.txt", sep = "\t")
