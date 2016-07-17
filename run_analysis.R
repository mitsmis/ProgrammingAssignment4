# Data was downloaded the old fashioned way and placed in
# appropriate Folder in order to complete the analysis.

# (features) Read and Bind the test and training datasets together
# Leverage 'read.table' and 'rbind' to do this.
x.train <- read.table('./UCI HAR Dataset/train/X_train.txt')
x.test <- read.table('./UCI HAR Dataset/test/X_test.txt')
features <- rbind(x.train, x.test)

# Examine the properties of 'features' to see if it worked
# Leverage 'str' to accomplish this
str(features)

# (subjects) Read and Bind the test and training datasets together
# Leverage 'read.table' and 'rbind' to do this
subj.train <- read.table('./UCI HAR Dataset/train/subject_train.txt')
subj.test <- read.table('./UCI HAR Dataset/test/subject_test.txt')
subjects <- rbind(subj.train, subj.test)

# Examine the properties of 'subjects' to see if it worked
# Leverage 'str' to accomplish this
str(subjects)

# (activities) Read and Bind the properties and training
# Leverage 'read.table' and 'rbind' to do this.
y.train <- read.table('./UCI HAR Dataset/train/y_train.txt')
y.test <- read.table('./UCI HAR Dataset/test/y_test.txt')
activities <- rbind(y.train, y.test)

# Examine the properties of 'activities' to see if it worked
# Leverage 'str' to accomplish this
str(activities)

# Add Variable Headers to the data
# I leveraged the 'names' function to do this
names(subjects) <- c("subject")
names(activities) <- c("activity")
featuresName <- read.table('./UCI HAR Dataset/features.txt', 
                           head = FALSE)
names(features) <- featuresName$V2

# Now to combine the cleaned up disparate data sets into one
# super-amazing tidy dataset

# Step 1: Combine the 'subjects' and 'activities' data sets.
# Leverage 'cbind'
combSubAct <- cbind(subjects, activities)

# Step 2: Combine 'combSubAct' and 'features' dataset
# Leverage 'cbind'
data <- cbind(features, combSubAct)

# Extraction of only the measurements on the mean and standard
# deviation for each measurement
# Leverage 'grep', regular expression syntax
extractFeatureNames <- featuresName$V2[grep("mean\\(\\)|std\\(\\)", featuresName$V2)]

# Subset data to get only the features desired
# Step 1: Get the elements we want from our 'data' set headers into
# a character vector which we will feed to a subset command on our riginal data.
# names must match perfectly.
subNames <- c(as.character(extractFeatureNames), "subject", "activity")

# Step 2: Subset our data into subdata using 'subNames' as the second
# argument in our 'subset' function. The first argument will be our 'data'
# we defined earlier.
subdata <- subset(data, select = subNames)

# Check the 'subdata' set
# Leverages 'str' function
str(subdata)
# Thank God this finally validated. I had a hell of a time with
# approriate regular expression logic three steps above.

# Bring in 'activity_labels.txt'
actLabels <- read.table('./UCI HAR Dataset/activity_labels.txt', 
                        head = FALSE)
# Update the 'activity' variable in actitivies as a factor
# Label the activity using the actLabel, did this using 'plyr' revalue
subdata$activity <- as.factor(subdata$activity)

subdata$activity <- revalue(subdata$activity, c("1" = "walking", 
                                                "2" = "walking_upstairs",
                                                "3" = "walking_downstairs",
                                                "4" = "sitting",
                                                "5" = "standing",
                                                "6" = "laying"))

# Check Structure of Data
str(subdata)

# Rename Variables to Better Give End User An Idea of what they are looking at
# As in the documentation for the dataset found on the UCI Machine Learning Repo
# I will be changing variables that that start with 't' to start with 'time', variables
# that start with 'f' with 'frequency'. Leverage 'gsub' to do this.

# Step 1: Examine Current Naming Convention
namesdf <- as.data.frame(names(subdata))

# Step 2: Replace 't' with 'time' using 'gsub'
names(subdata) <- gsub("^t", "time", names(subdata))

# Step 3: Examine Changes
namesdf <- as.data.frame(names(subdata))

# Step 4: Replace 'f' with 'freq' using 'gsub'
names(subdata) <- gsub("^f", "freq", names(subdata))

# Step 5: Examine Changes
namesdf <- as.data.frame(names(subdata))

# Note: Noticed duplication of Body in some variable names
# I changed "BodyBody" to "Body" to tidy it up the names a bit
# Step 6: Replace 'BodyBody' with 'Body' using gsub
names(subdata) <- gsub("BodyBody", "Body", names(subdata))

# Step 7: Examine Changes
namesdf <- as.data.frame(names(subdata))

# Step 8: Replace 'Acc' with 'Acceleration' using 'gsub'
names(subdata) <- gsub("Acc","Acceleration", names(subdata))
# Step 9: Replace 'Gyro' with 'Angularvelocity' using 'gsub'
names(subdata) <- gsub("Gyro","AngularVelocity", names(subdata))

# Finally, I can create a second, independent tidy data set with the average of
# each variable for eah activity and each subject by leveraging the power of the
# 'plyr' package.
library(plyr)

# Leverage the power of the 'aggregate' the data.
subdata2 <- aggregate(. ~subject + activity, subdata, mean)

# Sort the data, so 'subjects' are ordered together.
subdata2 <- subdata2[order(subdata2$subject,subdata2$activity),]

# Write the Data to File
write.table(subdata2, file = "tidydata.txt", row.names = FALSE)