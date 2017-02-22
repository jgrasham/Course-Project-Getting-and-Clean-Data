#
# The R Run Script for Getting and Cleaning Data Course Project
#
# 1. Combines the test and training data sets to create a single data set
# 2. Pulls out just the measurements for mean and standard deviation
# 3. Uses the activity names to categorize the observations in the data set
# 4. Labels the data set table with more descriptive variable (column) names
# 5. Makes a tidy data set that calculates the average of each variable for each activity/subject combo
#
# Requires that the plyr package is installed and will
# Use the plyr package for splitting, applying and combining
# Use install.packages(plyr) if not yet installed
#
library(plyr)
#
# Read the x_tst table from a local folder location
# If desired, file name can be selected using the choose.files() option to select a file - see sample code below the read.table statement
# table is 2947 observations (rows) of 561 variables
# table is structured with columns labelled: v1 v2 v3 v4 v5 ... v561 each containing a real number
#
x_tst <- read.table("C://Users/jgras/OneDrive/Documents/data/UCI HAR Dataset/test/x_test.txt")
# x_tst <- read.table(choose.files())
#
# Read the y_tst table
# If desired, file name can be selected using the choose.files() option to select a file - see sample code below the read.table statement
# table is 2947 observations of 1 variable
# table is structured with one column labelled: v1 containing a number from 1-6
# the numbers in y_test correspond to the activities in the actvlbls table 
#
y_tst<-read.table("C://Users/jgras/OneDrive/Documents/data/UCI HAR Dataset/test/y_test.txt")
# y_tst <- read.table(choose.files())
#
# Read the subj_tst table
# If desired, file name can be selected using the choose.files() option to select a file - see sample code below the read.table statement
# table is 2947 observations of 1 variable
# table is structured with one column labelled: v1 containing a number from 1-30 
# the numbers in subj_tst from 1-30 (represents the # of the participating subject)
#
subj_tst <-read.table("C://Users/jgras/OneDrive/Documents/data/UCI HAR Dataset/test/subject_test.txt")
# subj_tst <- read.table(choose.files())
#
# Read the training data tables
# If desired, file name can be selected using the choose.files() option to select a file - see sample code below the read.table statement
# the x_train file goes into the x_trn table
# x_trn table is 7352 observations of 561 variables
# table is structured with columns labelled: v1 v2 v3 v4 v5 ... v561 each containing a real number
#
x_trn <- read.table("C://Users/jgras/OneDrive/Documents/data/UCI HAR Dataset/train/X_train.txt")
# x_trn <- read.table(choose.files())
#
# the y_train file goes into the y_trn table
# If desired, file name can be selected using the choose.files() option to select a file - see sample code below the read.table statement
# y_trn table is 7352 observations of 1 variable
# table is structured with one column labelled: v1 containing a number from 1-6
#
y_trn<-read.table("C://Users/jgras/OneDrive/Documents/data/UCI HAR Dataset/train/y_train.txt")
# y_trn <- read.table(choose.files())
#
# Read the subj_trn table
# If desired, file name can be selected using the choose.files() option to select a file - see sample code below the read.table statement
# table is 7352 observations of 1 variable
# table is structured with one column labelled: v1 containing a number from 1-30
# the numbers in subj_trn (1-30) represents the # of the participating subject
#
subj_trn <- read.table("C://Users/jgras/OneDrive/Documents/data/UCI HAR Dataset/train/subject_train.txt")
# subj_trn <- read.table(choose.files())
#
# Read the feature table into table ftrs
# If desired, file name can be selected using the choose.files() option to select a file - see sample code below the read.table statement
# the feature table consists of variables v1 and v2
# v1 = an feature index; v2 = a feature name; these will be used as the new column names
#
ftrs <- read.table("C://Users/jgras/OneDrive/Documents/data/UCI HAR Dataset/features.txt")
# ftrs <- read.table(choose.files())
#
# Read the activity labels into table actvlbls
# If desired, file name can be selected using the choose.files() option to select a file - see sample code below the read.table statement
# the actvlbls labels table consists of variables v1 and v2
# v1 = a activity label index; v2 = an activity name; these will be used as the new activity names
#
actvlbls <- read.table("C://Users/jgras/OneDrive/Documents/data/UCI HAR Dataset/activity_labels.txt")
# actvlbls <- read.table(choose.files())
#
# Combine the training and test data tables
# x_dat will consist of x_tst with x_trn concatenated onto it using rbind
# y_dat will consist of y_tst with y_trn concatenated onto it using rbind
# subj_dat will consist of subj_tst with subj_trn concatenated onto it using rbind
# x_dat will be 10299 observations of 66 variables
# y_dat will be 10299 observations of 1 variables
# subj_dat will be 10299 observations of 1 variable
#
x_dat <- rbind(x_tst,x_trn)
y_dat <- rbind(y_tst,y_trn)
subj_dat <- rbind(subj_tst,subj_trn)
#
# get only the columns that have 'mean()' or 'std()' in their names
# put those columns into a temp list named mean_std_ftrs
# use that list to load the x_dat table with only those columns
# then use the names function to change the names of the columns to the the more descriptive names
#
mean_std_ftrs <- grep("-(mean|std)\\(\\)",ftrs[, 2])
x_dat <- x_dat[,mean_std_ftrs]
names(x_dat) <- ftrs[mean_std_ftrs,2]
#
# Use descriptive activity names to label the activities in the data set
# Use the look up values in ydat[,1] to replace with the activity labels 
# rename column name to 'activity'
#
y_dat[,1] <- actvlbls[y_dat[,1],2]
names(y_dat) <- "activity"
#
# change the column name in subj_dat to 'subject'
#
names(subj_dat) <- "subject"
#
# combine the data in x_dat, y_dat and subj_dat into a table named all_dat
#
all_dat <- cbind(x_dat, y_dat, subj_dat)
#
# create the tidy data set with the average of each variable
# for each activity and each subject
# using the ddply function - group on the subject and activity columns
# put the results in table avg_dat
#
avg_dat <- ddply(all_dat, .(subject, activity), function(x) colMeans(x[, 1:66]))
#
# write the table avg_dat to the data file 'avg_dat.txt'
#
write.table(avg_dat, "avg_dat.txt", row.name=FALSE)
