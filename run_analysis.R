##################################################################
# 1. Merges the training and the test sets to create one data set.
##################################################################

####### Getting the data from the web
## 1.a. Create a directory Data
if(!file.exists("./Samsung data")) {dir.create("./Samsung data")}
## 1.b. Creates an object with internet link
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
## 1.c. Download internet file in the Data folder
download.file(fileUrl, destfile = "./Samsung data/Dataset.zip", method = "curl")
## 1.d. Unzip the file
unzip ("./Samsung data/Dataset.zip")

## 1.e. Reads a directory full of files 
testf <- list.files("UCI HAR Dataset/test", full.names = TRUE)
trainf <- list.files("UCI HAR Dataset/train", full.names = TRUE)

#### read the train dataset
# read the subject_train.txt
trainsubject <- read.table(trainf[2])
# add varname the subject train dataset
names(trainsubject) <- "subject"
# read the labels dataset  (y_train.txt)
trainlabels <- read.table(trainf[4])
# add varname the labels  dataset
names(trainlabels) <- "activitylabels"
# read the X_train dataset
traindat <- read.table(trainf[3])
# column binding the train dataset
train <- cbind(traindat, trainlabels, trainsubject)

#### read in r test dataset
# read the subject_test.txt
testsubject <- read.table(testf[2])
# add varname the subject train dataset
names(testsubject) <- "subject"
# read the labels dataset (y_test.txt)
testlabels <- read.table(testf[4])
# add varname the labels  dataset
names(testlabels) <- "activitylabels"
# read the X_test dataset
testdat <- read.table(testf[3])
# column binding the train dataset
test <- cbind(testdat, testlabels, testsubject)

# Merges the training and the test sets
smartphones <- rbind(train, test)  


############################################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
############################################################################################

# check the size of a dataset
print(object.size(smartphones), units = "Mb") # 44.2 Mb
# load the data.table package
library(data.table)
# makes dataframa as.data.table(= data.frame)
smartph <- as.data.table(smartphones)
# calculate the mean and standard deviation(sd) for each variable
meansd <- sapply(smartph, function(x) rbind(mean(x), sd(x)))
# review the class the object
class(meansd)
# add names
rownames(meansd) <- c("mean", "sd")
meansd
## save the file
save("smartph", file = "smartph.RData")
# remove the objects in the global enviroment
rm(list = ls())


###########################################################################
# 3. Uses descriptive activity names to name the activities in the data set
###########################################################################

# load the data.table
load("smartph.RData")
# Table number of observations per activity before changing codes
table(smartph$activitylabels)
# We change the codes by the name of activities, 
# 1 WALKING
# 2 WALKING_UPSTAIRS
# 3 WALKING_DOWNSTAIRS
# 4 SITTING
# 5 STANDING
# 6 LAYING
smartph$activitylabels <- ifelse(smartph$activitylabels == 1, "walking",
                                 ifelse(smartph$activitylabels == 2, "walking_upstairs",
                                        ifelse(smartph$activitylabels == 3, "walking_downstairs",
                                               ifelse(smartph$activitylabels == 4, "sitting",
                                                      ifelse(smartph$activitylabels == 5, "standing", "laying")))))
# Table number of observations per activity after changing codes
table(smartph$activitylabels)

## save the file with changes
save("smartph", file = "smartph.RData")

# remove the objects in the global enviroment
rm(list = ls())


#######################################################################
# 4. Appropriately labels the data set with descriptive variable names.
#######################################################################
# load the data.table
load("smartph.RData")
# We note the names of the columns before adding the names of the variables
names(smartph)
# read the features.txt, that contain the descriptive variable names
datasetUCI <- list.files("UCI HAR Dataset", full.names = TRUE)
features <- read.table(datasetUCI[3], stringsAsFactors = FALSE)
# smartphone named columns with vector character features
names(smartph)[1:561] <- features[,2]
# We note the names of the columns before adding the names of the variables
names(smartph)
## save the file with changes
save("smartph", file = "smartph.RData")
# remove the objects in the global enviroment
rm(list = ls())


#############################################################################
# 5. From the data set in step 4, creates a second, independent tidy data 
#    set with the average of each variable for each activity and each subject.
#############################################################################
# load the data.table
load("smartph.RData")
table(smartph$activitylabels, smartph$subject)
# create independent tidydata
labels <- colnames(smartph)[-c(562,563)]
tidydata <- lapply(labels, function(x) tapply(smartph[[x]], list(smartph$activity, smartph$subject), mean))
names(tidydata) <- labels
tidydata
## data set as a txt file created with write.table() 
write.table(tidydata, file = "tidydata.txt", row.name=FALSE)
# remove the objects in the global enviroment
rm(list = ls())
