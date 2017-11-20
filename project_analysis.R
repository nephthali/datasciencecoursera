packages <- c("data.table", "reshape2", "knitr", "markdown")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
path <- getwd()

###################### Get the Data ##########################
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
if (!file.exists(path)) {dir.create(path)}
#download.file(url, file.path(path, f))
## Unzip the file
#executable <- file.path("C:", "Program Files (x86)", "7-Zip", "7z.exe") #for windows
#executable <- file.path("/usr/bin/unzip")
#cmd <- paste( executable, " ", file.path(path, f)) #target forlder can also being specified
#system(cmd)

pathArc <- file.path(path, "UCI HAR Dataset")
list.files(pathArc, recursive = TRUE)
## Subject file redding
dtSubjectTrain <- fread(file.path(pathArc, "train", "subject_train.txt"))
dtSubjectTest  <- fread(file.path(pathArc, "test" , "subject_test.txt" ))
## Activity file reading
fileToDataTable <- function (f) {
        df <- read.table(f)
        dt <- data.table(df)
} # Alternatively if fread give some error...dtTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
dtActivityTrain <- fread(file.path(pathArc, "train", "y_train.txt"))
dtActivityTest  <- fread(file.path(pathArc, "test" , "y_test.txt" ))
dtTrain <- fread(file.path(pathArc, "train", "X_train.txt"))
dtTest  <- fread(file.path(pathArc, "test" , "X_test.txt" ))

###################### Merge training and test sets ######################
## Merge columns
setnames(dtSubjectTrain, "V1", "subject")
setnames(dtSubjectTest, "V1", "subject")
setnames(dtActivityTrain, "V1", "activityNum")
setnames(dtActivityTest, "V1", "activityNum")
dtSubjectTrain1 <-cbind(dtSubjectTrain, dtActivityTrain, dtTrain)
dtSubjectTest1 <-cbind(dtSubjectTest, dtActivityTest, dtTest)

## Merge rows
dt <- rbind(dtSubjectTrain1, dtSubjectTest1)

## Set key
setkey(dt, subject, activityNum)

#################### Extract only the mean and standard deviation ################
dtFeatures <- fread(file.path(pathArc, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
## Convert the column numbers to a vector of variable names matching columns in dt dataset
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
## Subset mean and standard deviation variables using variable names.
vars <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, vars, with=FALSE]

################### Use descriptive activity names ###############################
dtActivityNames <- fread(file.path(pathArc, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

#### Merge activity labels.
dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)
### Add activityName as a key
setkey(dt, subject, activityNum, activityName)
### Melt the data table to reshape it from a short and wide format to a tall and narrow format.
dt <- melt(dt, key(dt), variable.name="featureCode")
### Add variable featureNum and featureName to dataset merging by featureCode.
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)
### Create a new variables, activity and feature that are equivalent to activityName and featureName as a factor class.
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)
### Seperate features from featureName
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
## Extract time, frequence domain
x <- matrix(c(grepl("^t", dt$feature), grepl("^f", dt$feature)), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
## Extract Acceleration, gyroscope features
x <- matrix(c(grepl("Acc", dt$feature),  grepl("Gyro", dt$feature)), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
## Extract Body and Gravity accceleration features
x <- matrix(c(grepl("BodyAcc", dt$feature), grepl("GravityAcc", dt$feature)), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
## Extract mean and standard deviation features
x <- matrix(c(grepl("mean()", dt$feature), grepl("std()", dt$feature)), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Extract Jerk and Magnitude features
dt$featJerk <- factor(grepl("Jerk", dt$feature), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepl("Mag", dt$feature), labels=c(NA, "Magnitude"))

## Features with 3-axial signals
n <- 3
y <- matrix(seq(1, n), nrow=n)
## Extract X,Y,Z axial features
x <- matrix(c(grepl("-X", dt$feature), grepl("-Y", dt$feature),grepl("-Z", dt$feature)), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))
### Check to make sure all possible combinations of the factor class variables are matched.
r1 <- nrow(dt[, .N, by=c("feature")])
r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

############################### Create a tidy data set #############################
## Create a data set with the average of each variable for each activity and each subject.
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]
write.table(dtTidy, "tidyDataset.txt", row.names = FALSE)

############################## Make codebook #######################################
knit("makeCodebook.Rmd", output="codebook.md", encoding="ISO8859-1", quiet=TRUE)
markdownToHTML("codebook.md", "codebook.html")