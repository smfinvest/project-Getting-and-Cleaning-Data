#function setup
# INPUT: X and Y data and list of subjects
# RETURN: a complete data.frame

library(plyr)
library(data.table)

setup<-function(X, Y, S) {
  df           <- read.table(X)
  features     <- read.table(".\\features.txt",colClasses=c("numeric","character"))
  features$V2  <- gsub("\\(","",features$V2)
  features$V2  <- gsub("\\)","",features$V2)
  features$V2  <- gsub("\\,","_",features$V2)
  features$V2  <- gsub("-","_",features$V2)
  colnames(df) <- features$V2
  Activity     <- scan(Y)
  act          <- read.table("activity_labels.txt",colClasses=c("numeric","character"))
  subject      <- scan(S)
  Activity     <- mapvalues(Activity, c(1:6), c(act$V2))
  # merge the subject and Activity columns with the dataframe
  cbind(subject, Activity,df) 
}

#fill data
traindata <- setup(".\\train\\X_train.txt", ".\\train\\Y_train.txt", ".\\train\\subject_train.txt")
testdata  <- setup(".\\test\\X_test.txt"  , ".\\test\\Y_test.txt"  , ".\\test\\subject_test.txt")

# merge the data.frames into one data set
merged<-rbind(traindata, testdata)
col <- colnames(merged) 
g<-grep("mean", col, ignore.case=TRUE) 
s<-grep("std", col, ignore.case=TRUE)
# create a set with only mean and standard deviation for each measurement
mean_std<-merged[,c(1,2,g,s)]

# create new data set with the average of each measurement grouped by activity and subject
DT<-data.table(mean_std)
setkeyv(DT, c("subject","Activity"))
average_of_set<-DT[,lapply(.SD,mean),by=key(DT),.SDcols=3:ncol(DT)]
write.table(average_of_set, "project.csv", quote=FALSE, sep=",", row.name= FALSE)
