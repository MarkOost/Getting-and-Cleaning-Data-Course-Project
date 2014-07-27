#First Dataset

#Load data
Train <- read.table("getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\X_train.txt")
TrainSubject <- read.table("getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\y_train.txt")

Test <- read.table("getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\X_test.txt")
TestSubject <- read.table("getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\y_test.txt")

#RBind the data
Subject <- rbind(Train,Test)
Activity <- rbind(TrainSubject, TestSubject)

#Load the labels for Acitvity and Subject data
labels <- read.table("getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\features.txt")
labelsactivity <- read.table("getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\activity_labels.txt")

#Add the labels
names(Subject) <- paste(labels$V2)

#Subsetting mean and sd's
SubsetMean <- Subject[,grep("mean()", colnames(Subject))] 
SubsetStd <- Subject[,grep("std()", colnames(Subject))] 

Subset <- cbind(SubsetMean, SubsetStd)

#First binding the two datasets
TotalSet <-cbind(Subset, Activity) 

#Merge Activity Labels wit data, this is done now because it re arrange the activity data
TotalSet1 <- merge(TotalSet, labelsactivity, by="V1", all = TRUE)

#Subsetting data again the rearrange the columns to create tidy dataset
SubsetActivityDesc <- data.frame(TotalSet1[,81])
names(SubsetActivityDesc) <- paste("Activity")
SubsetOther <- TotalSet1[,2:80]

#Tidy dataset is complete
TidyDataset <- cbind(SubsetActivityDesc, SubsetOther)

#Check dataset
summary(TidyDataset)
str(TidyDataset)

#If check is ok output data to
write.table(TidyDataset, "Dataset1.txt")

#----------------------
  
#Second Dataset

#Load data
Train <- read.table("getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\X_train.txt")
TrainActivity <- read.table("getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\y_train.txt")
TrainSubject <- read.table("getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\subject_train.txt")

Test <- read.table("getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\X_test.txt")
TestActivity <- read.table("getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\y_test.txt")
TestSubject <- read.table("getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\subject_test.txt")


#RBind the data
TestTrain <- rbind(Train,Test)
Activity <- rbind(TrainActivity, TestActivity)
Subject <- rbind(TrainSubject, TestSubject)

#Load the labels for Acitvity and TestTrain data
labels <- read.table("getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\features.txt")
labelsactivity <- read.table("getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\activity_labels.txt")

#Add the labels
names(TestTrain) <- paste(labels$V2)
names(Subject) <- paste(c("SubjectID"))

#Subsetting mean and sd's
SubsetMean <- TestTrain[,grep("mean()", colnames(TestTrain ))] 
SubsetStd <- TestTrain[,grep("std()", colnames(TestTrain ))] 
Subset <- cbind(SubsetMean, SubsetStd)

#First binding the three datasets
PartSet <-cbind(Subset, Activity) 
TotalSet <-cbind(PartSet, Subject) 


#Subsetting data again the rearrange the columns to create tidy dataset
SubsetActivityDesc <- data.frame(TotalSet[,80])
names(SubsetActivityDesc) <- paste("Activity")
SubsetOther <- TotalSet[,1:79]
SubsetSubject <- data.frame(TotalSet[,81])
names(SubsetSubject) <- paste(c("SubjectID"))

#Binding data in right order
FirstBind <- cbind(SubsetActivityDesc, SubsetSubject)
SecondBind <- cbind(FirstBind, SubsetOther)
                                      
#reshape data to get for every subject and activity the avarage 
require(reshape2)
require(plyr)
SetMelt <- melt(SecondBind, id=c(1:2), measure.vars=c(3:81))
TidyDataSet <- ddply(SetMelt, .(SubjectID,Activity,variable), summarize, mean = mean(value))

#Check data
head(TidyDataSet)

#If check is ok output data to
write.table(TidyDataSet, "Dataset2.txt")

