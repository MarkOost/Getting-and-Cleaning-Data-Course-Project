#This ReadMe file will explain how the analysis and creation of the databases was done.

##First Dataset
The first dataset was the dataset that needed to be created by merging the following files
1. X_train.txt
2. y_train.txt
3. X_test.txt
4. y_test.txt

The code below will create the desired database in Text format. When you run the code below and have the data in your user folder a database, Dataset1.txt will be created.

please see CodeBook.md for the precise format of the data.

You will also find summary statisics of the data below the code so you can check if the database that is reproduced is exactly the one you should expect from this document.


```r
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
head(TidyDataSet)
```

```
##   SubjectID Activity             variable     mean
## 1         1        1    tBodyAcc-mean()-X  0.27733
## 2         1        1    tBodyAcc-mean()-Y -0.01738
## 3         1        1    tBodyAcc-mean()-Z -0.11115
## 4         1        1 tGravityAcc-mean()-X  0.93522
## 5         1        1 tGravityAcc-mean()-Y -0.28217
## 6         1        1 tGravityAcc-mean()-Z -0.06810
```

```r
summary(TidyDataset)
```

```
##                Activity    tBodyAcc-mean()-X tBodyAcc-mean()-Y
##  LAYING            :1944   Min.   :-1.000    Min.   :-1.0000  
##  SITTING           :1777   1st Qu.: 0.263    1st Qu.:-0.0249  
##  STANDING          :1906   Median : 0.277    Median :-0.0172  
##  WALKING           :1722   Mean   : 0.274    Mean   :-0.0177  
##  WALKING_DOWNSTAIRS:1406   3rd Qu.: 0.288    3rd Qu.:-0.0106  
##  WALKING_UPSTAIRS  :1544   Max.   : 1.000    Max.   : 1.0000  
##  tBodyAcc-mean()-Z tGravityAcc-mean()-X tGravityAcc-mean()-Y
##  Min.   :-1.0000   Min.   :-1.000       Min.   :-1.000      
##  1st Qu.:-0.1210   1st Qu.: 0.812       1st Qu.:-0.243      
##  Median :-0.1086   Median : 0.922       Median :-0.144      
##  Mean   :-0.1089   Mean   : 0.669       Mean   : 0.004      
##  3rd Qu.:-0.0976   3rd Qu.: 0.955       3rd Qu.: 0.119      
##  Max.   : 1.0000   Max.   : 1.000       Max.   : 1.000      
##  tGravityAcc-mean()-Z tBodyAccJerk-mean()-X tBodyAccJerk-mean()-Y
##  Min.   :-1.0000      Min.   :-1.0000       Min.   :-1.0000      
##  1st Qu.:-0.1167      1st Qu.: 0.0630       1st Qu.:-0.0186      
##  Median : 0.0368      Median : 0.0760       Median : 0.0108      
##  Mean   : 0.0922      Mean   : 0.0789       Mean   : 0.0079      
##  3rd Qu.: 0.2162      3rd Qu.: 0.0913       3rd Qu.: 0.0335      
##  Max.   : 1.0000      Max.   : 1.0000       Max.   : 1.0000      
##  tBodyAccJerk-mean()-Z tBodyGyro-mean()-X tBodyGyro-mean()-Y
##  Min.   :-1.0000       Min.   :-1.0000    Min.   :-1.0000   
##  1st Qu.:-0.0316       1st Qu.:-0.0458    1st Qu.:-0.1040   
##  Median :-0.0012       Median :-0.0278    Median :-0.0748   
##  Mean   :-0.0047       Mean   :-0.0310    Mean   :-0.0747   
##  3rd Qu.: 0.0246       3rd Qu.:-0.0106    3rd Qu.:-0.0511   
##  Max.   : 1.0000       Max.   : 1.0000    Max.   : 1.0000   
##  tBodyGyro-mean()-Z tBodyGyroJerk-mean()-X tBodyGyroJerk-mean()-Y
##  Min.   :-1.0000    Min.   :-1.0000        Min.   :-1.0000       
##  1st Qu.: 0.0648    1st Qu.:-0.1172        1st Qu.:-0.0587       
##  Median : 0.0863    Median :-0.0982        Median :-0.0406       
##  Mean   : 0.0884    Mean   :-0.0967        Mean   :-0.0423       
##  3rd Qu.: 0.1104    3rd Qu.:-0.0793        3rd Qu.:-0.0252       
##  Max.   : 1.0000    Max.   : 1.0000        Max.   : 1.0000       
##  tBodyGyroJerk-mean()-Z tBodyAccMag-mean() tGravityAccMag-mean()
##  Min.   :-1.0000        Min.   :-1.000     Min.   :-1.000       
##  1st Qu.:-0.0794        1st Qu.:-0.982     1st Qu.:-0.982       
##  Median :-0.0546        Median :-0.875     Median :-0.875       
##  Mean   :-0.0548        Mean   :-0.548     Mean   :-0.548       
##  3rd Qu.:-0.0317        3rd Qu.:-0.120     3rd Qu.:-0.120       
##  Max.   : 1.0000        Max.   : 1.000     Max.   : 1.000       
##  tBodyAccJerkMag-mean() tBodyGyroMag-mean() tBodyGyroJerkMag-mean()
##  Min.   :-1.000         Min.   :-1.000      Min.   :-1.000         
##  1st Qu.:-0.990         1st Qu.:-0.978      1st Qu.:-0.992         
##  Median :-0.948         Median :-0.822      Median :-0.956         
##  Mean   :-0.649         Mean   :-0.605      Mean   :-0.762         
##  3rd Qu.:-0.296         3rd Qu.:-0.245      3rd Qu.:-0.550         
##  Max.   : 1.000         Max.   : 1.000      Max.   : 1.000         
##  fBodyAcc-mean()-X fBodyAcc-mean()-Y fBodyAcc-mean()-Z
##  Min.   :-1.000    Min.   :-1.000    Min.   :-1.000   
##  1st Qu.:-0.991    1st Qu.:-0.979    1st Qu.:-0.983   
##  Median :-0.946    Median :-0.864    Median :-0.895   
##  Mean   :-0.623    Mean   :-0.537    Mean   :-0.665   
##  3rd Qu.:-0.265    3rd Qu.:-0.103    3rd Qu.:-0.366   
##  Max.   : 1.000    Max.   : 1.000    Max.   : 1.000   
##  fBodyAcc-meanFreq()-X fBodyAcc-meanFreq()-Y fBodyAcc-meanFreq()-Z
##  Min.   :-1.0000       Min.   :-1.0000       Min.   :-1.0000      
##  1st Qu.:-0.4188       1st Qu.:-0.1448       1st Qu.:-0.1384      
##  Median :-0.2383       Median : 0.0047       Median : 0.0608      
##  Mean   :-0.2215       Mean   : 0.0154       Mean   : 0.0473      
##  3rd Qu.:-0.0204       3rd Qu.: 0.1766       3rd Qu.: 0.2492      
##  Max.   : 1.0000       Max.   : 1.0000       Max.   : 1.0000      
##  fBodyAccJerk-mean()-X fBodyAccJerk-mean()-Y fBodyAccJerk-mean()-Z
##  Min.   :-1.000        Min.   :-1.000        Min.   :-1.000       
##  1st Qu.:-0.991        1st Qu.:-0.985        1st Qu.:-0.987       
##  Median :-0.952        Median :-0.926        Median :-0.948       
##  Mean   :-0.657        Mean   :-0.629        Mean   :-0.744       
##  3rd Qu.:-0.327        3rd Qu.:-0.264        3rd Qu.:-0.513       
##  Max.   : 1.000        Max.   : 1.000        Max.   : 1.000       
##  fBodyAccJerk-meanFreq()-X fBodyAccJerk-meanFreq()-Y
##  Min.   :-1.0000           Min.   :-1.0000          
##  1st Qu.:-0.2977           1st Qu.:-0.4280          
##  Median :-0.0454           Median :-0.2365          
##  Mean   :-0.0477           Mean   :-0.2134          
##  3rd Qu.: 0.2045           3rd Qu.: 0.0087          
##  Max.   : 1.0000           Max.   : 1.0000          
##  fBodyAccJerk-meanFreq()-Z fBodyGyro-mean()-X fBodyGyro-mean()-Y
##  Min.   :-1.0000           Min.   :-1.000     Min.   :-1.000    
##  1st Qu.:-0.3314           1st Qu.:-0.985     1st Qu.:-0.985    
##  Median :-0.1025           Median :-0.892     Median :-0.920    
##  Mean   :-0.1238           Mean   :-0.672     Mean   :-0.706    
##  3rd Qu.: 0.0912           3rd Qu.:-0.384     3rd Qu.:-0.473    
##  Max.   : 1.0000           Max.   : 1.000     Max.   : 1.000    
##  fBodyGyro-mean()-Z fBodyGyro-meanFreq()-X fBodyGyro-meanFreq()-Y
##  Min.   :-1.000     Min.   :-1.0000        Min.   :-1.0000       
##  1st Qu.:-0.985     1st Qu.:-0.2719        1st Qu.:-0.3626       
##  Median :-0.888     Median :-0.0987        Median :-0.1730       
##  Mean   :-0.644     Mean   :-0.1010        Mean   :-0.1743       
##  3rd Qu.:-0.323     3rd Qu.: 0.0681        3rd Qu.: 0.0137       
##  Max.   : 1.000     Max.   : 1.0000        Max.   : 1.0000       
##  fBodyGyro-meanFreq()-Z fBodyAccMag-mean() fBodyAccMag-meanFreq()
##  Min.   :-1.0000        Min.   :-1.000     Min.   :-1.0000       
##  1st Qu.:-0.2324        1st Qu.:-0.985     1st Qu.:-0.0966       
##  Median :-0.0537        Median :-0.875     Median : 0.0703       
##  Mean   :-0.0514        Mean   :-0.586     Mean   : 0.0769       
##  3rd Qu.: 0.1225        3rd Qu.:-0.217     3rd Qu.: 0.2450       
##  Max.   : 1.0000        Max.   : 1.000     Max.   : 1.0000       
##  fBodyBodyAccJerkMag-mean() fBodyBodyAccJerkMag-meanFreq()
##  Min.   :-1.000             Min.   :-1.000                
##  1st Qu.:-0.990             1st Qu.:-0.003                
##  Median :-0.929             Median : 0.164                
##  Mean   :-0.621             Mean   : 0.173                
##  3rd Qu.:-0.260             3rd Qu.: 0.357                
##  Max.   : 1.000             Max.   : 1.000                
##  fBodyBodyGyroMag-mean() fBodyBodyGyroMag-meanFreq()
##  Min.   :-1.000          Min.   :-1.0000            
##  1st Qu.:-0.983          1st Qu.:-0.2344            
##  Median :-0.876          Median :-0.0521            
##  Mean   :-0.697          Mean   :-0.0416            
##  3rd Qu.:-0.451          3rd Qu.: 0.1516            
##  Max.   : 1.000          Max.   : 1.0000            
##  fBodyBodyGyroJerkMag-mean() fBodyBodyGyroJerkMag-meanFreq()
##  Min.   :-1.000              Min.   :-1.0000                
##  1st Qu.:-0.992              1st Qu.:-0.0195                
##  Median :-0.945              Median : 0.1362                
##  Mean   :-0.780              Mean   : 0.1267                
##  3rd Qu.:-0.612              3rd Qu.: 0.2890                
##  Max.   : 1.000              Max.   : 1.0000                
##  tBodyAcc-std()-X tBodyAcc-std()-Y  tBodyAcc-std()-Z tGravityAcc-std()-X
##  Min.   :-1.000   Min.   :-1.0000   Min.   :-1.000   Min.   :-1.000     
##  1st Qu.:-0.992   1st Qu.:-0.9770   1st Qu.:-0.979   1st Qu.:-0.995     
##  Median :-0.943   Median :-0.8350   Median :-0.851   Median :-0.982     
##  Mean   :-0.608   Mean   :-0.5102   Mean   :-0.613   Mean   :-0.965     
##  3rd Qu.:-0.250   3rd Qu.:-0.0573   3rd Qu.:-0.279   3rd Qu.:-0.962     
##  Max.   : 1.000   Max.   : 1.0000   Max.   : 1.000   Max.   : 1.000     
##  tGravityAcc-std()-Y tGravityAcc-std()-Z tBodyAccJerk-std()-X
##  Min.   :-1.000      Min.   :-1.000      Min.   :-1.000      
##  1st Qu.:-0.991      1st Qu.:-0.987      1st Qu.:-0.991      
##  Median :-0.976      Median :-0.967      Median :-0.951      
##  Mean   :-0.954      Mean   :-0.939      Mean   :-0.640      
##  3rd Qu.:-0.946      3rd Qu.:-0.930      3rd Qu.:-0.291      
##  Max.   : 1.000      Max.   : 1.000      Max.   : 1.000      
##  tBodyAccJerk-std()-Y tBodyAccJerk-std()-Z tBodyGyro-std()-X
##  Min.   :-1.000       Min.   :-1.000       Min.   :-1.000   
##  1st Qu.:-0.985       1st Qu.:-0.989       1st Qu.:-0.987   
##  Median :-0.925       Median :-0.954       Median :-0.902   
##  Mean   :-0.608       Mean   :-0.763       Mean   :-0.721   
##  3rd Qu.:-0.222       3rd Qu.:-0.548       3rd Qu.:-0.482   
##  Max.   : 1.000       Max.   : 1.000       Max.   : 1.000   
##  tBodyGyro-std()-Y tBodyGyro-std()-Z tBodyGyroJerk-std()-X
##  Min.   :-1.000    Min.   :-1.000    Min.   :-1.000       
##  1st Qu.:-0.982    1st Qu.:-0.985    1st Qu.:-0.991       
##  Median :-0.911    Median :-0.882    Median :-0.935       
##  Mean   :-0.683    Mean   :-0.654    Mean   :-0.731       
##  3rd Qu.:-0.446    3rd Qu.:-0.338    3rd Qu.:-0.486       
##  Max.   : 1.000    Max.   : 1.000    Max.   : 1.000       
##  tBodyGyroJerk-std()-Y tBodyGyroJerk-std()-Z tBodyAccMag-std()
##  Min.   :-1.000        Min.   :-1.000        Min.   :-1.000   
##  1st Qu.:-0.992        1st Qu.:-0.993        1st Qu.:-0.982   
##  Median :-0.955        Median :-0.950        Median :-0.844   
##  Mean   :-0.786        Mean   :-0.740        Mean   :-0.591   
##  3rd Qu.:-0.627        3rd Qu.:-0.510        3rd Qu.:-0.242   
##  Max.   : 1.000        Max.   : 1.000        Max.   : 1.000   
##  tGravityAccMag-std() tBodyAccJerkMag-std() tBodyGyroMag-std()
##  Min.   :-1.000       Min.   :-1.000        Min.   :-1.000    
##  1st Qu.:-0.982       1st Qu.:-0.991        1st Qu.:-0.978    
##  Median :-0.844       Median :-0.929        Median :-0.826    
##  Mean   :-0.591       Mean   :-0.628        Mean   :-0.662    
##  3rd Qu.:-0.242       3rd Qu.:-0.273        3rd Qu.:-0.394    
##  Max.   : 1.000       Max.   : 1.000        Max.   : 1.000    
##  tBodyGyroJerkMag-std() fBodyAcc-std()-X fBodyAcc-std()-Y 
##  Min.   :-1.000         Min.   :-1.000   Min.   :-1.0000  
##  1st Qu.:-0.992         1st Qu.:-0.993   1st Qu.:-0.9769  
##  Median :-0.940         Median :-0.942   Median :-0.8326  
##  Mean   :-0.778         Mean   :-0.603   Mean   :-0.5284  
##  3rd Qu.:-0.609         3rd Qu.:-0.249   3rd Qu.:-0.0922  
##  Max.   : 1.000         Max.   : 1.000   Max.   : 1.0000  
##  fBodyAcc-std()-Z fBodyAccJerk-std()-X fBodyAccJerk-std()-Y
##  Min.   :-1.000   Min.   :-1.000       Min.   :-1.000      
##  1st Qu.:-0.978   1st Qu.:-0.992       1st Qu.:-0.987      
##  Median :-0.840   Median :-0.956       Median :-0.928      
##  Mean   :-0.618   Mean   :-0.655       Mean   :-0.612      
##  3rd Qu.:-0.302   3rd Qu.:-0.320       3rd Qu.:-0.236      
##  Max.   : 1.000   Max.   : 1.000       Max.   : 1.000      
##  fBodyAccJerk-std()-Z fBodyGyro-std()-X fBodyGyro-std()-Y
##  Min.   :-1.000       Min.   :-1.000    Min.   :-1.000   
##  1st Qu.:-0.990       1st Qu.:-0.988    1st Qu.:-0.981   
##  Median :-0.959       Median :-0.905    Median :-0.906   
##  Mean   :-0.781       Mean   :-0.739    Mean   :-0.674   
##  3rd Qu.:-0.590       3rd Qu.:-0.522    3rd Qu.:-0.438   
##  Max.   : 1.000       Max.   : 1.000    Max.   : 1.000   
##  fBodyGyro-std()-Z fBodyAccMag-std() fBodyBodyAccJerkMag-std()
##  Min.   :-1.000    Min.   :-1.000    Min.   :-1.000           
##  1st Qu.:-0.986    1st Qu.:-0.983    1st Qu.:-0.991           
##  Median :-0.891    Median :-0.855    Median :-0.925           
##  Mean   :-0.690    Mean   :-0.659    Mean   :-0.640           
##  3rd Qu.:-0.417    3rd Qu.:-0.382    3rd Qu.:-0.308           
##  Max.   : 1.000    Max.   : 1.000    Max.   : 1.000           
##  fBodyBodyGyroMag-std() fBodyBodyGyroJerkMag-std()
##  Min.   :-1.000         Min.   :-1.000            
##  1st Qu.:-0.978         1st Qu.:-0.993            
##  Median :-0.828         Median :-0.938            
##  Mean   :-0.700         Mean   :-0.792            
##  3rd Qu.:-0.471         3rd Qu.:-0.644            
##  Max.   : 1.000         Max.   : 1.000
```

```r
str(TidyDataset)
```

```
## 'data.frame':	10299 obs. of  80 variables:
##  $ Activity                       : Factor w/ 6 levels "LAYING","SITTING",..: 4 4 4 4 4 4 4 4 4 4 ...
##  $ tBodyAcc-mean()-X              : num  0.302 0.343 0.27 0.268 0.314 ...
##  $ tBodyAcc-mean()-Y              : num  -0.02688 -0.00343 0.01091 -0.01273 -0.0087 ...
##  $ tBodyAcc-mean()-Z              : num  -0.0958 -0.1015 -0.0749 -0.0937 -0.1246 ...
##  $ tGravityAcc-mean()-X           : num  0.902 0.965 0.96 0.944 0.892 ...
##  $ tGravityAcc-mean()-Y           : num  -0.137 -0.145 -0.214 -0.159 -0.147 ...
##  $ tGravityAcc-mean()-Z           : num  -0.3428 0.0285 0.0505 -0.1425 -0.3414 ...
##  $ tBodyAccJerk-mean()-X          : num  0.1505 -0.1351 0.1262 -0.0358 -0.2256 ...
##  $ tBodyAccJerk-mean()-Y          : num  -0.2391 -0.0637 0.2083 -0.0136 -0.0699 ...
##  $ tBodyAccJerk-mean()-Z          : num  -0.1533 -0.2631 0.0773 0.2511 -0.0937 ...
##  $ tBodyGyro-mean()-X             : num  0.2139 -0.0277 -0.0484 -0.0117 0.0334 ...
##  $ tBodyGyro-mean()-Y             : num  -0.049 -0.1253 -0.1036 -0.0135 -0.0969 ...
##  $ tBodyGyro-mean()-Z             : num  -0.24527 0.12289 0.08692 0.15683 -0.00968 ...
##  $ tBodyGyroJerk-mean()-X         : num  -0.0634 0.0756 0.047 -0.1203 -0.203 ...
##  $ tBodyGyroJerk-mean()-Y         : num  0.03158 -0.05686 -0.09651 -0.00423 -0.16378 ...
##  $ tBodyGyroJerk-mean()-Z         : num  -0.0112 0.14377 0.00215 -0.18785 0.05743 ...
##  $ tBodyAccMag-mean()             : num  -0.0919 -0.0604 -0.1786 -0.1654 -0.0508 ...
##  $ tGravityAccMag-mean()          : num  -0.0919 -0.0604 -0.1786 -0.1654 -0.0508 ...
##  $ tBodyAccJerkMag-mean()         : num  -0.221 -0.177 -0.154 -0.165 -0.167 ...
##  $ tBodyGyroMag-mean()            : num  -0.2319 -0.2911 -0.2321 -0.0934 -0.3111 ...
##  $ tBodyGyroJerkMag-mean()        : num  -0.3526 -0.3462 -0.1846 -0.0862 -0.3425 ...
##  $ fBodyAcc-mean()-X              : num  -0.426 -0.18 -0.229 -0.337 -0.377 ...
##  $ fBodyAcc-mean()-Y              : num  -0.0972 0.0852 0.1118 -0.1718 -0.1915 ...
##  $ fBodyAcc-mean()-Z              : num  0.1846 -0.3 -0.4013 0.0634 0.1929 ...
##  $ fBodyAcc-meanFreq()-X          : num  -0.474 -0.348 -0.247 -0.324 -0.249 ...
##  $ fBodyAcc-meanFreq()-Y          : num  0.2648 -0.1593 -0.0343 -0.0378 0.1274 ...
##  $ fBodyAcc-meanFreq()-Z          : num  0.10475 0.2059 0.33348 0.11459 -0.00943 ...
##  $ fBodyAccJerk-mean()-X          : num  -0.49 -0.2 -0.174 -0.353 -0.466 ...
##  $ fBodyAccJerk-mean()-Y          : num  -0.189 -0.1399 -0.0781 -0.2802 -0.2017 ...
##  $ fBodyAccJerk-mean()-Z          : num  0.0147 -0.39925 -0.41452 -0.06667 0.00383 ...
##  $ fBodyAccJerk-meanFreq()-X      : num  -0.325 -0.235 -0.283 -0.28 -0.426 ...
##  $ fBodyAccJerk-meanFreq()-Y      : num  -0.263 -0.681 -0.593 -0.467 -0.171 ...
##  $ fBodyAccJerk-meanFreq()-Z      : num  -0.25797 -0.25201 -0.00807 -0.45074 -0.32644 ...
##  $ fBodyGyro-mean()-X             : num  -0.321 -0.311 -0.164 -0.368 -0.347 ...
##  $ fBodyGyro-mean()-Y             : num  -0.201 -0.414 -0.185 0.127 -0.248 ...
##  $ fBodyGyro-mean()-Z             : num  -0.4485 -0.157 -0.0698 -0.2908 -0.4522 ...
##  $ fBodyGyro-meanFreq()-X         : num  0.044 0.0085 0.0896 -0.2157 0.134 ...
##  $ fBodyGyro-meanFreq()-Y         : num  0.105 0.0201 0.0239 0.0392 0.0229 ...
##  $ fBodyGyro-meanFreq()-Z         : num  -0.2098 0.2922 0.2877 -0.0887 -0.1607 ...
##  $ fBodyAccMag-mean()             : num  -0.224 -0.196 -0.213 -0.135 -0.168 ...
##  $ fBodyAccMag-meanFreq()         : num  0.199 0.21 0.246 0.289 0.222 ...
##  $ fBodyBodyAccJerkMag-mean()     : num  -0.1081 -0.1625 -0.0958 -0.0753 -0.081 ...
##  $ fBodyBodyAccJerkMag-meanFreq() : num  0.03543 -0.00273 0.02406 0.05082 0.156 ...
##  $ fBodyBodyGyroMag-mean()        : num  -0.356 -0.414 -0.149 0.202 -0.255 ...
##  $ fBodyBodyGyroMag-meanFreq()    : num  0.1883 0.3676 0.2685 0.4571 0.0931 ...
##  $ fBodyBodyGyroJerkMag-mean()    : num  -0.2935 -0.5085 -0.2354 0.0615 -0.2945 ...
##  $ fBodyBodyGyroJerkMag-meanFreq(): num  -0.112 0.2 0.215 0.293 -0.151 ...
##  $ tBodyAcc-std()-X               : num  -0.38 -0.201 -0.337 -0.384 -0.356 ...
##  $ tBodyAcc-std()-Y               : num  -0.191 0.133 0.146 -0.204 -0.166 ...
##  $ tBodyAcc-std()-Z               : num  0.341 -0.318 -0.446 0.148 0.407 ...
##  $ tGravityAcc-std()-X            : num  -0.953 -0.984 -0.965 -0.981 -0.98 ...
##  $ tGravityAcc-std()-Y            : num  -0.937 -0.953 -0.939 -0.973 -0.961 ...
##  $ tGravityAcc-std()-Z            : num  -0.921 -0.99 -0.946 -0.966 -0.941 ...
##  $ tBodyAccJerk-std()-X           : num  -0.472 -0.197 -0.159 -0.333 -0.453 ...
##  $ tBodyAccJerk-std()-Y           : num  -0.1607 -0.0446 0.0551 -0.2088 -0.1215 ...
##  $ tBodyAccJerk-std()-Z           : num  -0.06331 -0.44016 -0.47746 -0.05011 -0.00443 ...
##  $ tBodyGyro-std()-X              : num  -0.512 -0.448 -0.407 -0.501 -0.536 ...
##  $ tBodyGyro-std()-Y              : num  -0.254 -0.394 -0.208 0.178 -0.239 ...
##  $ tBodyGyro-std()-Z              : num  -0.404 -0.311 -0.288 -0.305 -0.362 ...
##  $ tBodyGyroJerk-std()-X          : num  -0.228 -0.3 -0.148 -0.411 -0.235 ...
##  $ tBodyGyroJerk-std()-Y          : num  -0.293 -0.454 -0.211 0.173 -0.279 ...
##  $ tBodyGyroJerk-std()-Z          : num  -0.56075 -0.09672 -0.00331 -0.44637 -0.55917 ...
##  $ tBodyAccMag-std()              : num  -0.219 -0.318 -0.331 -0.25 -0.193 ...
##  $ tGravityAccMag-std()           : num  -0.219 -0.318 -0.331 -0.25 -0.193 ...
##  $ tBodyAccJerkMag-std()          : num  -0.0879 -0.1895 -0.1146 -0.0714 -0.0674 ...
##  $ tBodyGyroMag-std()             : num  -0.359 -0.457 -0.169 0.095 -0.26 ...
##  $ tBodyGyroJerkMag-std()         : num  -0.26 -0.432 -0.175 0.135 -0.255 ...
##  $ fBodyAcc-std()-X               : num  -0.363 -0.21 -0.384 -0.403 -0.347 ...
##  $ fBodyAcc-std()-Y               : num  -0.2983 0.0866 0.0919 -0.272 -0.2047 ...
##  $ fBodyAcc-std()-Z               : num  0.319 -0.385 -0.52 0.104 0.41 ...
##  $ fBodyAccJerk-std()-X           : num  -0.5 -0.269 -0.219 -0.372 -0.489 ...
##  $ fBodyAccJerk-std()-Y           : num  -0.18651 -0.00372 0.12855 -0.18274 -0.09159 ...
##  $ fBodyAccJerk-std()-Z           : num  -0.1381 -0.4783 -0.5398 -0.0346 -0.011 ...
##  $ fBodyGyro-std()-X              : num  -0.574 -0.491 -0.487 -0.544 -0.598 ...
##  $ fBodyGyro-std()-Y              : num  -0.294 -0.387 -0.229 0.201 -0.239 ...
##  $ fBodyGyro-std()-Z              : num  -0.445 -0.433 -0.441 -0.373 -0.393 ...
##  $ fBodyAccMag-std()              : num  -0.338 -0.505 -0.512 -0.44 -0.333 ...
##  $ fBodyBodyAccJerkMag-std()      : num  -0.0685 -0.2309 -0.139 -0.0728 -0.0562 ...
##  $ fBodyBodyGyroMag-std()         : num  -0.474 -0.589 -0.331 -0.192 -0.394 ...
##  $ fBodyBodyGyroJerkMag-std()     : num  -0.269 -0.382 -0.158 0.149 -0.256 ...
```

```r
#If check is ok output data to
write.table(TidyDataset, "Dataset1.txt")
```

##Second Dataset
The second dataset was the dataset that needed to be created by merging and reshaping the data from the following files
1. X_train.txt
2. y_train.txt
3. subject_train.txt
4. X_test.txt
5. y_test.txt
6. subject_test.txt

The code below will create the desired database in Text format. When you run the code below and have the data in your user folder a database the Dataset2.txt will be created.

Next to subsetting and merging the files together it will also reshape the data into four columns please see CodeBook.md for the precise format of the data.

You will also find summary statisics of the data below the code so you can check if the database that is reproduced is exactly the one you should expect from this document.


```r
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
```

```
##   SubjectID Activity             variable     mean
## 1         1        1    tBodyAcc-mean()-X  0.27733
## 2         1        1    tBodyAcc-mean()-Y -0.01738
## 3         1        1    tBodyAcc-mean()-Z -0.11115
## 4         1        1 tGravityAcc-mean()-X  0.93522
## 5         1        1 tGravityAcc-mean()-Y -0.28217
## 6         1        1 tGravityAcc-mean()-Z -0.06810
```

```r
summary(TidyDataSet)
```

```
##    SubjectID       Activity                   variable    
##  Min.   : 1.0   Min.   :1.0   tBodyAcc-mean()-X   :  180  
##  1st Qu.: 8.0   1st Qu.:2.0   tBodyAcc-mean()-Y   :  180  
##  Median :15.5   Median :3.5   tBodyAcc-mean()-Z   :  180  
##  Mean   :15.5   Mean   :3.5   tGravityAcc-mean()-X:  180  
##  3rd Qu.:23.0   3rd Qu.:5.0   tGravityAcc-mean()-Y:  180  
##  Max.   :30.0   Max.   :6.0   tGravityAcc-mean()-Z:  180  
##                               (Other)             :13140  
##       mean        
##  Min.   :-0.9977  
##  1st Qu.:-0.9524  
##  Median :-0.3423  
##  Mean   :-0.4124  
##  3rd Qu.:-0.0365  
##  Max.   : 0.9745  
## 
```

```r
str(TidyDataSet)
```

```
## 'data.frame':	14220 obs. of  4 variables:
##  $ SubjectID: int  1 1 1 1 1 1 1 1 1 1 ...
##  $ Activity : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ variable : Factor w/ 79 levels "tBodyAcc-mean()-X",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ mean     : num  0.2773 -0.0174 -0.1111 0.9352 -0.2822 ...
```

```r
#If check is ok output data to
write.table(TidyDataSet, "Dataset2.txt")
```



