#README
This repo contains data derived from "Human Activity Recognition Using Smartphones Data Set" the data was processed 
with sript run_analysis.R, the script was runed in R version 3.3.3 in windows 10 workstation.

The script define 4 functions used to transform de original data sets.

**downloadandunzip.**  this function download de data from de url, and uzip all files in directory named data, in current 
directory.

**getcolumnnames** this function get column names from features.txt file, the names was processed to eliminate parenthesis, comma
and hyphen.

**addsubjectactivity** this function receive a directory like param, and read the data in the directory and compose a data frame
adding subject y activity columns.

**recodeactivity** this function recode the activity column whith clear names described in activity_labels original file.

when the files are processed i produce 3 files in csv format.

alldata.csv, this file contains merge info in test and training directory X_ file and the columns subject and activity was added using
subject and y file in the directory

meanandstd.csv, contains only columns corresponding to mean and std for all variables in the original data.

summdata.csv contains summarized info for activity and subject

```
#
# Download and unzip files for analisys, the file will be download in current
# directory and unziped in a directory named data, created in current directory
#
#
downloadandunzip <- function(url){
	filename <- "FUCIHARdataset.zip"
	download.file(url,dest=filename)
	unzip(filename, exdir="./data")
}


#
# Compose column names from features file in data directory. the characters -(), were removed from names.
#
getcolumnnames <- function(){
	featurefilename <- file.path(".","data","UCI HAR Dataset","features.txt",fsep=.Platform$file.sep)
	featuretable <- read.table(featurefilename)
	columnnames <- as.vector(featuretable[['V2']])
	columnnames <- gsub("-","",columnnames)
	columnnames <- gsub("\\(","",columnnames)
	columnnames <- gsub("\\)","",columnnames)
	columnnames <- gsub("\\,","",columnnames)
	columnnames <- tolower(columnnames)
	print(columnnames)
}


#
# This function read files in a directory structure defined for UCI HAR Dataset
# and merge all files in a single data frame.
#
addsubjectactivity <- function(directory){

	#compose filename and read x file
	xfilename <- paste("X_", directory, ".txt",sep="")
	xfullpath <- file.path(".","data","UCI HAR Dataset",directory,xfilename,fsep=.Platform$file.sep)
	xtable <- read.table(xfullpath)
	
	colnames(xtable) <- getcolumnnames()
	
	#compose filename and read y file
	yfilename <- paste("y_", directory, ".txt",sep="")
	yfullpath <- file.path(".","data","UCI HAR Dataset",directory,yfilename,fsep=.Platform$file.sep)
	ytable <- read.table(yfullpath)
	
	#compose file name and read subject file.
	subjectfilename <- paste("subject_", directory, ".txt",sep="")
	subjectfullpath <- file.path(".","data","UCI HAR Dataset",directory,subjectfilename,fsep=.Platform$file.sep)
	subjecttable <- read.table(subjectfullpath)
		
	#bind columns subject and activity to x data frame.
	xtable$subject <- subjecttable[[1]]
	xtable$activity <- ytable[[1]]
		
	xtable
}



recodeactivity <-function(df){
	df$activity[df$activity==1] <- "WALKING"
	df$activity[df$activity==2] <- "WALKING_UPSTAIRS"
	df$activity[df$activity==3] <- "WALKING_DOWNSTAIRS"
	df$activity[df$activity==4] <- "SITTING"
	df$activity[df$activity==5] <- "STANDING"
	df$activity[df$activity==6] <- "LAYING"
	df
						
}



#download all information
dataurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
downloadandunzip(dataurl)

#Merge data
testdata <- addsubjectactivity("test")
traindata <- addsubjectactivity("train")
alldata <- rbind(testdata,traindata)


#recode activity names uses descriptive activity names to name the activities in the data set
alldata <- recodeactivity(alldata)
write.csv(alldata,file="./data/alldata.csv")

alldata<-alldata[,!duplicated(colnames(alldata))]

#get only mean and std for all variables.
alldatameanstd<-select(alldata,matches("mean|std"))
write.csv(alldatameanstd,file="./data/meanandstd.csv")

grdata<-group_by(alldata,subject,activity)
summdata<-summarise_all(grdata,mean)
write.csv(summdata,file="./data/summdata.csv")


```
