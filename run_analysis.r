#download data
library(httr) 
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "instancia.zip"
if(!file.exists(file)){
	download.file(url, file, method="curl")
}

myfolder <- "UCI HAR Dataset"
myresults <- "results"
if(!file.exists(myfolder)){
	unzip(file, list = FALSE, overwrite = TRUE)
} 
if(!file.exists(myresults)){
	dir.create(myresults)
} 


gettables <- function (filename,cols = NULL){
	print(paste("Getting table:", filename))
	f <- paste(datafolder,filename,sep="/")
	data <- data.frame()
	if(is.null(cols)){
		data <- read.table(f,sep="",stringsAsFactors=F)
	} else {
		data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
	}
	data
}


features <- gettables("features.txt")

#build database
getdata <- function(type, features){
	print(paste("Getting data", type))
	subject_data <- gettables(paste(type,"/","subject_",type,".txt",sep=""),"id")
	y_data <- gettables(paste(type,"/","y_",type,".txt",sep=""),"activity")
	x_data <- gettables(paste(type,"/","X_",type,".txt",sep=""),features$V2)
	return (cbind(subject_data,y_data,x_data))
}


test <- getdata("test", features)
train <- getdata("train", features)


saveresults <- function (data,name){
	print(paste("saving results", name))
	file <- paste(resultsfolder, "/", name,".csv" ,sep="")
	write.csv(data,file)
}


#merge the training and test set
library(plyr)
data <- rbind(train, test)
data <- arrange(data, id)

#extract the mean and standard deviation
meanstd <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]


activity_labels <- gettables("activity_labels.txt")

data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

#create tidy data
tidy_dataset <- ddply(meanstd, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(tidy_dataset)[-c(1:2)] <- paste(colnames(tidy_dataset)[-c(1:2)], "_mean", sep="")
saveresults(tidy_dataset,"tidy_dataset")
