#set up library
library(plyr)
library(ggplot2)
#import data 
Original_data <- read.csv("drought_target.csv",header=T)

###############################
##### Data Pre processing #####
###############################

#Data structure check
names(Original_data)
str(Original_data)
colnames(Original_data)[1] <- "ID"  #Change column of X to ID 
Original_data$ValidEnd <- as.Date(Original_data$ValidEnd,format="%m/%d/%Y")  #Change factor to date variable type
Original_data$ValidStart <- as.Date(Original_data$ValidStart,format="%m/%d/%Y")  #Change factor to date variable type
str(Original_data)  

#Checking for missing value 
sum(is.na(Original_data))  #Total missing value
colSum(is.na(Original_data)) #Total missing value in each column
data_nona <- na.omit(Original_data)  #Remove PR start because it's not in US 
sum(is.na(data_nona)) #check for the missing value again 

#Checking for Correlation

#Checking for transformation
ggplot(Original_data,aes(x = target , fill=target)) + geom_bar() + scale_fill_manual("target", values = c("Blue" = "blue", "Red" = "red")) + ggtitle("Target amount") 
ggplot(Original_data,aes(x = D0)) + geom_histogram(bins=50) + ggtitle("D0 Distribution")
ggplot(Original_data,aes(x = log(D0))) + geom_histogram(bins=50) + ggtitle("D0 Distribution")
ggplot(Original_data,aes(x = D1)) + geom_histogram(bins=50) + ggtitle("D0 Distribution")
ggplot(Original_data,aes(x = log(D1))) + geom_histogram(bins=50) + ggtitle("D1 Distribution")
ggplot(Original_data,aes(x = D2)) + geom_histogram(bins=50) + ggtitle("D2 Distribution")
ggplot(Original_data,aes(x = log(D2))) + geom_histogram(bins=50) + ggtitle("D2 Distribution")
ggplot(Original_data,aes(x = D3)) + geom_histogram(bins=50) + ggtitle("D3 Distribution")
ggplot(Original_data,aes(x = log(D3))) + geom_histogram(bins=50) + ggtitle("D3 Distribution")
ggplot(Original_data,aes(x = D4)) + geom_histogram(bins=50) + ggtitle("D4 Distribution")
ggplot(Original_data,aes(x = log(D4))) + geom_histogram(bins=50) + ggtitle("D4 Distribution")
ggplot(Original_data,aes(x = None)) + geom_histogram(bins=50) + ggtitle("None Distribution")
ggplot(Original_data,aes(x = log10(None))) + geom_histogram(bins=50) + ggtitle("None Distribution")
str(Original_data)

##########################################
###########  Create a model ##############
##########################################


##########################################
####### evaulate model result ############
##########################################






