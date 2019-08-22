#set up library
library(plyr)
library(ggplot2)
library(ROCR)
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
str(Original_data$target)  
levels(Original_data$target)<-c("Red"=0,"Blue"=1)
table(Original_data$target)
#baseline when the model predict all BLUE = 0.60 
4867 / (3140+4867)

#Checking for missing value 
sum(is.na(Original_data))  #Total missing value
colSums(is.na(Original_data)) #Total missing value in each column
Original_data <- na.omit(Original_data)  #Remove PR start because it's not in US 
sum(is.na(Original_data)) #check for the missing value again 

#Checking distribution for transformation
ggplot(Original_data,aes(x = target , fill=target)) + geom_bar() + scale_fill_manual("target", values = c("Blue" = "blue", "Red" = "red")) + ggtitle("Target amount") 
ggplot(Original_data,aes(x = D0)) + geom_histogram(bins=50) + ggtitle("D0 Distribution")
ggplot(Original_data,aes(x = log(D0+1))) + geom_histogram(bins=50) + ggtitle("D0 Distribution")
ggplot(Original_data,aes(x = D1)) + geom_histogram(bins=50) + ggtitle("D0 Distribution")
ggplot(Original_data,aes(x = log(D1+1))) + geom_histogram(bins=50) + ggtitle("D1 Distribution")
ggplot(Original_data,aes(x = D2)) + geom_histogram(bins=50) + ggtitle("D2 Distribution")
ggplot(Original_data,aes(x = log(D2+1))) + geom_histogram(bins=50) + ggtitle("D2 Distribution")
ggplot(Original_data,aes(x = D3)) + geom_histogram(bins=50) + ggtitle("D3 Distribution")
ggplot(Original_data,aes(x = log(D3+1))) + geom_histogram(bins=50) + ggtitle("D3 Distribution")
ggplot(Original_data,aes(x = D4)) + geom_histogram(bins=50) + ggtitle("D4 Distribution")
ggplot(Original_data,aes(x = log(D4+1))) + geom_histogram(bins=50) + ggtitle("D4 Distribution")
ggplot(Original_data,aes(x = None)) + geom_histogram(bins=50) + ggtitle("None Distribution")
ggplot(Original_data,aes(x = log10(None+1))) + geom_histogram(bins=50) + ggtitle("None Distribution")

#store transform data
Original_data$D0_tranform <- log(Original_data$D0+1)
Original_data$D1_tranform <- log(Original_data$D1+1)
Original_data$D2_tranform <- log(Original_data$D2+1)
Original_data$D3_tranform <- log(Original_data$D3+1)
Original_data$D4_tranform <- log(Original_data$D4+1)
Original_data$None_transform <- log(Original_data$None+1)
str(Original_data)

#create new dataframe choose only nescessary
working_data <- data.frame(D0 = Original_data$D0_tranform,D1 = Original_data$D1_tranform,D2 = Original_data$D2_tranform,D3 = Original_data$D3_tranform,
                           D4 = Original_data$D4_tranform,None = Original_data$None_transform,Target = Original_data$target)

#Checking for Correlation (Correlation Matrix)
pairs(working_data,main="Scatterplot Matrix males")

#Checking outlier by boxplot
boxplot(working_data[,])

#splitting the data 
#train 80 and test 20 

#create(split) INDEX for training data set only. [It's not split the data]
splitting_index <- createDataPartition(working_data$Target , p=0.60 ,list=F)

train_dataset <- working_data[splitting_index,]
test_dataset <- working_data[-splitting_index,]

##########################################
###########  Create a model ##############
##########################################

#-----------------------------------------------------------#
#----- Logistic Regression with Cross validation method ----#
#-----------------------------------------------------------#

trainControl <- trainControl(method = "cv", number = 10)
Logistic_model_cv <- train(Target ~ D0+D1+D2+D3+D4+None , train_dataset , trControl = trainControl ,method="glm")
summary(Logistic_model_cv)

#-----------------------------------------------------------#
#------------------- Random Forrest ------------------------#
#-----------------------------------------------------------#

##########################################
####### evaulate model result ############
##########################################

#-----------------------------------------------------------#
#---------------- Logistic Regression Model ----------------#
#-----------------------------------------------------------#

#predict value from our model
logistic_predicted <- predict(Logistic_model_cv,newdata = test_dataset,type="raw")
summary(logistic_predicted)

#Create confusion matrix
logistic_cm <- confusionMatrix(test_dataset$Target,logistic_predicted)

# Prediction function, This function used to find other matrics
logistic_prediction <- prediction(as.numeric(logistic_predicted),test_dataset$Target)

#Find ROC 
ROCR_logistic = performance(logistic_prediction, "tpr", "fpr")
#Find AUC
auc.logis <- performance(logistic_prediction,"auc")
auc.logistic.value <- auc.tmp.reduced@y.values[[1]] #area under curve

#Plot ROC and AUC curve
plot(ROCR_logistic,colorize=T, main="ROC Curve")
mtext(paste("AUC: ",round(auc.logistic.value,4)),line = -8)

#lift chart 
lift.logictic <- performance(logistic_prediction,"lift","rpp")
#plot lift chart 
plot(lift.logictic, main="lift curve", colorize=T)


#-----------------------------------------------------------#
#----------------- Random Forrest Model --------------------#
#-----------------------------------------------------------#







