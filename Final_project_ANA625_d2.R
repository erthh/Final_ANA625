#set up library
library(plyr)
library(ggplot2)
library(ROCR)
library(caret)

#import data 
vote_data <- read.csv("vote_drough.csv",header=T)

###############################
##### Data Pre processing #####
###############################

#Data structure check (vote_data year 2012)
names(vote_data)
str(vote_data)
# 0 is blue , 1 is red
table(vote_data$target)
#baseline when the model predict all RED = 0.79 
2384 / (643+2384)

#create data frame choose only necessary variables 
working_data <- data.frame(county = vote_data$county , state = vote_data$state , None = vote_data$None , D0 = vote_data$D0 , D1 = vote_data$D1, D2 = vote_data$D2 , D3 = vote_data$D3 , D4 = vote_data$D4 , target = as.factor(vote_data$target))
#splitting the data 
#create(split) INDEX for training data set only. [It's not split the data]
set.seed(12)

split_index <- createDataPartition(working_data$target,p=0.8,list=F)
train_data <- working_data[split_index,] 
test_data <- working_data[-split_index,]
nrow(working_data)
nrow(train_data)
nrow(test_data)

#create model

#create trainControl for reuse 
trainControl <- trainControl(method = "cv", number = 5) 

###################################
####### Logistic Regression #######
###################################

logistic_model <- train(target ~ D0 + D1 + D2 + D3 + D4 + None , train_data , 
                        trControl = trainControl , 
                        method = "glm", 
                        preProcess = c("zv","center","scale","pca"))
#predict 
#train data set 
logistic_pred_train <- predict(logistic_model,type="raw")
summary(logistic_pred_train)
#test data set 
logistic_pred_test <- predict(logistic_model,newdata=test_data,type="raw")
summary(logistic_pred_test)

#Confusion Matrix with train data
confusionMatrix(as.factor(logistic_pred_train),train_data$target,mode='everything')

#Confusion Matrix with test data
confusionMatrix(as.factor(logistic_pred_test),test_data$target,mode='everything')

###################################
######### Random Forrest ##########
###################################
randomF_model <- train(target ~ D0 + D1 + D2 + D3 + D4 + None , train_data , 
                       trControl = trainControl , 
                       method = "ranger", 
                       tuneLength = 4,
                       preProcess = c("zv","center","scale","pca"))
plot(randomF_model)

#predict model with data
#train data set 
randomF_pred_train <- predict(randomF_model,type="raw")
summary(randomF_pred_train)
#test data set 
randomF_pred_test <- predict(randomF_model,newdata=test_data,type="raw")
summary(randomF_pred_test)

#Confusion Matrix with train data
confusionMatrix(as.factor(randomF_pred_train),train_data$target,mode='everything')

#Confusion Matrix with test data
confusionMatrix(as.factor(randomF_pred_test),test_data$target,mode='everything')

###################################
############## KNN ################
###################################
knn_model <- train(target ~ D0 + D1 + D2 + D3 + D4 + None , train_data , 
                       trControl = trainControl , 
                       method = "knn", 
                       preProcess = c("zv","center","scale","pca"))
plot(knn_model)
print(knn_model)

#prediction
#train data set 
knn_pred_train <- predict(knn_model,type="raw")
summary(knn_pred_train)
#test data set 
knn_pred_test <- predict(knn_model,newdata=test_data,type="raw")

#Confusion Matrix with train data 
confusionMatrix(as.factor(knn_pred_train),train_data$target,mode='everything')

#Confusion Matrix with test data
confusionMatrix(as.factor(knn_pred_test),test_data$target,mode='everything')

############################################################

#Plot ROC and AUC 



