#set up library
library(plyr)
library(ggplot2)
library(ROCR)
library(caret)
library(base)
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
table.target <- table(vote_data$target)

str(vote_data$target)
#baseline when the model predict all RED = 0.79 
2384 / (643+2384)

#create data frame choose only necessary variables 
working_data <- data.frame(county = vote_data$county , state = vote_data$state , None = vote_data$None , D0 = vote_data$D0 , D1 = vote_data$D1, D2 = vote_data$D2 , D3 = vote_data$D3 , D4 = vote_data$D4 , target = as.factor(vote_data$target))
levels(working_data$target) <- c("blue","red")
#splitting the data 
#create(split) INDEX for training data set only. [It's not split the data]
set.seed(1234)

split_index <- createDataPartition(working_data$target,p=0.8,list=F)
train_data <- working_data[split_index,] 
test_data <- working_data[-split_index,]
table(train_data$target)
table(test_data$target)
#plot
# multi.hist
# hist(log(working_data$D0))
# hist((working_data$D1))
# hist(log(working_data$D1+1))
# hist((working_data$D2))
# hist(log(working_data$D2+1))
# hist((working_data$D3))
# hist(log(working_data$D3+1))
# hist((working_data$D4))
# hist(sqrt(working_data$D4))
# hist(working_data$None)

#############################################
############  create model  ################# 
#############################################
#create trainControl for reuse 
trainControl <- trainControl(method = "repeatedcv", number = 5 , repeats = 5) 

###################################
####### Logistic Regression #######
###################################

logistic_model <- train(target ~ D0 + D1 + D2 + D3 + D4 + None  , train_data, 
                        trControl = trainControl , 
                        method = "glm", 
                        preProcess = c("center","scale"))

plot(varImp(logistic_model))

#predict 
#train data set 
logistic_pred_train <- predict(logistic_model,type="raw")
summary(logistic_pred_train)

#Confusion Matrix with train data
confusionMatrix(as.factor(logistic_pred_train),train_data$target,mode='everything')

#test data set 
logistic_pred_test <- predict(logistic_model,newdata=test_data,type="raw")
summary(logistic_pred_test)

#Confusion Matrix with test data
logis_cm <- confusionMatrix(as.factor(logistic_pred_test),test_data$target,mode='everything')

#evaluate model
round(coef(logistic_model$finalModel),3)

#AUC 

###################################
######### Decision tree ###########
###################################

Decision_model <- train(target ~D0 + D1 +None +state , train_data , 
                       trControl = rf_trainControl , 
                       method = "rpart2", 
                       tuneLength = 10,
                       preProcess = c("center","scale"),
                       metric="ROC")
plot(Decision_model,main="Decision Tree's Max Tree Depth")

###################################
######### Random Forrest ##########
###################################
rf_trainControl <- trainControl(method="cv",number=5,classProbs=T,summaryFunction=twoClassSummary)
randomF_model <- train(target ~D0 + D1 +None +state , train_data , 
                       trControl = rf_trainControl , 
                       method = "ranger", 
                       tuneLength = 4,
                       preProcess = c("center","scale"))
randomF_model
plot(randomF_model)
print(randomF_model)
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
randomF_cm <- confusionMatrix(as.factor(randomF_pred_test),test_data$target,mode='everything')

###################################
############## KNN ################
###################################

knn_model <- train(target ~ D0 + D1 + D3 + D2 + D4 + None , train_data , 
                       trControl = trainControl, 
                       method = "knn", 
                       tuneLength = 10,
                       preProcess = c("center","scale"))
plot(knn_model)
print(knn_model)

#prediction
#train data set 
knn_pred_train <- predict(knn_model,type="raw")
summary(knn_pred_train)

#Confusion Matrix with train data 
confusionMatrix(as.factor(knn_pred_train),train_data$target,mode='everything')

#test data set 
knn_pred_test <- predict(knn_model,newdata=test_data,type="raw")

#Confusion Matrix with test data
knn_cm <- confusionMatrix(as.factor(knn_pred_test),test_data$target,mode='everything')

############################################################

#plot ROC 
plot(knn_perf, col="blue",main="ROC Curve")
plot(rf_perf, col="salmon",add=T)
plot(logistic_perf, col="green",add=T)
mtext(paste("AUC(Logistic): ",round(knn_auc_value,4)),line = -2, side=1)
mtext(paste("AUC(Random Forrest): ",round(randomF_auc_value,4)),line = -3,side=1)
mtext(paste("AUC(KNN): ",round(knn_auc_value,4)),line = -4,side=1)

#make prediction to find ROC and AUC Curve
logis_pred_roc <- predict(logistic_model,newdata=test_data,type="prob")
randomF_pred_roc <- predict(randomF_model,newdata=test_data,type="prob")
knn_pred_roc <- predict(knn_model,newdata=test_data,type="prob")

#Find AUC Value
logistic_auc <- auc(roc(test_data$target,logis_pred_roc$blue))
randomF_auc <- auc(roc(test_data$target,randomF_pred_roc$blue))
knn_auc <- auc(roc(test_data$target,knn_pred_roc$blue))

#Plot ROC and AUC
roc(test_data$target,logis_pred_roc$blue,plot = T,print.auc=T,col="salmon",main="ROC Curve of 3 models")
roc(test_data$target,randomF_pred_roc$blue,plot = T,print.auc=T,print.auc.y=.4,col="green",add=T)
roc(test_data$target,knn_pred_roc$blue,plot = T,print.auc=T,col="blue",print.auc.y=.3,add=T)
#add legend
legend("bottomright",legend=c("Logistic","Random Forrest","KNN"),col=c("salmon","green","blue"),lwd=4,cex=0.9,
       text.col = c("salmon","green","blue"),bty='n')

# #plot box plot 
# featurePlot(x=working_data[3:8],y=working_data$target,scales=list(y=list(relation="free"),x=list(rot=90)))
# 
# #scatterplot matrix
# featurePlot(x=working_data[3:8],y=working_data$target,plot = "pairs",auto.key = list(columns = 2))
# 

#Summary Result

paste("AUC - Logistic: ", round(logistic_auc[1],3), "| Random Forrest: ",round(randomF_auc[1],3),
      "| KNN: ", round(knn_auc[1],3))

paste("F1 Score - Logistic: ",round(logis_cm$byClass[7],4),"| Random Forrest: ", round(randomF_cm$byClass[7],4),
      "| KNN: ",round(knn_cm$byClass[7],4))

paste("Accuracy - Logistic: ",round(logis_cm$overall[1],4),"| Random Forrest: ", round(randomF_cm$overall[1],4),
      "| KNN: ",round(knn_cm$overall[1],4))

paste("Kappa - Logistic: ",round(logis_cm$byClass[2],4),"| Random Forrest: ", round(randomF_cm$overall[2],4),
      "| KNN: ",round(knn_cm$overall[2],4))