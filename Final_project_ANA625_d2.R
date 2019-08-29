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
multi.hist
hist(log(working_data$D0))
hist((working_data$D1))
hist(log(working_data$D1+1))
hist((working_data$D2))
hist(log(working_data$D2+1))
hist((working_data$D3))
hist(log(working_data$D3+1))
hist((working_data$D4))
hist(sqrt(working_data$D4))

hist(working_data$None)

plot(varImp(logistic_model))
View(working_data)
#create model

#create trainControl for reuse 
trainControl <- trainControl(method = "repeatedcv", number = 5 , repeats = 5) 

###################################
####### Logistic Regression #######
###################################

logistic_model <- train(target ~ D0 + D1 + D2 + D3 + D4 + None , train_data, 
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
coef(logistic_model$finalModel)

#ROC and AUC 
logistic_prediction <- prediction(as.numeric(logistic_pred_test),test_data$target)
logistic_perf <- performance(logistic_prediction, "tpr", "fpr")
plot(logistic_perf, main="ROC Curve")

#AUC 
logistic_auc <- performance(rf_prediction,"auc")
logistic_auc_value <- logistic_auc@y.values[[1]] #area under curve
logistic_auc_value

###################################
######### Random Forrest ##########
###################################
#rf_trainControl <- trainControl(method = "cv", number = 5)
randomF_model <- train(target ~ D0 + D1 + D2 + D3 + D4 + None , train_data , 
                       trControl = trainControl , 
                       method = "ranger", 
                       tuneLength = 4,
                       preProcess = c("center","scale"))
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

#ROC and AUC 
rf_prediction <- prediction(as.numeric(randomF_pred_test),test_data$target)
rf_perf <- performance(rf_prediction, "tpr", "fpr")
plot(rf_perf, colorize=T,main="ROC Curve")

#AUC 
randomF_auc <- performance(rf_prediction,"auc")
randomF_auc_value <- randomF_auc@y.values[[1]] #area under curve
randomF_auc_value

#feature selection in R 
rfe_control <- rfeControl(functions=rfFuncs, method="cv", number=10)
rfe_result <- 


###################################
############## KNN ################
###################################

knn_model <- train(target ~ D0 + D1 + D2 + D3 + D4 + None , train_data , 
                       trControl = trainControl, 
                       method = "knn", 
                       tuneLength = 20,
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

#ROC and AUC 
knn_prediction <- prediction(as.numeric(knn_pred_test),test_data$target)
knn_perf <- performance(knn_prediction, "tpr", "fpr")
plot(knn_perf, colorize=T,main="ROC Curve")

#AUC 
knn_auc <- performance(knn_prediction,"auc")
knn_auc_value <- knn_auc@y.values[[1]] #area under curve
knn_auc_value
############################################################
#Result

paste("AUC - Logistic: ", round(logistic_auc_value,4), "| Random Forrest: ",round(randomF_auc_value,4),
      "| KNN: ",round(knn_auc_value,4))

paste("F1 Score - Logistic: ",round(logis_cm$byClass[7],4),"| Random Forrest: ", round(randomF_cm$byClass[7],4),
      "| KNN: ",round(knn_cm$byClass[7],4))

paste("Accuracy - Logistic: ",round(logis_cm$overall[1],4),"| Random Forrest: ", round(randomF_cm$overall[1],4),
      "| KNN: ",round(knn_cm$overall[1],4))

paste("Kappa - Logistic: ",round(logis_cm$byClass[2],4),"| Random Forrest: ", round(randomF_cm$overall[2],4),
      "| KNN: ",round(knn_cm$overall[2],4))

#plot ROC 
plot(knn_perf, col="blue",main="ROC Curve")
plot(rf_perf, col="salmon",add=T)
plot(logistic_perf, col="green",add=T)
mtext(paste("AUC(Logistic): ",round(knn_auc_value,4)),line = -2, side=1)
mtext(paste("AUC(Random Forrest): ",round(randomF_auc_value,4)),line = -3,side=1)
mtext(paste("AUC(KNN): ",round(knn_auc_value,4)),line = -4,side=1)

#plot box plot 
featurePlot(x=working_data[3:8],y=working_data$target,scales=list(y=list(relation="free"),x=list(rot=90)))

#scatterplot matrix
featurePlot(x=working_data[3:8],y=working_data$target,plot = "pairs",auto.key = list(columns = 2))

#=====================================================================================================
# Marketne code ======================================================================================
#=====================================================================================================
# 
# head(vote_data)
# 
# #  Make Work data set 
# mentor <-vote_data[,1:8] 
# mentor$target <-vote_data[,11]
# 
# multi.hist(mentor[,3:8],freq=T,bcol="salmon")
# 
# #pca
# pca <- prcomp(mentor[,3:8],scale=T)
# summary(pca)
# plot(pca,type="l")
# biplot(pca,scale=0)
# 
# # Get the PCs
# PCs <-round(pca$x[,1:3], 2)
# summary(PCs)
# multi.hist(PCs, freq = T, bcol ="salmon")
# 
# expert <- cbind(PCs,mentor)
# head(expert)
# 
# rows <-createDataPartition(expert$target, p =0.80, list = F)
# # Create Test and Train  data
# x1_train <-expert[rows,]
# x1_test <-expert[-rows,]
# 
# table(x1_train$target)
# 
# table(x1_test$target)
# 
# # Data to model 
# keep_pc <-c("PC1", "PC2", "PC3","state", "target")
# keep <-c("None", "D0", "D1", "D2", "D3", "D4","state", "target") 
# 
# # Model with NoneHas better state + D0 + D1
# #---- Create Model Binomial 
# # Make Null model 
# model.null <-glm(target ~1, data = x1_train[keep], family ="binomial")
# coef(model.null)
# # Make Full model 
# model.full <-glm(target ~., data = x1_train[keep], family ="binomial")
# coef(model.full)
# 
# model.step <-step(model.null, scope =list(lower = model.null, upper = model.full),direction ="forward")
# summary(model.step)
# 
# model.x1_train <-glm(formula = target ~state +D0 +D1, data = x1_train[keep], family ="binomial")
# model.x1_train