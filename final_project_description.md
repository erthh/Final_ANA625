# Final Project Result

Baseline : 0.80

## Logistic Regression 

### Model Result 

<pre><code>
logistic_model <- train(target ~ D0 + D1 + D2 + D3 + D4 + None , train_data , 
                        trControl = trainControl , 
                        method = "glm", 
                        preProcess = c("zv","center","scale","pca"))

</code></pre>

*Result*

Generalized Linear Model 

2423 samples
   6 predictor
   2 classes: '0', '1' 

Pre-processing: centered (6), scaled (6), principal component signal extraction (6) 
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 1939, 1939, 1938, 1938, 1938 
Resampling results:

  *Accuracy   Kappa        *
  0.7870401  -0.0008218632


### Predict with training dataset 
<pre><code>
logistic_pred_train <- predict(logistic_model,type="raw")
summary(logistic_pred_train)
</code></pre>

*Result*

| 0 | 1 |
|--- | --- |
| 0 | 2423 |

### Confusion Matrix with train dataset 

<pre><code>
confusionMatrix(as.factor(logistic_pred_train),train_data$target,mode='everything')
</code></pre>

**Result**

Confusion Matrix and Statistics

| |Reference | |
|Prediction |--- | --- |
| 0 | 0 | 1 | 
|   |--- | --- |
| 1 | 515 | 1908 |
                                          
               Accuracy : 0.7875          
                 95% CI : (0.7706, 0.8036)
    No Information Rate : 0.7875          
    P-Value [Acc > NIR] : 0.5118          
                  Kappa : 0               
 Mcnemar's Test P-Value : <2e-16
 			Sensitivity : 0.0000                         
            Specificity : 1.0000                         
         Pos Pred Value :    NaN                         
         Neg Pred Value : 0.7875                         
              Precision :     NA                         
                 Recall : 0.0000                         
                     F1 :     NA                         
             Prevalence : 0.2125                         
         Detection Rate : 0.0000                         
   Detection Prevalence : 0.0000                         
      Balanced Accuracy : 0.5000                         
       'Positive' Class : 0                 

### Predict with test dataset 

<pre><code>
logistic_pred_test <- predict(logistic_model,newdata=test_data,type="raw")
summary(logistic_pred_test)
</code></pre>

**Result**

| 0 | 1 |
|--- | --- |
| 0 | 604 |













