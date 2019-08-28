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
<pre><code>
Generalized Linear Model 

2423 samples
   6 predictor
   2 classes: '0', '1' 

Pre-processing: centered (6), scaled (6), principal component signal extraction (6) 
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 1939, 1939, 1938, 1938, 1938 
Resampling results:

  Accuracy   Kappa        
  0.7870401  -0.0008218632
</code></pre>

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
|---|--- | --- |
|Prediction | 0 | 1 |
| 0 | 0 | 1 | 
| 1 | 515 | 1908 |

<pre><code>
               Accuracy : 0.7875          
                 95% CI : (0.7706, 0.8036)
    No Information Rate : 0.7875          
    P-Value [Acc > NIR] : 0.5118          
                  Kappa : 0               
 Mcnemar's Test P-Value : &lt2e-16
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
</code></pre>

### Predict with test dataset 

<pre><code>
logistic_pred_test <- predict(logistic_model,newdata=test_data,type="raw")
summary(logistic_pred_test)
</code></pre>

**Result**

| 0 | 1 |
|--- | --- |
| 0 | 604 |


### Confusion Matrix with test dataset 

<pre><code>
confusionMatrix(as.factor(logistic_pred_test),test_data$target,mode='everything')
</code></pre>

**Result**

Confusion Matrix and Statistics

| |Reference | |
|---|--- | --- |
|Prediction | 0 | 1 |
| 0 | 0 | 0 | 
| 1 | 128 | 476 |

<pre><code>
Accuracy : 0.7881        
                 95% CI : (0.7533, 0.82)
    No Information Rate : 0.7881        
    P-Value [Acc > NIR] : 0.5237        
                  Kappa : 0             
 Mcnemar's Test P-Value : &lt2e-16        
            Sensitivity : 0.0000        
            Specificity : 1.0000        
         Pos Pred Value :    NaN        
         Neg Pred Value : 0.7881        
              Precision :     NA        
                 Recall : 0.0000        
                     F1 :     NA        
             Prevalence : 0.2119        
         Detection Rate : 0.0000        
   Detection Prevalence : 0.0000        
      Balanced Accuracy : 0.5000        
       'Positive' Class : 0 
</code></pre>


## KNN

### Model Result 

	<pre><code>
		knn_model <- train(target ~ D0 + D1 + D2 + D3 + D4 + None , train_data , 
                       trControl = trainControl , 
                       method = "knn", 
                       preProcess = c("zv","center","scale","pca"))
	</code></pre>

*Result*

<pre><code>
	-Nearest Neighbors 

2423 samples
   6 predictor
   2 classes: '0', '1' 

Pre-processing: centered (6), scaled (6), principal component signal extraction (6) 
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 1938, 1938, 1938, 1939, 1939 
Resampling results across tuning parameters:

  k  Accuracy   Kappa     
  5  0.7482508  0.01758023
  7  0.7659973  0.04551994
  9  0.7709474  0.03792153

Accuracy was used to select the optimal model using the largest value.
The final value used for the model was k = 9.
</code></pre>

### Predict with training dataset 
<pre><code>
knn_pred_train <- predict(knn_model,type="raw")
summary(knn_pred_train)
</code></pre>

*Result*

| 0 | 1 |
|--- | --- |
| 116 | 2307 |

### Confusion Matrix with train dataset 

<pre><code>
confusionMatrix(as.factor(knn_pred_train),train_data$target,mode='everything')
</code></pre>

**Result**

| |Reference | |
|---|--- | --- |
|Prediction | 0 | 1 |
| 0 | 65 | 51 | 
| 1 | 450 | 1857 |

<pre><code>
	Accuracy : 0.7932          
                 95% CI : (0.7765, 0.8092)
    No Information Rate : 0.7875          
    P-Value [Acc > NIR] : 0.2521          
                  Kappa : 0.1387          
 Mcnemar's Test P-Value : &lt2e-16          
            Sensitivity : 0.12621         
            Specificity : 0.97327         
         Pos Pred Value : 0.56034         
         Neg Pred Value : 0.80494         
              Precision : 0.56034         
                 Recall : 0.12621         
                     F1 : 0.20602         
             Prevalence : 0.21255         
         Detection Rate : 0.02683         
   Detection Prevalence : 0.04787         
      Balanced Accuracy : 0.54974         
       'Positive' Class : 0  
</code></pre>