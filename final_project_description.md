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

  