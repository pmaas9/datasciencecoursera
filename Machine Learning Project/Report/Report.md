---
title       : "Practical Machine Learning Project:"
subtitle    : Predicting Quality of Weight Lifting Exercises from Activity Monitor Data
author      : "By: Coursera Student"
job         : "Job: Data Scientist"
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [bootstrap]            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
fontsize    : 10pt

--- .class #id 

## The Goal

To classify the quality of weight lifting exercises based on data from activity sensors.

## The Data
The training data consisted of sensor measurements from 6 male participants, 160 variables observed at 19,622 total time points, including an outcome variable ("classe") with 5 levels.

## My Approach

My approach to the problem included the following major steps:

1.  Data exploration 
2.  Data preprocessing
3.  Model selection and cross-validation
4.  Final model fitting
5.  Prediction on the test data

--- .class #id 

## 1 & 2. Data Exploration and Data Preprocessing

By exploring the data, I identified an number of issues that informed my choices about how to best prepare (preprocess) the dataset for the analysis.


__Issue #1:__ Many variables (104 of them) were almost entirely missing in the dataset, had all unique values, or all had the same value.  
__Decision:__ Remove these variables from the analysis dataset (as they are not informative), reducing dataset to 56 variables.


__Issue #2:__ This still leaves a lot of possible predictors and some are likely to be correlated.   
To use them all may cause trouble with model fitting.  
**Decision:** Use principle components to reduce the number of predictors. (I used the first 10).


**Issue #3:** 19,622 is a lot of observations. Some methods (random forests, bagging, etc.) may be computationally intensive, and may not be feasible to carry out in a reasonable time frame.  
**Decision:** Consider using faster methods (multinomial regression model, k-means clustering)

--- .class #id 

## 3. Model Selection and Cross-Validation
I wanted to try out a couple possible models to use for prediction.  I decided to implement four __multinomial models__ (with different model forms) and a __k-means clustering__ method. 

__How did I select a final model from these different options?__

* I split the training data into 10 different sets (folds).
* For each cross-validation set, I fit all the models on the other 9 sets (including preprocessing to get principle components).
* I then made predictions for the hold-out cross-validation set.

This allowed me to estimate the out-of-sample error rate for the different possible models.  

The __k-means clustering__ approach (accuracy = 0.95) dramatically outperformed all of the multinomial models that I examined (best accuracy = 0.63).

--- .class #id 

## Final Model and Out-of-Sample Error Rate

Due to its superior performance in my cross-validated analysis, I selected a __k-means clustering__ model, based on the first 10 principle components, to be my __final model__.

Based on the cross-validated performance (fitting the model on 90% of the training data and predicting on the final 10% for 10 folds of the data), I expected my out-of-sample accuracy to be 95%, equating to an _expected_ __out-of-sample error rate = 5%__.

I fit the final model on the entire training dataset and used the model to make predictions on the test set data (n=20).  

By submitting my predictions to the Coursera submission page, I was able to get feedback on the _actual_ out-of-sample error rate of my method.  

I correctly identified 19 of the 20 outcomes, and incorrectly classified 1 of the 20, resulting in an _actual_ __out-of-sample error rate = 5%__.

<hr>
This concludes my report on the analysis.  In the next few slides, I have included my code for the project in case you are interested.  Enjoy!

--- .class #id 

## My Code for the Project (All of It!)


```r
## Load packages and datasets
library(caret); library(nnet); library(gbm); library(survival); library(randomForest);  
library(e1071); library(rpart)
train = read.table("pml-training.csv", header=T, sep=",", stringsAsFactors=F)
test = read.table("pml-testing.csv", header=T, sep=",", stringsAsFactors=F)

# remove variables that are almost entirely missing
pare1 = which(colSums(is.na(train))>19000)
pare2 = which(colSums((train == ""))>19000)
pare = union(pare1, c(pare2, 1, 3:5))
train = train[,-pare]
test = test[, -pare]

### identify numeric variables (so in future can get PCs)
hold=ncol(train)
for(i in 1:ncol(train)){  hold[i] = mode(train[,i]) }
these = which(hold=="numeric")
```

--- .class #id 

## Code Continued

```r
## Look at the data
dim(train); names(train); summary(train)

## Split Data into Folds for Cross-Validation
fold = createFolds(train$classe, k = 10)

###### Determine Which Method is Best #######

## initialize results holders
multinom_predictions<-rep(NA, nrow(train))
multinom_predictions2<-rep(NA, nrow(train))
multinom_predictions3<-rep(NA, nrow(train))
multinom_predictions4<-rep(NA, nrow(train))
knn_predictions<-rep(NA, nrow(train))
```

--- .class #id 


```r
# evaluate all models with cross-validation
for(i in 1:10){   data_fit = train[-fold[[i]],]; cross_fit = train[fold[[i]],]
# reduce covariate space with principle components
hold = preProcess(data_fit[,these], method="pca", na.rm=T, pcaComp=10)
data_pcs = predict(hold, data_fit[,these]);  data_full = cbind(data_fit[,-these], data_pcs)
cross_pcs = predict(hold, cross_fit[,these]); cross_full = cbind(cross_fit[,-these], cross_pcs)
# tell method to train
multinom_model <-multinom(classe ~ ., data = data_full)
multinom_model2 <-multinom(classe ~ as.factor(user_name) + as.factor(new_window) + PC1 + PC2 +  
        PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = data_full)
multinom_model3 <-multinom(classe ~ as.factor(user_name) + as.factor(new_window) + (PC1 + PC2 + 
        PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10)*as.factor(user_name), data = data_full)
multinom_model4 <-multinom(classe ~ as.factor(user_name) + as.factor(new_window) + (PC1 + PC2 + 
        PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10)^2, data = data_full)
modelKNN <- knn3(classe ~ ., data = data_full, k = 5, prob = TRUE)
# predict on fold held out from fitting
multinom_predictions[fold[[i]]] <- predict(multinom_model,  cross_full)
multinom_predictions2[fold[[i]]] <- predict(multinom_model2,  cross_full)
multinom_predictions3[fold[[i]]] <- predict(multinom_model3,  cross_full)
multinom_predictions4[fold[[i]]] <- predict(multinom_model4,  cross_full)
knn_predictions[fold[[i]]]  <- predict(modelKNN, cross_full, type = "class")   }
```

--- .class #id 

## Code Continued

```r
###  Calculate the Cross-Validated Accuracy Rate of All Models
mean( multinom_predictions==as.numeric(train$classe ) )
mean( multinom_predictions2==as.numeric(train$classe ) )
mean( multinom_predictions3==as.numeric(train$classe ) )
mean( multinom_predictions4==as.numeric(train$classe ) )
mean( knn_predictions==as.numeric(train$classe ) )
###### Selected Best Method: KNN clustering! #######
# fit on entire training data
data_fit = train; cross_fit = test
# reduce covariate space with principle components
hold = preProcess(data_fit[,these], thresh=0.95, method="pca", na.rm=T, pcaComp=10)
data_pcs = predict(hold, data_fit[,these]); data_full = cbind(data_fit[,-these], data_pcs)
cross_pcs = predict(hold, cross_fit[,these]); cross_full = cbind(cross_fit[,-these], cross_pcs)
# fit final model
final_model <-knn3(classe ~ ., data = data_full, k = 5, prob = TRUE)
# predict for test data
test_predictions<- predict(final_model, cross_full, type = "class")
```

--- .class #id 

## Code Continued

```r
# function to write out files for submitting answers
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
## prepare my answers for submission
answers = as.character(test_predictions)
## create submission files
pml_write_files(answers)

## THE END!
```








<!---## The Goal

My objective was to build a model for classifying the quality of weight lifting exercises based on data from activity sensors.

| My Approach | The Data |
| :-------: | :-------: |
|My approach to the problem included the following major steps:| what |
| 1.  Data exploration                       | hi  |
| 2.  Data preprocessing                     | hi1 |
| 3.  Model selection and cross-validation   | hi2 |
| 4.  Final model fitting                    | hi3 | 
| 5.  Prediction on the test data            | hi4 |--->


