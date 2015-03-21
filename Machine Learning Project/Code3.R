setwd("C:/Users/Paige/Documents/GitHubStuff/datasciencecoursera/Machine Learning Project")

## Load packages and datasets
library(caret)
library(nnet)
library(gbm)
library(survival)
library(randomForest)
library(e1071)
library(rpart)
train = read.table("pml-training.csv", header=T, sep=",", stringsAsFactors=F)
test = read.table("pml-testing.csv", header=T, sep=",", stringsAsFactors=F)

# remove variables that are almost entirely missing
pare1 = which(colSums(is.na(train))>19000)
pare2 = which(colSums((train == ""))>19000)
pare = union(pare1, c(pare2, 1, 3:5))
train = train[,-pare]
test = test[, -pare]

hold=ncol(train)
for(i in 1:ncol(train)){
  hold[i] = mode(train[,i])
}
these = which(hold=="numeric")

factor_vars = setdiff( seq(1:ncol(train)), these)
for(i in 1:length(factor_vars)){
  train[,factor_vars[i]] = factor(train[,factor_vars[i]])
}

## Look at the data
dim(train)
names(train)
summary(train)

## Split Data into Folds for Cross-Validation
fold = createFolds(train$classe, k = 10)

###### Determine Which Method is Best #######

forest_predictions<-rep(NA, nrow(train))
gbm_predictions<-rep(NA, nrow(train))
rpart_predictions<-rep(NA, nrow(train))
nnet_predictions<-rep(NA, nrow(train))
multinom_predictions<-rep(NA, nrow(train))
# try three approaches
for(i in 1:10){

data_fit = train[-fold[[i]],]
cross_fit = train[fold[[i]],]
# reduce covariate space with principle components
hold = preProcess(data_fit[,these], thresh=0.95, method="pca", na.rm=T)
data_pcs = predict(hold, data_fit[,these])
cross_pcs = predict(hold, cross_fit[,these])

data_full = cbind(data_fit[,-these], data_pcs)
cross_full = cbind(cross_fit[,-these], cross_pcs)

# tell method to train with random forests
 #forest_model <-train(classe ~ ., data=data_full, method="rf")#, subset=sample(seq(1,nrow(data_full)), size=500))
# gbm_model <-train(classe ~ ., data=data_full, method="gbm", quietly=TRUE)#, subset=sample(seq(1,nrow(data_full)), size=500))
 rpart_model <-train(classe ~ ., data=data_full, method="rpart")#, subset=sample(seq(1,nrow(data_full)), size=500))
 #nnet_model <-train(classe ~ ., data=data_full, method="nnet")#, subset=sample(seq(1,nrow(data_full)), size=500))
 multinom_model <-multinom(classe ~ ., data = data_full)

# predict on fold held out from fitting
#forest_predictions[fold[[i]]] <- predict(forest_model, cross_full)
#gbm_predictions[fold[[i]]] <- predict(gbm_model,  cross_full)
rpart_predictions[fold[[i]]] <- predict(rpart_model,  cross_full)
 #nnet_predictions[fold[[i]]] <- predict(nnet_model,  cross_full)
 multinom_predictions[fold[[i]]] <- predict(multinom_model,  cross_full)
}
#mean( forest_predictions==as.numeric(train$classe ) )#, na.rm=T)
#mean( gbm_predictions==as.numeric(train$classe ) )
mean( rpart_predictions==as.numeric(train$classe ) )
mean( nnet_predictions==as.numeric(train$classe ) )
mean( multinom_predictions==as.numeric(train$classe ) )
for(i in 1:10){
  
  data_fit = train[-fold[[i]],]
  cross_fit = train[fold[[i]],]
  # reduce covariate space with principle components
  hold = preProcess(data_fit[,these], thresh=0.95, method="pca", na.rm=T)
  data_pcs = predict(hold, data_fit[,these])
  cross_pcs = predict(hold, cross_fit[,these])
  
  data_full = cbind(data_fit[,-these], data_pcs)
  cross_full = cbind(cross_fit[,-these], cross_pcs)
  
  # tell method to train with random forests
  forest_model <-train(classe ~ ., data=data_full, method="rf")#, subset=sample(seq(1,nrow(data_full)), size=500))
  # predict on fold held out from fitting
  forest_predictions[fold[[i]]] <- predict(forest_model, cross_full)
}
# determine accuracy of different methods
mean( forest_predictions==as.numeric(train$classe ) )#, na.rm=T)

###### Selected Method:  #######

# fit on entire training data
data_fit = train
cross_fit = test
# reduce covariate space with principle components
hold = preProcess(data_fit[,these], thresh=0.95, method="pca", na.rm=T)
data_pcs = predict(hold, data_fit[,these])
cross_pcs = predict(hold, cross_fit[,these])

data_full = cbind(data_fit[,-these], data_pcs)
cross_full = cbind(cross_fit[,-these], cross_pcs)

final_model <-train(classe ~ ., data=data_fit, method="???")

# predict for test data
test_predictions<- predict(forest_model, cross_fit)

# function to write out files for submitting answers
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

answers = rep("A", 20) ## PUT IN YOUR OWN ANSWERS
pml_write_files(answers)



