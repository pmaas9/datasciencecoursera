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

knn_predictions<-rep(NA, nrow(train))
# cross-validate
for(i in 1:10){

data_fit = train[-fold[[i]],]
cross_fit = train[fold[[i]],]
# reduce covariate space with principle components
hold = preProcess(data_fit[,these], thresh=0.95, method="pca", na.rm=T, pcaComp=10)
data_pcs = predict(hold, data_fit[,these])
cross_pcs = predict(hold, cross_fit[,these])

data_full = cbind(data_fit[,-these], data_pcs)
cross_full = cbind(cross_fit[,-these], cross_pcs)

# tell method to train
modelKNN <- knn3(classe ~ ., data = data_full, k = 5, prob = TRUE)

# predict on fold held out from fitting
knn_predictions[fold[[i]]]  <- predict(modelKNN, cross_full, type = "class")
}
mean( knn_predictions==as.numeric(train$classe ) )

###### Selected Method: KNN clustering! #######

# fit on entire training data
data_fit = train
cross_fit = test
# reduce covariate space with principle components
hold = preProcess(data_fit[,these], thresh=0.95, method="pca", na.rm=T, pcaComp=10)
data_pcs = predict(hold, data_fit[,these])
cross_pcs = predict(hold, cross_fit[,these])

data_full = cbind(data_fit[,-these], data_pcs)
cross_full = cbind(cross_fit[,-these], cross_pcs)

final_model <-knn3(classe ~ ., data = data_full, k = 5, prob = TRUE)

# predict for test data
test_predictions<- predict(final_model, cross_full, type = "class")

# function to write out files for submitting answers
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

#answers = rep("A", 20) ## PUT IN YOUR OWN ANSWERS
answers = as.character(test_predictions)
pml_write_files(answers)



