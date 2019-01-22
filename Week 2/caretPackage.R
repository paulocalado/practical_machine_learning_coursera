library(caret)
library(kernlab)
data(spam)

#75% of the data to train the model and 25% to test
inTrain<- createDataPartition(y=spam$type, p=0.75,list=FALSE)

#subset the data to training and test
training<- spam[inTrain,]
test<- spam[-inTrain,]

dim(training)

set.seed(32343)

#creating the model
modelFit<- train(type ~.,data=training,method="glm")
modelFit
modelFit$finalModel

#predicting new samples
predictions<- predict(modelFit,newdata=test)
predictions

#evaluate the predictions
confusionMatrix(predictions,test$type)

##### cross validation data slicing lecture
set.seed(32323)

#you can either return the train or the test set with the returnTrain parameter
folds<- createFolds(y=spam$type,k=10,list=TRUE,returnTrain = T)
sapply(folds, length)

#using resampling
folds<- createResample(y=spam$type, times=10,list = TRUE)

sapply(folds, length)
folds[[1]][1:10]

#create times slices, for continuous values in time
#creates a slice that has a window of 20 samples in it, and I'm going to predict the next 10
tme<- 1:1000
folds<- createTimeSlices(y=tme, initialWindow = 20, horizon = 10)
names(folds)

folds$train[3]
folds$test[3]

###training options
