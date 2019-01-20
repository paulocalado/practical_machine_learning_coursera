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
