library(caret)
library(kernlab)
library(RANN)

data("spam")

inTrain<- createDataPartition(y=spam$type, p=0.75, list=F)

training<- spam[inTrain,]
test<- spam[-inTrain,]

hist(training$capitalAve,main="",xlab="Ave. capital run lenght")

mean(training$capitalAve)

sd(training$capitalAve)

#standardizing variables
trainCapAve<- training$capitalAve
trainCapAveStandardize<- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveStandardize)

sd(trainCapAveStandardize)

#standardizing the test set, use the same parameters
testCapAve<- test$capitalAve
testCapAveStardidize<- (testCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveStardidize)
sd(testCapAveStardidize)

#standardizing with preprocess function
preObj<- preProcess(training[,-58], method = c("center","scale"))
trainCapAveStandardize<- predict(preObj,training[,-58])$capitalAve

mean(trainCapAveStandardize)
sd(trainCapAveStandardize)

#preprocessing the test sample
testCapAveStardidize<- predict(preObj,test[,-58])$capitalAve
mean(testCapAveStardidize)
sd(testCapAveStardidize)

#preprocess direct throughout the creation of the model
set.seed(32343)
modelFit<- train(type~.,data=training,preProcess=c("center","scale"),method="glm")
modelFit

#standardize box-cox transformations
preObj<- preProcess(training[,-58],method = c("BoxCox"))
trainCapAveStandardize<- predict(preObj, training[,-58])$capitalAve

par(mfrow=c(1,2))
hist(trainCapAveStandardize)
qqnorm(trainCapAveStandardize)

#imputing data

set.seed(13343)

#making values NA
training$capAve<- training$capitalAve
selectNA<- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA]<- NA

#impute and standardize
preObj<- preProcess(training[,-58],method = c("knnImpute"))
capAve<- predict(preObj,training[,-58])$capAve

#Standardize true values
capAveTruth<- training$capitalAve
capAveTruth<- (capAveTruth - mean(capAveTruth))/sd(capAveTruth)
