library(ISLR)
library(caret)

data(Wage)

inTrain<- createDataPartition(y=Wage$wage,
                              p=0.7, list=F)

trainingt<- Wage[inTrain,]
testing<- Wage[-inTrain,]

#turn qualitative and factor variables into Dummy variables (quantitative variables)

table(trainingt$jobclass)

dummies<- dummyVars(wage~jobclass, data=trainingt)
head(predict(dummies,newdata = trainingt))

#removing zero covariates
nsv<- nearZeroVar(trainingt,saveMetrics = T)
nsv

#spline basis
library(splines)
bsBasis<- bs(trainingt$age, df=3)
bsBasis

lm1<- lm(wage ~bsBasis, data=trainingt)
plot(trainingt$age, trainingt$wage,pch=19,cex=0.5)
points(trainingt$age,predict(lm1,newdata = trainingt),col="red",pch=19,cex=0.5)

#do it in the test set in the same way that we did on the training set
predict(bsBasis, age=testing$age)
