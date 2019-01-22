library(caret)
library(kernlab)
data(spam)

inTrain<- createDataPartition(y=spam$type,
                              p=0.75, list=F)
training<- spam[inTrain,]
testing<- spam[-inTrain,]

M<- abs(cor(training[,-58]))
diag(M)<- 0
which(M>0.8,arr.ind = T)

names(spam)[c(34,32)]

plot(spam[,34],spam[,32])

#rotating the plot
x<- 0.71*spam$num415 + 0.71*spam$num857
y<- 0.71*spam$num415 - 0.71*spam$num857
plot(x,y)

#we can see that most of the variability occurs in the x-axis
#so, adding the 2 variables capture the most information so we'll use the sum as a predictor

smallSpam<- spam[,c(34,32)]
prComp<- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

#rotation matrix
prComp$rotation

###other example
typeColor<- ((spam$type=="spam")*1 + 1)
prComp<- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2], col=typeColor,xlab="PC1",ylab="PC2")

#PCA with caret package
preProc<- preProcess(log10(spam[,-58]+1), method = "pca",pcaComp = 2)
spamPC<- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

##########
testePreProc<- preProcess(log10(training[,-58]+1), method = "pca",pcaComp = 2)
trainPC<- predict(testePreProc,log10(training[,-58]+1))
modelFit<- train(x=trainPC, y=training$type,method = "glm")

testPC<- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))



