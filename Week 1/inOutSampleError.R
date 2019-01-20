library(kernlab)

data(spam)
set.seed(333)

smallSpam<- spam[sample(dim(spam)[1],size=10),]
spamLabel<- (smallSpam$type=="spam")*1+1

plot(smallSpam$capitalAve,col=spamLabel)

#bulding a predictor based on the number of capital letters