install.packages("kernlab")
library(kernlab)

data(spam)
head(spam)

#frequency of the word 'your'
plot(density(spam$your[spam$type=="nonspam"]),
     col = "blue",main="",xlab="Frequency of 'your' ")
lines(density(spam$your[spam$type=="spam"]),
      col="red")

#Density is the number of times that frequency appears amongst the emails

#let's build an algorithm where we find a cut off value C
#and if the frequency of 'your' > C the we predict it's a SPAM

#first let's choose a value for C
plot(density(spam$your[spam$type=="nonspam"]),
     col = "blue",main="",xlab="Frequency of 'your' ")
lines(density(spam$your[spam$type=="spam"]),
      col="red")
abline(v=0.5,col="black")

#let's say if the value is above 0,5 it's a SPAM, and if it's below, it's not a SPAM

#predictions
prediction<- ifelse(spam$your>0.5,"spam","nonspam")
table(prediction,spam$type)/length(spam$type)

#Accuracy
0.459+0.292
#Our prediction is about 75% accurate, that's how we would evaluate our algorithm