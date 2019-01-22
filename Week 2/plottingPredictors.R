library(ISLR)
library(ggplot2)
library(caret)
library(Hmisc)
library(grid)
library(gridExtra)

data("Wage")
summary(Wage)

inTrain<- createDataPartition(y=Wage$wage,p=0.7,list=F)

training<- Wage[inTrain,]
testing<- Wage[-inTrain,]

featurePlot(x=training[,c("age","education","jobclass")],
            y=training$wage,
            plot = "pairs")

qplot(age, wage,data=training)

qplot(age, wage, colour=education,data=training)+
  geom_smooth(method = "lm",formula = y~x)

#grouping variables to factor levels
cutWage<- cut2(training$wage,g=3)
table(cutWage)

#ploting those groups
p1<- qplot(cutWage,age,data=training,fill=cutWage)+geom_boxplot()
p2<- qplot(cutWage,age,data=training,fill=cutWage)+geom_boxplot()+geom_jitter()

grid.arrange(p1,p2,ncol=2)

t1<- table(cutWage,training$jobclass)
t1

prop.table(t1,1)

#density plot
qplot(wage,colour=education,data=training,geom="density")

#EXPLORE DATA ONLY WITH THE TRAINING SET