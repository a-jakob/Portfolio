#loading libraries
library(ISLR)
library(caTools)
library(class)
library(ggplot2)
#checking data set
head(iris)
dim(iris)
class(iris)
View(iris)
#Standardizing all numeric variables
IsNum <- sapply(iris,is.numeric)
standard.iris<-scale(iris[,IsNum])
View(standard.iris)
#checking variances
var(standard.iris)
iris$Species
class(standard.iris)
standard.iris<-as.data.frame(standard.iris)
standard.iris$Species<-iris$Species
#split train and test sets
dim(standard.iris)
sample<-sample.split(standard.iris$Species,SplitRatio = 0.7)
train<-subset(standard.iris,sample==T)
test<-subset(standard.iris,sample==F)
#building a knn model
predictedSpecies<-knn(train[1:4],test[1:4],train$Species,k=1)
#checking missclassification
miss<-mean(test$Species!=predictedSpecies)
miss
#so, we had an 4% of error, not bat for a single try. 
#Anyway, lets check for a better k using for loops
predictedSpecies<-NULL
errorRate<-NULL
for(i in 1:20){
  predictedSpecies<-knn(train[1:4],test[1:4],train$Species,k=i)
  errorRate[i]<-mean(test$Species!=predictedSpecies)
}
kvalues<-1:20
errors<-data.frame(errorRate,kvalues)
#ploting to see it

ggplot(errors,aes(x=kvalues,y=errorRate))+
  geom_point()+
  geom_line(color='red')
errors[which(errors$errorRate==min(errorRate)),]
#so, with a k=2 we acchieved an error rate of 2.22%
