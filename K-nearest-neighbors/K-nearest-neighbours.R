library(ISLR)
#first exploratory analysis
View(Caravan)
dim(Caravan)
str(Caravan)
summary(Caravan$Purchase)
any(is.na(Caravan))
#our prediction is on purchases, so lets make a vector of it
purchase<-Caravan$Purchase
#now that i know that there is no NA I can move on
#Showing variance of the two first columns, just to show that there is a large difference between the scales
var(Caravan[,1])
var(Caravan[,2])
#so we need to standardize entire dataset (minus purchase column)
standardized.Caravan<-scale(Caravan[,-86])
#lets see the 1st and 2nd variance again
var(standardized.Caravan[,1])
var(standardized.Caravan[,2])
#train and test split
test.index<-1:1000
test.data<-standardized.Caravan[test.index,]
test.purchase<-purchase[test.index]
train.data<-standardized.Caravan[-test.index,]
train.purchase<-purchase[-test.index]

#KNN model
library(class)
set.seed(101)
#runing a model. Using knn function we have 1.the training data, 2. the test data, 3. the feature we want to predict. We setted k=1 for now (later we test other ks)
predicted.purchase<-knn(train.data,test.data,train.purchase,k=1)
head(predicted.purchase)
summary(predicted.purchase)
#checking error
misclass.error<-mean(test.purchase!=predicted.purchase)
misclass.error

#using the elbow method to find a better K
predicted.purchase<-NULL
error.rate<-NULL
#running a for loops to test Ks
for(i in 1:20){
  set.seed(101)
  predicted.purchase<-knn(train.data,test.data,train.purchase,k=i)
  error.rate[i]<-mean(test.purchase!=predicted.purchase)
}
#lets see which is the K with the minimum error rate throught a plot
library(ggplot2)
k.values<-1:20
error.df<-data.frame(error.rate,k.values)
ggplot(error.df,aes(k.values,error.rate))+geom_point()+geom_line(lty='dotted',color='red')

which(error.rate==min(error.rate))

