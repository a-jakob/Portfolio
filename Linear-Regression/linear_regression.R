#loading packages
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)

#load data set
df<-read.csv('student-mat.csv',sep=';')
#checking for NA
any(is.na(df))
#making sure characters are factors
str(df)
#yes, they are. But Fedu and Medu are numeric, but should act as factors too. (actually, for a while we let them be numeric)
#using plots to check possible correlations. First filter only numerical columns
num.cols<-sapply(df,is.numeric)
cor.data<-cor(df[,num.cols])
cor.data
corrplot(cor.data,method='color')
#this comand below is very useful
corrgram(df)
#lets make more complex. 
corrgram(df,order=T,
         lower.panel = panel.shade,
         upper.panel=panel.pie,
         text.panel = panel.txt)
ggplot(df,aes(x=G3))+geom_histogram(bins=20,alpha=0.5,fill='blue')
#split sample into test and training set
set.seed(101)
sample<-sample.split(df$G3,SplitRatio = 0.7)
train<-subset(df,sample==T)
test<-subset(df,sample==F)
#running a linear regression model
model<-lm(G3~.,data=train)
#the stars in summary show how relevant is the variable for the prediction
summary(model)
#looking at residuals, which are the distance between the points and the linear regression.
res<-residuals(model)
res<-as.data.frame(res)
head(res)
ggplot(res,aes(res))+geom_histogram(fill='blue',alpha=0.5)
#we want the residuals distributions to be a normal distribution
#below we make 4 plots with the regression validation (study this later)
plot(model)
#apply model to test set
G3.predictions<-predict(model,test)
#making a new data frame with the predictions and actual values of G3. 
results<-cbind(G3.predictions,test$G3)
colnames(results)<-c('predicted','actual')
results<-as.data.frame(results)
head(results)
#our model predicted many negative grades, but the lowest possible grade is 0. Let's make a function to correct this.
to_zero<-function(x){
  if(x<0){
    return(0)
  }else{
    return(x)
  }
}
#and apply the function to results df
results$predicted<-sapply(results$predicted,to_zero)
#Now we check the accuracy of the model usin Mean Square Error
mse<-mean((results$actual-results$predicted)^2)
mse
#and by Rooted Mean Square Error
sqrt(mse)
#sum of square errors
SSE<-sum((results$predicted-results$actual)^2)
#sum of square total
SST<-sum((mean(df$G3)-results$actual)^2)
#formula for square error is 1-SSE/SST
R2<-1-SSE/SST
R2
