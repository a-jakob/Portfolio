install.packages('neuralnet')
#open libraries
library(MASS)
library(caTools)
library(neuralnet)
library(ggplot2)
#keep in mind that the data set is from 1978
head(Boston)
str(Boston)
data<-Boston
#checking if there is NAs
any(is.na(Boston))
#here we're not going to to exploratory analysis and is good practice always normalize the data
maxs<-apply(data,MARGIN=2,max)
maxs
mins<-apply(data,MARGIN=2,min)
mins
#thats the normalization: get the data (arg1), subtract the minimun vaue (arg2) and then divide for the diference between max and minimun value (arg3)
scaled.data<-scale(data,center = mins,scale = maxs-mins)
scaled<-as.data.frame(scaled.data)
#spliting train and test data
split<-sample.split(scaled$medv,SplitRatio = 0.7)
train<-subset(scaled,split==T)
test<-subset(scaled,split==F)
#unfortunately the neuralnets package does not accept the old form y~. so we need to write every variable in the formula. So there is a trick to call every variable without writing it.
n<-names(train)
f<-as.formula(paste('medv ~',paste(n[!n %in%'medv'],collapse='+')))
#now we make our model. We use linear output because we are predicting a continuous variable, we would do otherwise (F) if we did classification.
nn<-neuralnet(f,data=train,hidden=c(5,3),linear.output = T)
#plot neural net. Ps. The blue lines are the bias introduced
plot(nn)
#in neural nets we run predictions with compute, not predict
predicted.nn.values<-compute(nn,test[1:13])
#and now we need to convert our scaled data to the initial values just reverting the equasion.
true.predictions<-predicted.nn.values$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
#convert the test data
test.r<-test$medv*(max(data$medv)-min(data$medv))+min(data$medv)
#and lets take the Mean Squared Error
MSE.nn<-sum((test.r-true.predictions)^2/nrow(test))
MSE.nn
error.df<-data.frame(test.r,true.predictions)
head(error.df)
#plotting
ggplot(error.df,aes(x=test.r,y=true.predictions))+
  geom_point()+
  stat_smooth()
