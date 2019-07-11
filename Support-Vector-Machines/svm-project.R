#libraries and theme
library(ggplot2)
library(caTools)
library(e1071)
theme_set(theme_bw())

#opening and analysing original dataset
loans<-read.csv('C:/Users/Bode/Desktop/serious/Cursos/Data Science and Machine Learning Bootcamp with R/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/loan_data.csv')
str(loans)
summary(loans)
View(loans)
#transforming relevant variables as vectors
loans$credit.policy <- factor(loans$credit.policy)
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)
str(loans)
#plotting data
ggplot(loans,aes(x=fico))+
  geom_histogram(aes(fill=not.fully.paid),color='black',bins=40,alpha=0.5)+
  scale_fill_manual(values=c('green','red'))

ggplot(loans,aes(x=factor(purpose)))+
  geom_bar(aes(fill=not.fully.paid),position = 'dodge')

ggplot(loans,aes(x=int.rate,y=fico))+
  geom_point(aes(color=not.fully.paid),alpha=0.2)

#spliting train and test sets
set.seed(101)
spl<-sample.split(loans$not.fully.paid,0.7)
train<-subset(loans,spl==T)
test<-subset(loans,spl==F)
#creating a svm model
model<-svm(not.fully.paid~.,data=train)
summary(model)
model$gamma
#predicting in test
pred.values<-predict(model,test[1:13])
table(pred.values,test[,14])
#our actual model is really bad, according to it, everibody pays fully.
#the I need to tune the model
tune.results<-tune(svm,
                   train.x=not.fully.paid~.,
                   data=train,
                   kernel='radial',
                   ranges=list(cost=c(1,10),
                               gamma=c(0.1,1)))
summary(tune.results)

model <- svm(not.fully.paid ~ .,data=train,cost=10,gamma = 0.1)
predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)
#so, we started predicting some of the 1 values. We could test longer for costs and gamma values, but that takes to much time.