#loading libraries
install.packages('e1071')
library(ISLR)
library(e1071)
class(iris)
model<-svm(Species~.,data=iris)
summary(model)
#the summary above show, among other things, cost and (should have presented) gamma. Cost is the hardiness of the margin, we can knowing cost set some values to be out of the model. Gamma is a complex value to set non linear values to the spliting line. Bascially, large gamma leads to high bias and low variance models and vice viersa.

#attention, the code below is only to show how I could predict a value. In this case, I should run it with a test set, never with the data itself (because the model was trained with the same data it is trying to predict). Nevertheless it is shown here for ilustrative purposes.
pred.values<-predict(model,iris[1:4])
table(pred.values,iris[,5])

#backing to our svm lesson
#tuning parameters (chosing better cost and gamma).The argument range is the place for it, it accepts a list as arguments.
tune.results<-tune(svm,train.x=iris[1:4],train.y=iris[,5],kernel='radial',ranges=list(cost=c(0.1,1,10),gamma=c(0.5,1,2)))
summary(tune.results)
#now we got an idea of the best cost and gamma, we try more specific values
tune.results<-tune(svm,train.x=iris[1:4],train.y=iris[,5],kernel='radial',ranges=list(cost=c(0.5,1,1.5),gamma=c(0.1,0.5,0.7)))
summary(tune.results)
#we could go on, but we already have an idea. Lets move on
tuned.svm<-svm(Species~.,data=iris,kernel='radial',cost=1.5,gamma=0.1)
summary(tuned.svm)
