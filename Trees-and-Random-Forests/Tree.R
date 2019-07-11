#opening libraries
install.packages('randomForest')
library(rpart)
library(ISLR)
library(ggplot2)
library(caTools)
library(rpart.plot)
library(randomForest)
#checking data
head(College)
View(College)
#ploting
ggplot(College,aes(x=Room.Board,y=Grad.Rate,color=Private))+
  geom_point()

ggplot(College,aes(x=F.Undergrad))+
  geom_histogram(aes(fill=Private),color='black',bins=50)

ggplot(College,aes(x=Grad.Rate))+
  geom_histogram(aes(fill=Private),color='black',bins=50)
#there is one grading rate above 100, lets correct this
subset(College,Grad.Rate>100)
College['Cazenovia College','Grad.Rate'] <- 100
#splitting training and test sets
set.seed(101)
sample<-sample.split(College$Private,SplitRatio = 0.7)
train<-subset(College,sample==T)
test<-subset(College,sample==F)
#running decision tree
tree<-rpart(Private~.,method = 'class',data=train)
treePred<-predict(tree,test)
head(treePred)
#predicting according to our decision tree above
treePred<-as.data.frame(treePred)
treePred$Private<-ifelse(treePred$Yes>0.5,'Yes','No')
head(treePred)
#creating a confusion matrix
table(treePred$Private,test$Private)
#ploting
prp(tree)
#now lets make a random forest
rf.model<-randomForest(Private~.,data=train,importance=T)
rf.model$confusion

rf.model$importance



p <- predict(rf.model,test)
table(p,test$Private)
