#loading packages
library(readxl)
library(readr)
library(tidyverse)
library(caret)
library(knitr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)

#loading and taking a first look on the data
titanic0<-read_xls("titanic3.xls")
View(titanic0)
class(titanic0)
head(titanic0)
names(titanic0)
dim(titanic0)
summary(titanic0)
#looking for NAs
nas0<-data.frame(pclass=sum(is.na(titanic0$pclass)),survived=sum(is.na(titanic0$survived)),sex=sum(is.na(titanic0$sex)),age=sum(is.na(titanic0$age)),sibsp=sum(is.na(titanic0$sibsp)),parch=sum(is.na(titanic0$parch)), fare=sum(is.na(titanic0$fare)), embarked=sum(is.na(titanic0$embarked)),cabin=sum(is.na(titanic0$cabin)),boat=sum(is.na(titanic0$boat)))
kable(nas0)
#selecting usefull variables
titanic<-titanic0%>%select(pclass,survived,sex,age,sibsp,parch,fare,embarked)

#understanding the distribution of each variable
histogram(titanic$pclass)
summary<-data.frame(survived=sum(titanic$survived==1),non_survived=sum(titanic$survived==0))
kable(summary)
?histogram
sex<-data.frame(Male=sum(titanic$sex=="male"),Female=sum(titanic$sex=="female"))
kable(sex)

nas<-data.frame(pclass=sum(is.na(titanic$pclass)),survived=sum(is.na(titanic$survived)),sex=sum(is.na(titanic$sex)),age=sum(is.na(titanic$age)),sibsp=sum(is.na(titanic$sibsp)),parch=sum(is.na(titanic$parch)), fare=sum(is.na(titanic$fare)), embarked=sum(is.na(titanic$embarked)))
kable(nas)               

mean(titanic$sibsp)
mean(titanic$parch)
#creating variable "family"
titanic$family<-titanic$sibsp+titanic$parch+1
histogram(titanic$family)

#viewing and correcting the NA on "fare"
titanic[is.na(titanic$fare),]
titanic$fare[is.na(titanic$fare)]<-median(titanic$pclass==3&titanic$embarked=="S", na.rm=TRUE)
sum(is.na(titanic$fare))
histogram(titanic$fare)

#viewing the NA on "embarked"
titanic[is.na(titanic$embarked),]
#filtering the data by similar observations on fare, class and family. Other passagers with the same characteristics have embarked in Queenstown port. So I will correct the 2 NAs with Q in the column embarked.
embarked<-titanic%>%filter(fare>=79&fare<=80&pclass=="1"&family=="1")
kable(embarked)
titanic$embarked[is.na(titanic$embarked)]<-"C"

sum(is.na(titanic$embarked))

ports<-data.frame("South Hampton"=(sum(titanic$embarked=="S")),Cherbourg=(sum(titanic$embarked=="C")),Queenstown=(sum(titanic$embarked=="Q")))
kable(ports)

#Using rpart function to predict age from all other variables from the non-NAs (except survived, which is the final goal). Then using predict function to fill the NAs.
names(titanic)
fit_age<-rpart(age~pclass+sex+sibsp+parch+fare+embarked+family,data=titanic[!is.na(titanic$age),], method="anova")
titanic$age[is.na(titanic$age)]<-predict(fit_age,titanic[is.na(titanic$age),])
sum(is.na(titanic$age))
histogram(titanic$age)

#transforming the data as factors before splitting it
titanic$embarked<-as.factor(titanic$embarked)
titanic$sex<-as.factor(titanic$sex)

#creating data partition and dividing the data into training and test set. It was common in the course using a proportion of 0.1 between training and test set. But here the entire data set is not so large (1309 rows). So I choosed a higher proportion of 0.3.

set.seed(1) # I recommend setting the seed into 1 to match yours to my results

test_index <- createDataPartition(y = titanic$survived, times = 1, p = 0.3, list = FALSE)

train <- titanic[-test_index,]
dim(train)
test0 <- titanic[test_index,]
dim(test0)
traintest<-data.frame(test=dim(test0),train=dim(train))
kable(traintest[1,])
#then removed the survived column from the test set.
test<-test0[,-2]
head(test)

#I again made a fit modell using all variables available, this time to predict the data survived.   
fit_survived<-rpart(survived~pclass+sex+sibsp+parch+fare+embarked+family,data=train,method="class")
#Here is a regression tree modell.
prp(fit_survived)

#if we plot the modell, we see how have R decided for the amount of braces of the regression tree. It has reached a total amont of 8 braces, but it is possible to see that from 4 braces on the relative error has not decreased consederably.
plotcp(fit_survived)

#now using random forest, which is basically a combination of multiples regression trees, crossing multiple combinations.
set.seed(1) #again, better set seed to 1 to match my results
rf<-randomForest(as.factor(survived)~pclass+age+sex+sibsp+parch+fare+embarked+family,data=train)

#in the plot below we checked the errors from the random forest algorithm, in red for the non survivors, green for the survivors and black for the central line combining both errors. By this plot it is possible to say that our model predict better the destiny from those who have not survived than for those who did. It is explained because there are more people who have not survived the Titanic, therefore the data available for the non survivors is larger, turning the prediction for them more accurate.
plot(rf,ylim=c(0,0.8))
legend('topright',colnames(rf$err.rate),col=1:3,fill=1:3)

#minimum error table
error<-data.frame(combined_error=min(rf$err.rate[,1]),non_survivor=min(rf$err.rate[,2]),survivor=min(rf$err.rate[,3]))
kable(error)
