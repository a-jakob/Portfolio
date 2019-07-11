#loading packages
library(dplyr)
library(Amelia)
library(tidyverse)
library(caTools)
#exploratory analysis
rm(list=ls())
adult<-read.csv('adult_sal.csv')
adult<-select(adult,-X)
View(adult)
#looking at type_employer variable
x<-table(adult$type_employer)
sum(adult$type_employer=='?')
x[order(x)]
#mixing both minimals in one category - unemployed
adult$type_employer<-as.character(adult$type_employer)
f1<-adult$type_employer=='Never-worked'
f2<-adult$type_employer=='Without-pay'
adult$type_employer[f1]<-'Unemployed'
adult$type_employer[f2]<-'Unemployed'
table(adult$type_employer)
rm(f1)
rm(f2)
rm(x)
#other mixings, doing the same thing otherwise, this time with gov jobs
gov<-function(x){
  if(x=='Local-gov'|x=='State-gov'){
    return('SL-gov')
  }else{
    return(x)
  }
}
adult$type_employer<- sapply(adult$type_employer,gov)
#same thing, now with self employment
self<-function(x){
  if(x=='Self-emp-inc'|x=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(x)
  }
}
adult$type_employer<- sapply(adult$type_employer,self)

table(adult$type_employer)
#done with type of employer, now lets check marital and reduce to 3 factors
table(adult$marital)
married<-function(x){
  x<-as.character(x)
  if(x=='Married-spouse-absent'|x=='Married-AF-spouse'|x=='Married-civ-spouse'){
    return('Married')
  }else if(x=='Divorced'|x=='Separated'|x=='Widowed'){
    return('Not-Married')
  }else{
    return(x)
  }
}
adult$marital<- sapply(adult$marital,married)
table(adult$marital)
#now country
table(adult$country)
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}
adult$country <- sapply(adult$country,group_country)
table(adult$country)
names(adult)[names(adult)=="country"] <- "region"
str(adult)
#correcting ? and turning back everything to factor

adult[adult == '?'] <- NA
adult$type_employer <- sapply(adult$type_employer,factor)
adult$region <- sapply(adult$region,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)

#seeing NAs graphically
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))
#omiting NAs
adult <- na.omit(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))
str(adult)
#ploting histogram of ages and region
ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()
ggplot(adult,aes(hr_per_week))+geom_histogram()
ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Set a random see so your "random" results are the same as this notebook
set.seed(101) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(adult$income, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(adult, sample == TRUE)

# Testing Data
test = subset(adult, sample == FALSE)


model = glm(income ~ ., family = binomial(logit), data = train)
summary(model)


new.step.model <- step(model)

#creating confusion matrix
test$predicted.income = predict(model, newdata=test, type="response")

table(test$income, test$predicted.income > 0.5)
(6372+1423)/(6372+1423+548+872)
#recall
6732/(6372+548)
#precision
6732/(6372+872)
