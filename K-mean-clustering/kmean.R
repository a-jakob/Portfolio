#in this exercise we will deconsider the species classification of iris data set, run a kmeans clustering and then check how well die the kmeans clustered according other criterias. There is no right or wrong in clusering, we compare to especies here only to have an idea
library(ISLR)
library(ggplot2)
library(cluster)
library(dplyr)
library(tidyverse)
head(iris)
class(iris)
ggplot(iris,aes(Petal.Length,Petal.Width,color=Species))+
  geom_point(size=4)
set.seed((101))
#now we already know that we are dealing with 3 species, but normally we wont know. Se we need to use our instint or background knoledge on the object to set the centers,s
irisCluster<-kmeans(iris[,1:4],centers=3,nstart = 20)
irisCluster

#checking how well did we cluster comparing with the original species variable
table(irisCluster$cluster,iris$Species)
#ploting a cluster
clusplot(iris,irisCluster$cluster,
        color=T,shade=T,labels=0,lines=0)

#so lets run a unknown project. First open the original data sets
rm(irisCluster)
df1<-read.csv('C:/Users/Bode/Desktop/serious/Cursos/Data Science and Machine Learning Bootcamp with R/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/winequality-red.csv',sep=';')
df2<-read.csv('C:/Users/Bode/Desktop/serious/Cursos/Data Science and Machine Learning Bootcamp with R/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/winequality-white.csv',sep=';')
head(df1)
#adding a label to both data frames
df1$label<-'red'
df2$label<-'white'
View(df1)
View(df2)
#binding both in a unique data frame
wines<-rbind(df1,df2)
View(wines)
class(wines)
rm('df1')
rm('df2')
#now exploring through graphics
ggplot(wines,aes(x=residual.sugar))+
  geom_histogram(position='dodge2',aes(fill=label),color='black',bins=50)+
  scale_fill_manual(values=c('#ae4554','#faf7ea'))

ggplot(wines,aes(x=citric.acid))+
  geom_histogram(position='dodge2',aes(fill=label),color='black',bins=50)+
  scale_fill_manual(values=c('#ae4554','#faf7ea'))

ggplot(wines,aes(x=alcohol))+
  geom_histogram(position='dodge2',aes(fill=label),color='black',bins=50)+
  scale_fill_manual(values=c('#ae4554','#faf7ea'))

ggplot(wines,aes(x=citric.acid,y=residual.sugar))+
  geom_point(aes(alpha=0.2,color=label))+
  scale_color_manual(values=c('#ae4554','#faf7ea'))+
  theme_dark()

ggplot(wines,aes(x=volatile.acidity,y=residual.sugar))+
  geom_point(aes(color=label),alpha=0.2)+
  scale_color_manual(values=c('#ae4554','#faf7ea'))+
  theme_dark()

clus.data<-select(wines,-label)
#building a kmeans function
wine.cluster<-kmeans(clus.data,centers=2,nstart = 20)
(wine.cluster$centers)
table(wines$label,wine.cluster$cluster)

clusplot(clus.data,wine.cluster$cluster,
         color=T,shade=T,labels=1,lines=0)
#well, it happens that clustering types of wines according to its features isn't good. Maybe because there is some rosé wines classified technically as white, maybe because we didn't have the right chemical characteristics of wines enough to predict them as white or red (or maybe even as rosé and other types of wine that I dont know). 