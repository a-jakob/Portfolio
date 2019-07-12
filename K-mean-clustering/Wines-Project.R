#first the libraries
library(ISLR)
library(ggplot2)
library(cluster)
library(dplyr)
library(tidyverse)
#Opening original data sets
url1<-getURL('https://raw.githubusercontent.com/a-jakob/Portfolio/master/K-mean-clustering/winequality-red.csv')
url2<-getURL('https://raw.githubusercontent.com/a-jakob/Portfolio/master/K-mean-clustering/winequality-white.csv')
df1<-read.csv(text=url1,sep=';')
df2<-read.csv(text=url1,sep=';')
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