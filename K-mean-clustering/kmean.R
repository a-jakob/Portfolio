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
