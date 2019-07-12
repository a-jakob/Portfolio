#set packages
library(tidyverse)
library(corrgram)
library(corrplot)
library(caTools)
#setting the theme for all the plots
theme_set(theme_bw())
#load data
url<-getURL('https://raw.githubusercontent.com/a-jakob/Portfolio/master/Linear-Regression/bikeshare.csv')
df<-read.csv(text=url)
head(df)
View(df)
#Exploratory data analysis: plot count vc temp
ggplot(df,aes(x=temp,y=count))+
  geom_point(alpha=0.2,aes(color=temp))+
  theme_bw()
#converting variable date to POSIXct
df$datetime<-as.POSIXct(df$datetime)
#then other plot
ggplot(df,aes(x=datetime,y=count))+
  geom_point(alpha=0.2,aes(color=temp))+
  scale_color_gradient(high='orange',low='blue')+
  theme_bw()
#turning df as numeric to analyse correlation
df.num<-sapply(df,is.numeric)
#then check the correlations
cor.df<-cor(df[,df.num])
#but we are now interested only in temp and count
cor(df[,c('temp','count')])
#making boxplot showing that each season (transformed as factor since it comes as numeric) has its own logic of distribution.
ggplot(df,aes(x=factor(season),y=count))+
  geom_boxplot(aes(color=factor(season)))
#creating a hour variable  
time.stamp <- df$datetime
hours<-format(time.stamp, "%H")
df$hours<-hours
rm(time.stamp)
rm(hours)
df$hours<-as.numeric(df$hours)
#plot count x hour, considering only workingdays
ggplot(filter(df,workingday==1),aes(x=hours,y=count))+
  geom_point(position=position_jitter(w=1, h=0),alpha=0.4,aes(color=temp))+
  scale_colour_gradientn(colours=c('green','purple'))
#then the same plot to not workingdays
ggplot(filter(df,workingday==0),aes(x=hours,y=count))+
  geom_point(position=position_jitter(w=1, h=0),alpha=0.4,aes(color=temp))+
  scale_colour_gradientn(colours=c('green','purple'))
#building a model only with temp
temp.model<-lm(count~temp,df)
summary(temp.model)
#predict count for a temperature of 25 Celsius degrees
#I could create a data frame with 1 observation and run a prediction...
temp.test<-data.frame(temp=25)
predict(temp.model,temp.test)
#...or use a 1 degree equasion a*x+b to determine the value
as.numeric(temp.model$coefficients[1]+temp.model$coefficients[2]*25)

#now a prediction based in almost all variables
model <- lm(count ~ . -casual - registered -datetime -atemp,df)
corrgram(df)


