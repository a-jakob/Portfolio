#Loadiing libraries
library(caTools)
library(neuralnet)
#loading original Data Frame
df<-read.csv('C:/Users/Bode/Desktop/serious/Cursos/Data Science and Machine Learning Bootcamp with R/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/bank_note_data.csv')

#spliting train and test set
sample<-sample.split(df$Class,SplitRatio = 0.7)
train<-subset(df,sample==T)
test<-subset(df,sample==F)
#unfortunately the neuralnets package does not accept the old form y~. so we need to write every variable in the formula. So there is a trick to call every variable without writing it.
n<-names(train)
n
f<-as.formula(paste('Class ~',paste(n[!n %in%'Class'],collapse='+')))
#now we make our model. We use linear output because we are predicting a continuous variable, we would do otherwise (F) if we did classification.
nn<-neuralnet(f,data=train,hidden=10,linear.output = F)
plot(nn)
#based on this model we run a prediction
predicted.nn.values<-compute(nn,test[1:4])
head(predicted.nn.values$net.result)
#a lot of small numbers here, to simplify I will round them
predictions <- sapply(predicted.nn.values$net.result,round)
head(predictions)
#Showing a Confusion Matrix to check how good (or bad) were our model
table(predictions,test$Class)
#Wow, we got 100%!