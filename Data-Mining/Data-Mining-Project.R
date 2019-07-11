#first the libraries
library(tm)
library(twitteR)
library(wordcloud)
library(RColorBrewer)
library(e1071)
library(class)

#connect to twitter
setup_twitter_oauth(#here one must to put its own account keys in the following order:
  #ckey,skey,token,sectoken
  #without this none of the codes below will work.
  )
#returning tweets
soccer.tweets<-searchTwitter('soccer',n=1000,lang='en')
#grabing text data from tweets
soccer.text<-sapply(soccer.tweets,function(x) x$getText())
#cleaning text data (remove symbles and not iconv characters)
soccer.text<-iconv(soccer.text,'UTF-8','ASCII')
#
soccer.corpus<-Corpus(VectorSource(soccer.text))
#document term matrix
term.doc.matrix<-TermDocumentMatrix(soccer.corpus,
                                    control=list(removePunctuation=T,
                                                 stopwords=c('soccer',stopwords('english')),
                                                 removeNumbers=T,
                                                 tolower=T))
#convert object into matrix
term.doc.matrix<-as.matrix(term.doc.matrix)
#get word counts
word.freq<-sort(rowSums(term.doc.matrix),decreasing = T)
dm<-data.frame(word=names(word.freq),freq=word.freq)
#create wordcloud
wordcloud(dm$word,dm$freq,random.order = F,colors = brewer.pal(8,'Dark2'))
