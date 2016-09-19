setwd("E:/NUS/Semester 2/BIG-DATA ANALYTICS TECHNOLOGY/Project")
install.packages("readxl")
library("readxl")
library('plyr')
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud)

tbl<- read_excel("ArticleData.xlsx")
head(tbl)

## trying to get the published year .
head(tbl$Link)
unlist(strsplit((tbl$Link[2000]),"/") )[5]
tbl1<-tbl

## data cleaning 
sum(count(grep('video', tbl1$Link))$freq) ## those with 317 videos
tbl1<-tbl1[-grep('video', tbl1$Link),] ## 317 videos removed

head(tbl1[grep('radio', tbl1$Link),1])
sum(count(grep('radio', tbl1$Link))$freq)
tbl1<-tbl1[-grep('radio', tbl1$Link),] ## 1142 radio removed

tbl1$year<-unlist(strsplit(tbl$Link[[5]],"/"))[5]
summary(tbl1$year)
for (i in 1:nrow(tbl)){
  if(unlist(strsplit(tbl$Link[[i]],"/"))[5] %in% c(1980:2016))
    tbl1[i,"year"]<-unlist(strsplit(tbl$Link[[i]],"/"))[5]
  else if(unlist(strsplit(tbl$Link[[i]],"/"))[6] %in% c(1980:2016))
    tbl1[i,"year"]<-unlist(strsplit(tbl$Link[[i]],"/"))[6]
  else if(unlist(strsplit(tbl$Link[[i]],"/"))[7] %in% c(1980:2016))   
    tbl1[i,"year"]<-unlist(strsplit(tbl$Link[[i]],"/"))[7]
  else if(unlist(strsplit(tbl$Link[[i]],"/"))[8] %in% c(1980:2016))   
    tbl1[i,"year"]<-unlist(strsplit(tbl$Link[[i]],"/"))[8]
  else 
    tbl1[i,"year"]<-NA
}

year.freq<-count(tbl1$year[tbl1$year > 1995])
year.freq <-na.omit(year.freq)
str(year.freq)

year.freq.plot <- ggplot(year.freq, aes(x, freq))+geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90),panel.background = element_blank()) +
  ggtitle('Number of Opinion Articles over the years')
year.freq.plot
year.freq.plot+geom_hline(yintercept=mean(year.freq$freq))




## list of categories
unique(tbl1$Topic)  ## 2088 authors (2185)
topic.freq <-na.omit(tbl1$Topic)

topic.freq<- count(topic.freq)

topic.freq.sorted<-topic.freq[order(-topic.freq$freq),]
head(topic.freq.sorted)


install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")


freqCorpus <- Corpus(VectorSource(tbl1$Topic))
freqCorpus <- tm_map(freqCorpus,removePunctuation)
freqCorpus<- tm_map(freqCorpus, removeWords, stopwords("en"))
freqCorpus <- tm_map(freqCorpus, removeNumbers) 
freqCorpus <- tm_map(freqCorpus, tolower)

freqCorpus <- tm_map(freqCorpus, PlainTextDocument)
wordcloud(freqCorpus, max.words = 150, random.order = FALSE)

## count of list of articles written by each author across time
unique(tbl1$Author) ## 178 authors (190)
author.freq <-na.omit(tbl1$Author)
author.freq <- count(author.freq)
author.freq.sorted<-author.freq[order(-author.freq$freq),]
author.freq.sorted10<-author.freq[order(-author.freq$freq),]

author.freq.sorted10<-head(author.freq.sorted,10)
row.names(author.freq.sorted10)<-NULL

newtbl <- cbind(tbl1$Author, tbl1$Topic, tbl1$year)
aggr <- aggregate(tbl1$Topic~tbl1$Author+tbl1$year, FUN=length) 
colnames(aggr) <- c("Author","Year","Article_count")
sorted.author.article <- aggr[order(aggr$Author,aggr$Year),]

plot(sorted.author.article$Year, sorted.author.article$Article_count)


author.freq.plot <- ggplot(author.freq.sorted10, aes (x,freq))+geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90),panel.background = element_blank()) +
  ggtitle('Authors over the years') + coord_flip()
author.freq.plot

#unique(tbl1$Author[tbl1$year ==2015])

## Author specific analysis - Polly Toynbee
tbl1[tbl1$Author == "Aditya Chakrabortty",]
a <- subset(tbl1, tbl1$Author=="Polly Toynbee")
count(a$year)
at <- count(a$Topic)
head(at[order(-at$freq),],10)


freqCorpus <- Corpus(VectorSource(a$Topic))
freqCorpus <- tm_map(freqCorpus, PlainTextDocument)
wordcloud(freqCorpus, max.words = 120, random.order = FALSE)


freqCorpus <- Corpus(VectorSource(a$Article))
freqCorpus <- tm_map(freqCorpus,removePunctuation)
freqCorpus<- tm_map(freqCorpus, removeWords, stopwords("en"))
freqCorpus <- tm_map(freqCorpus, removeNumbers) 
freqCorpus <- tm_map(freqCorpus, tolower)
freqCorpus <- tm_map(freqCorpus, stemDocument)   
freqCorpus <- tm_map(freqCorpus, stripWhitespace)   
freqCorpus <- tm_map(freqCorpus, PlainTextDocument)

wordcloud(freqCorpus, min.freq = 1000, random.order = FALSE)

