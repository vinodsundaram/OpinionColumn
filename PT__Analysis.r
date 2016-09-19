setwd("E:/NUS/Semester 2/BIG-DATA ANALYTICS TECHNOLOGY/Project")
pt<- read.csv("Polly_Toynbee_Data.csv",stringsAsFactors = FALSE)
str(pt)
head(pt)

library("readxl")
library('plyr')
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)

## Data cleaning
pt$Author<- as.factor(pt$Author)

sum(count(grep('video', pt$Link))$freq) ## those with 26 videos
pt<-pt[-grep('video', pt$Link),] ## 26 videos removed

head(pt[grep('radio', pt$Link),1])
sum(count(grep('radio', pt$Link))$freq)
pt<-pt[-grep('radio', pt$Link),] ## 1 radio removed

head(pt$Link)


pt$year<-unlist(strsplit(pt$Link[[5]],"/"))[5]
head(pt$year)
for (i in 1:nrow(pt)){
  if(unlist(strsplit(pt$Link[[i]],"/"))[5] %in% c(1980:2016))
    pt[i,"year"]<-unlist(strsplit(pt$Link[[i]],"/"))[5]
  else if(unlist(strsplit(pt$Link[[i]],"/"))[6] %in% c(1980:2016))
    pt[i,"year"]<-unlist(strsplit(pt$Link[[i]],"/"))[6]
  else if(unlist(strsplit(pt$Link[[i]],"/"))[7] %in% c(1980:2016))   
    pt[i,"year"]<-unlist(strsplit(pt$Link[[i]],"/"))[7]
  else if(unlist(strsplit(pt$Link[[i]],"/"))[8] %in% c(1980:2016))   
    pt[i,"year"]<-unlist(strsplit(pt$Link[[i]],"/"))[8]
  else 
    pt[i,"year"]<-NA
}


pt.year.freq<-count(pt$year)
pt.year.freq<-na.omit(pt.year.freq)
# Articles across years
plot(pt.year.freq$x, pt.year.freq$freq)
pt.year.freq.plot <- ggplot(pt.year.freq, aes(x, freq))+geom_bar(stat="identity")+
  ylab("Number of articles")+ theme(axis.text.x=element_text(angle=90))
pt.year.freq.plot

unique(pt$Topic)  ## 344 topics
pt.topic.freq<- count(pt$Topic)
pt.topic.freq.sorted<-pt.topic.freq[order(-pt.topic.freq$freq),] #sort
pt.topic.freq.sorted<- pt.topic.freq.sorted[!is.na(pt.topic.freq.sorted),] #rem NA
head(pt.topic.freq.sorted, na.rm=FALSE)


## Len    qdap ## need harsh help
library(qdap)
#words(pt$Article[1])


## topic specific analysis
topic<-"David Cameron" 
head(pt[grep ('Cameron',pt$Topic),])
count(pt[grep ('Miliband',pt$Topic),], c("year"))

## article analysis
freqCorpus <- Corpus(VectorSource(pt$Article))
freqCorpus <- tm_map(freqCorpus,removePunctuation)
freqCorpus<- tm_map(freqCorpus, removeWords, stopwords("en"))
mystopwords <- c("xcxabn","that","this","back","the","never","now","but","says","take","like","make","will","one","can","every","many","may","just","much","last","still","get","yet","make","even","xcxa","less")
freqCorpus<- tm_map(freqCorpus, removeWords, mystopwords)
freqCorpus <- tm_map(freqCorpus, removeNumbers) 
freqCorpus <- tm_map(freqCorpus, tolower)
freqCorpus <- tm_map(freqCorpus, stemDocument)   
#freqCorpus <- tm_map(freqCorpus, stripWhitespace)   
freqCorpus <- tm_map(freqCorpus, PlainTextDocument)
mystopwords <- c("xcxabn","that","this","back","the","never","now","but","says","take","like","make","will","one","can","every","many","may","just","much","last","still","get","yet","make","even","xcxa","less")
freqCorpus<- tm_map(freqCorpus, removeWords, mystopwords)

wordcloud(freqCorpus, min.freq = 2000, random.order = FALSE,scale=c(5,0.5),colors=brewer.pal(8, "Dark2"))

# top 25 in each year
word_year <- data.frame()
for(year in sort(unique(pt$year))){
  freqCorpus <- Corpus(VectorSource(pt$Article[pt$year == year]))
  freqCorpus <- tm_map(freqCorpus,removePunctuation)
  freqCorpus<- tm_map(freqCorpus, removeWords, stopwords("en"))
  mystopwords <- c("xcxabn","that","this","back","the","never","now","but","says","take","like","make","will","one","can","every","many","may","just","much","last","still","get","yet","make","even","xcxa","less")
  freqCorpus<- tm_map(freqCorpus, removeWords, mystopwords)
  freqCorpus <- tm_map(freqCorpus, removeNumbers) 
  freqCorpus <- tm_map(freqCorpus, tolower)
  freqCorpus <- tm_map(freqCorpus, stemDocument)
  freqCorpus <- tm_map(freqCorpus, stemDocument)
  freqCorpus <- tm_map(freqCorpus, stemDocument)
  #freqCorpus <- tm_map(freqCorpus, stripWhitespace)   
  freqCorpus <- tm_map(freqCorpus, PlainTextDocument)
  mystopwords <- c("xcxabn","that","this","back","the","never","now","but","says","take",
                   "like","make","will","one","can","every","many","may","just","much","last","still","get","yet","make","even","xcxa","less",
                   "always","great","ever","way","for","far","man","said","thats","dont")
  freqCorpus<- tm_map(freqCorpus, removeWords, mystopwords)
  tdm <- TermDocumentMatrix(freqCorpus)
  tdm_idf <- weightTfIdf(tdm, normalize = T)
  freq <- rowSums(as.matrix(tdm))   
  g <- head(sort(freq,decreasing = TRUE),15) 
  g <- as.data.frame(g)
  g$words <- rownames(g)
  rownames(g) <- NULL
  g$year <- rep(year,nrow(g))
  g$rank <- 1:nrow(g)
  word_year <- rbind(word_year,g)
}
word_year$x <- rep(1, nrow(g))
word_year$year <- as.factor(word_year$year)
ggplot(word_year) + geom_label(aes(x = x, y = rank, label = words, fill = year), size = 2.0) +
  scale_y_reverse() + theme_minimal() + facet_grid(. ~ year) + theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(),
                                                                     axis.text.y = element_blank(), axis.title.y = element_blank()) +
  scale_x_discrete()


#Creating Matrix
dtm <- DocumentTermMatrix(freqCorpus)
dtm
tdm <- TermDocumentMatrix(freqCorpus)
tdm


freq <- colSums(as.matrix(dtm))   
head(sort(freq,decreasing = TRUE),25) #top 25 frequent words throughtout her carrier
#wordcloud(names(sort(freq,decreasing = TRUE)[1:100]),sort(freq,decreasing = TRUE)[1:100])


dtms <- removeSparseTerms(dtm, 0.4)
dtms
#head(inspect(dtms))
freq1 <- colSums(as.matrix(dtms))
head(freq1,n=25)



## yearly article analysis
#pt.year.article<- pt[c("year","Article")]
#pt.year.article<- na.omit(pt.year.article)
library(stringr)

positives= readLines("positive-words.txt")
negatives = readLines("negative-words.txt")

str(polarity(pt.year.article$Article[1]))
p<-polarity(pt.year.article$Article[1])$all$polarity
#pt.year.article$polarity<- 0
pt$wordcount<-0


#for (i in (1:1624)){
#pt[i,12]<- polarity(pt[i,7])$all$polarity
#  pt[i,13]<- polarity(pt[i,7])$all$wc
#}
write.csv(pt1,"Polly_Toynbee_Data_cleaned1.csv")
pt3<- read.csv("Polly_Toynbee_Data_cleaned.csv")

pt3<-(pt3[-(1)])
names(pt3)
row.names(pt1)<-NULL


plot(pt1$year, pt1$polarity)
plot((pt1$year), pt1$wordcount)

pt2 <- pt1[(pt1$polarity < -1),]
View(pt2)
pt2[1577,1]


head(pt.topic.freq.sorted)
pt3 <- pt1[(pt1$Topic =="Economic policy"),]
count(pt1$Topic)
head(pt3$Topic)
plot(pt3$year,pt3$polarity)
abline(h=0)



library(stringr)
trial <- pt1$Article[2]
trial


trial <- str_split(trial, '\\.')
trial[[1]]
outs <- lapply(trial[[1]], polarity)
str(outs[[20]]$all$polarity)
temp_outs <- outs[str_detect(trial[[1]],'Blair')]
scores <- NULL
for(i in 1:length(temp_outs)){
  score <- temp_outs[[i]]$all$polarity
  scores <- c(scores,score)
}

sum(scores)

## personality sentiment

get_personality_sentiment <- function(x,personality){
  article <- x
  scores <- NULL
  
  if(str_detect(article,personality))
  {
    article_sentences <- str_split(article,'\\.')
    sentiments <- lapply(article_sentences[[1]],polarity)
    temp_outs <- sentiments[str_detect(article_sentences[[1]],personality)]
    print(table(str_detect(article_sentences[[1]],personality)))
    if(length(temp_outs)>0){
      for(i in 1:length(temp_outs)){
        score <- temp_outs[[i]]$all$polarity
        scores <- c(scores,score)
      }
    }
    
  } ## strdetect condition ends
  return(sum(scores))
}
z <- (get_personality_sentiment(trial,"Blair"))

for (i in (2:1622)){
  pt1[i,15]<- get_personality_sentiment(pt1[i,8],"Blair")
  print(paste0("Article",i," done"))
}

for (i in (2:1622)){
  pt1[i,16]<- get_personality_sentiment(pt1[i,8],"Brown")
  print(paste0("Article",i," done"))
}

for (i in (2:1622)){
  pt1[i,17]<- get_personality_sentiment(pt1[i,8],"Cameron")
  print(paste0("Article",i," done"))
}

for (i in (2:1622)){
  pt1[i,18]<- get_personality_sentiment(pt1[i,8],"Miliband")
  print(paste0("Article",i," done"))
}


#pt1[2,14]
get_personality_sentiment(pt1[700,8],"Miliband")
names(pt1)<-c(colnames(pt1[-(15:18)]),"Blair_Polarity","Brown_Polarity","Cameron_Polarity","Miliband_Polarity")

pt1[pt1$Miliband_Polarity < -3,1]


colnames(pt1[-(1:2)])

pt1<-(pt1[-(1)])
row.names(pt1)<-NULL


## readability
library(koRpus)
help(package = "koRpus")
?readability
str(pt1$Article)

pt1$Article[2]
names(pt1)

aCorpus <- tokenize(pt1$Article[2],"obj", lang = "en")  
readability(aCorpus, index = "Flesch")

get_readability <- function(x){
  r_obj <- tokenize(x,"obj", lang = "en")
  read_score <- readability(r_obj, index = "Flesch")
  return(read_score@Flesch$RE)
}

read_ease <- NULL
for(i in 1219:1622){
  article<- pt1[i,6]
  score <- get_readability(article)
  pt1$readability[i] <- score
  print(paste0("Article",i," done"))
}



ggplot(pt1,aes(x = as.factor(year), y = readability)) + geom_boxplot() + ggtitle("Readability of articles with Gordon Brown mentioned") + xlab("Year") + ylab("Readability") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90))
