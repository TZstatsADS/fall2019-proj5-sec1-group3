---
title: "Song Lyrics Data Story"
author: "Daniel Weiss, dmw2180"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r load libraries, warning=FALSE, message=FALSE,echo=FALSE}
library(tidyverse)
library(tidytext)
library(plotly)
library(DT)
library(tm)
library(data.table)
library(scales)
library(wordcloud)
library(gridExtra)
library(ngram)
library(dplyr)
library(janeaustenr)
library(textdata)
library(igraph)
library(ggraph)
library(wordcloud2)
library(reshape2)
library(sentimentr)
library(syuzhet)
library(factoextra)
library(ggpubr)
library(ggplot2)


word_count=function(str){
  library(stringr)
  return(str_count(str, '\\w+'))
}


```

```{r load data, warning=FALSE, message=FALSE}
# load lyrics data
load('../output/processed_lyrics.RData')
genre_list <- c("Country", "Electronic", "Folk", "Hip-Hop", "Indie", "Jazz", "Metal", "Pop", "Rock", "R&B", "Other")

dt_lyrics$words = word_count(dt_lyrics$lyrics)
dt_lyrics.emo=filter(dt_lyrics, 
                     genre%in%genre_list) %>%
              filter(year>=2010) %>%
              filter(year<2020)

```



```{r organize data, warning=FALSE, message=FALSE}

dt_lyrics.emo$genre=factor(dt_lyrics.emo$genre)
dt_lyrics.emo$genreOrdered=reorder(dt_lyrics.emo$genre, 
                                  dt_lyrics.emo$words, 
                                  mean, 
                                  order=T)

lyrics.list=NULL
for(i in 1:nrow(dt_lyrics.emo)){
  lyrics=dt_lyrics.emo$lyrics[i]
  if(dt_lyrics.emo$words[i]>0){
    emotions=get_nrc_sentiment(lyrics)
    emotions=as.matrix(emotions)/dt_lyrics.emo$words[i]
    lyrics.list=rbind(lyrics.list, 
                        cbind(dt_lyrics.emo[i,],
                              emotions))
  }
}

lyr_emo.summary=tbl_df(lyrics.list)%>%
  group_by(genre)%>%
  summarise(
    anger=mean(anger),
    anticipation=mean(anticipation),
    disgust=mean(disgust),
    fear=mean(fear),
    joy=mean(joy),
    sadness=mean(sadness),
    surprise=mean(surprise),
    trust=mean(trust)
  )

lyr_emo1.summary=tbl_df(lyrics.list)%>%
  group_by(genre)%>%
  summarise(
    negative=mean(negative),
    positive=mean(positive)
  )
lyr_emo1.summary[,2]<-lyr_emo1.summary[,2]*-1

```

```{r wordcloud setup, message=FALSE, echo=FALSE}
corpus <- VCorpus((VectorSource(dt_lyrics$stemmedwords)))
dtm <- DocumentTermMatrix(corpus,control = list(bounds=list(global=c(5,Inf)),removeNumbers=T, weighting=weightTf))
tidy_dtm = tidy(dtm)
word_mapping <- summarise(group_by(tidy_dtm, term), n=sum(count))

```

```{r wordcloudimplement, warning=FALSE, message=FALSE,echo=FALSE}
wordcloud2(word_mapping[word_mapping$n>15000,],size=1,minRotation = -pi/6,maxRotation = pi/6,rotateRatio = 0.4)
```




```{r barplot emo, fig.height=5, fig.width=8}

lyr_emo2.bars=melt(lyr_emo.summary, id.vars = c("genre"),
                measure.vars = colnames(lyr_emo.summary)[2:9])

ggplot(lyr_emo2.bars, aes(x=genre, y=value)) +
  geom_bar(aes(fill = variable),stat = "identity")+

  labs(title = "Emotional Distribution of Lyrics by Genre")+
  xlab("Genre") +
  ylab("Emotional Scores")

```

```{r barplot sent, fig.height=5, fig.width=8}

lyr_emo1.bars=melt(lyr_emo1.summary, id.vars = c("genre"),
                measure.vars = c("positive", "negative"))

ggplot(lyr_emo1.bars, aes(x=genre, y=value)) +
  geom_bar(aes(fill = variable),stat = "identity")+

  labs(title = "Sentiment of Lyrics by Genre")+
  xlab("Genre") +
  ylab("Average Sentiment Score")

```

```{r clusterplot, fig.height=5, fig.width=5}

lyr_emo.summary0=as.data.frame(lyr_emo.summary)
rownames(lyr_emo.summary0)=as.character((lyr_emo.summary0[,1]))
km.res=kmeans(lyr_emo.summary0[,-1], iter.max=200,
              5)
fviz_cluster(km.res, 
             stand=F, repel= TRUE,
             data = lyr_emo.summary0[,-1], xlab="", xaxt="n",
             show.clust.cent=FALSE)


```

