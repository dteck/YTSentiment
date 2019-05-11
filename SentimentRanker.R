#---
#title: "Using Sentiment Analysis to Improve YouTube Recommended Video Rankings"
#author: "Mark Richards"
#date: "5/10/2019"
#GitHub: https://github.com/dteck/YTSentiment
#---

#---Install and load required packages---
if (!require(tuber)) install.packages('tuber')
if (!require(syuzhet)) install.packages('syuzhet')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(knitr)) install.packages('knitr')
if (!require(kableExtra)) install.packages('kableExtra')
if (!require(stringr)) install.packages('stringr')
library(tuber)
library(syuzhet)
library(ggplot2)
library(tidyverse)
library(knitr)
library(kableExtra)
library(stringr)
#---

#---Check to see if API Key file exists---
APIKey<-file_test("-f","APIKey.rds") #test if apikey exists
if (APIKey == FALSE) { #run code block if apikey does not exist
  print("No youtube API Key")
  print("See https://developers.google.com/youtube/v3/getting-started")
  print("expects key as RDS data.frame in form 
        APIKey<-data.frame(app_id='Your App ID Here', 
        app_secret = 'Your App Secret Here')")
  rm(APIKey) #removes file test
}else { #run code block if api key exists
  print("Try the R code here with your API key")
  print("https://raw.githubusercontent.com/dteck/YTSentiment/master/SentimentRanker_wKey.R")
}
#---

#---Link to liked youtube video---
Baselink<-"https://www.youtube.com/watch?v=HsxBw6ls7Z0" #link to a liked youtube video
#---

#---Extract video ID---
Baselink<-str_match(Baselink,"[^=]+$") #regex to extract video ID
#---

#---Read related video RDS---
RelatedVids<-readRDS("RelatedVids.rds") #Load RDS with API results
#---

#---Read the Base video comments and details RDS---
baseVid<-readRDS("baseVid.rds") #Load RDS with API results
BaseDetail<-readRDS("BaseDetail.rds") #Load RDS with API results
BaseTitle<-BaseDetail$items[[1]]$snippet$title #extract the title of the base video
#---


#---Sanitize comments and calculate emotions---

baseVid$sanitary<-iconv(baseVid$textOriginal, to="UTF-8") #sanitize to utf-8
BaseEmo<-get_nrc_sentiment(baseVid$sanitary)#get comment emotion
#---

#---Calculate Base emotion Averages---
Baseavg<-apply(BaseEmo, 2, mean)#avg of base sentiment
Baseavg<-data.frame(name=Baseavg) #df for plotting
#---

#---Plot base emotion with ggplot--- 
Avg<-ggplot(Baseavg, aes_(x=row.names(Baseavg), y=Baseavg$name,
                          fill=row.names(Baseavg)))+geom_bar(stat="identity")+theme(legend.position="none",
                                                                                    axis.title.x=element_blank(),axis.title.y=element_blank())
Avg+ggtitle(BaseTitle,paste("Video ID: ",baseVid$videoId[1]))
#---

#---Read related vid comments RDS---
RelatedComm<-readRDS("RelatedComm.rds") #Load RDS with API results
#---

#---Calculate related video emotions---
seq<-1:length(RelatedVids$rel_video_id) #set iteration length
RelatedEmo<-list() #initialize a list to store data frames
for (n in seq){ #iterate over all comments and get emotional sentiment
  RelatedEmo[[n]]<-get_nrc_sentiment(RelatedComm[[n]]$sanitary)
}
rm(n,seq)
#---

#---Calculate the averages for the related videos---
seq<-1:length(RelatedEmo) #set iteration length
RelatedEmoAvg<-list()#initialize a list to store data frames
for (n in seq){ #calculate averages of emotional sentiment
  RelatedEmoAvg[[n]]<-apply(RelatedEmo[[n]][1:10], 2, mean)
}
rm(n,seq)
#---


#---Reshape df, add video IDs---
RelatedEmoAvg<-data.frame(RelatedEmoAvg) #reshape the data frame
RelatedEmoAvg<-data.frame(t(RelatedEmoAvg)) #transpose the df
rownames(RelatedEmoAvg)<-RelatedVids$rel_video_id #set row names
#---

#---Calculate absolute difference---
Baseavg<-data.frame(t(Baseavg)) #transpose the base avg df
AbsoluteEmo<-data.frame() #initialize ABS difference data frame
seq<-1:length(RelatedEmo) 
for (n in seq){ #calculate difference of base and related video emotions
  AbsoluteEmoTemp<-abs(Baseavg[1,]-RelatedEmoAvg[n,])
  AbsoluteEmo<-rbind(AbsoluteEmo,AbsoluteEmoTemp)
}
rm(n,seq,AbsoluteEmoTemp) 
rownames(AbsoluteEmo)<-RelatedVids$rel_video_id #set row names
#---


#---Rank abs difference---
AbsoluteEmo<-data.frame(t(AbsoluteEmo)) #transpose data frame
AbsoluteEmo<-apply(AbsoluteEmo,2,rank) #rank abs diff for each emotion by video
#---  

#---transpose df----
AbsoluteEmo<-t(AbsoluteEmo) #transpose data frame
#---

#---Rank Ranks for emotions---
AbsoluteEmo<-apply(AbsoluteEmo,2,rank) #rank columns 
AbsoluteEmo<-data.frame(AbsoluteEmo) #convert back to df
#---

#---Add row sums---
AbsoluteEmo$sum<-rowSums(AbsoluteEmo) #add sum of rows
#---

#---Add Titles and links---
AbsoluteEmo$title<-RelatedVids$title #add titles to emo df
AbsoluteEmo$link<-paste("https://www.youtube.com/watch?v=",
                        RelatedVids$rel_video_id, sep="") #build links to videos
#---

#---Compile Ranked list---
RankedList<-data.frame(rank=rank(AbsoluteEmo$sum, ties.method ="first"),
                       title=AbsoluteEmo$title,link=AbsoluteEmo$link) #list of videos and rank by emotional fit
#---

#---Find top rank and plot it with base---
TopRank<-which.min(RankedList$rank)#top ranked video index value
ClosestMatch<-RelatedEmoAvg[which.min(RankedList$rank),]#avg values from closest match
ClosestMatch<-data.frame(t(ClosestMatch)) #transpose df
colnames(ClosestMatch)<-"1" #set column name to set value for graphing
Recc<-Avg+geom_point(data=ClosestMatch,aes(shape=18, size=5, y=ClosestMatch$'1', 
                                           x=colnames(Baseavg)))+scale_shape_identity()+ggtitle(paste(BaseTitle,"-",
                                                                                                      baseVid$videoId[1]),paste("Reccomended Video: ",
                                                                                                                                RelatedVids$title[TopRank]," - ",
                                                                                                                                RelatedVids$rel_video_id[TopRank]))
#add to ggplot to show how close best match video is
Recc
#---

#---add plots of all recommendations--
RelatedEmoAvgTrans<-data.frame(t(RelatedEmoAvg)) # reshape df for graphing
seq<-c(1:length(RelatedVids$rel_video_id)) #set number of iterations
for (n in seq){ #add data points for each video to the graph to show spread.
  Recc<-Recc+geom_point(data=RelatedEmoAvgTrans,aes_(y=RelatedEmoAvgTrans[,n], alpha=0.2))
}
rm(n,seq) 
Recc #display final graph
#---

#---Write ranked list to CSV--
write.csv(RankedList,"RankedList.csv", row.names = FALSE) #output ranked list to csv
#---

#---Display ranked list--
RankedList[order(RankedList$rank),]
#---

#---Calculate rank changes---
RankedList$ind<-c(1:10)
RankDelta<-sum(abs(RankedList$ind-RankedList$rank))
RankDelta
#---
