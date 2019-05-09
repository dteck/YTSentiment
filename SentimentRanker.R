##YouTube Recommendation Sentiment Analysis and Ranking
##Author: Mark Richards
##GitHub: https://github.com/dteck/YTSentiment

#--install and load required packages--
if (!require(tuber)) install.packages('tuber')
if (!require(syuzhet)) install.packages('syuzhet')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(tidyverse)) install.packages('tidyverse')
library(tuber)
library(syuzhet)
library(ggplot2)
library(tidyverse)
#------


#--Report code does not require an API key--
APIKey<-file_test("-f","APIKey.rds") #test if apikey exists
if (APIKey == FALSE) { #run code block if apikey does not exist
  print("No youtube API Key")
  print("See https://developers.google.com/youtube/v3/getting-started")
  print("expects key as RDS data.frame in form APIKey<-data.frame(app_id='Your App ID Here', app_secret = 'Your App Secret Here')")
}else { #run code block if api key exists
  APIKey<-readRDS("APIKey.rds") #read API key
  yt_oauth(app_id=APIKey$app_id, app_secret = APIKey$app_secret, token = "") #load key to memory
}
#------


#--Enter the link to the youtube video you want reccomendations for--
Baselink<-"https://www.youtube.com/watch?v=HsxBw6ls7Z0" #link to a youtube video
#------


#--Extract the video ID, and pull the recomended videos and the base video comments--
Baselink<-str_match(Baselink,"[^=]+$") #regex to extract video ID
#RelatedVids<- get_related_videos(video_id =Baselink, max_results = 11) #Code with APIKey
RelatedVids<- readRDS("RelatedVids.rds") #Get recomended
#baseVid<-get_all_comments(video_id = Baselink) #Code with APIKey
baseVid<-readRDS("baseVid.rds") #get the comments from that video
#BaseDetail<-get_video_details(video_id = Baselink)#Code with APIKey
BaseDetail<-readRDS("BaseDetail.rds")#pull details of the base video
BaseTitle<-BaseDetail$items[[1]]$snippet$title #extract the title of the base video
#------


#-- build and plot base video emotional averages--
#sanitize the text by converting to utf-8
baseVid$sanitary<-iconv(baseVid$textOriginal, to="UTF-8")
#get emotional levels from each comment
BaseEmo<-get_nrc_sentiment(baseVid$sanitary)
#create averages of sentiment and emotion for base video
Baseavg<-apply(BaseEmo, 2, mean)
#convert averages to data frame for plotting
Baseavg<-data.frame(name=Baseavg)
#plot the emotional sentiment averages
Avg<-ggplot(Baseavg, aes_(x=row.names(Baseavg), y=Baseavg$name, fill=row.names(Baseavg)))+geom_bar(stat="identity")+theme(legend.position="none",axis.title.x=element_blank(),axis.title.y=element_blank())
Avg+ggtitle(BaseTitle,paste("Video ID: ",baseVid$videoId[1])) #avgplot1
#------

#--get comments for recomended videos--
# seq<-1:length(RelatedVids$rel_video_id) #code for API Key
# RelatedComm<-list() #initalize a list for the related video comments
# for (n in seq){ #get comments for the related video, sanitize the comments by converting to UTF-8 format, Also convert like counts to numeric
#   RelatedComm[[n]]<-get_all_comments(video_id = as.character(RelatedVids$rel_video_id[n]))
#   RelatedComm[[n]]$sanitary<-iconv(RelatedComm[[n]]$textOriginal, to="UTF-8")
#   RelatedComm[[n]]$likeCount<-as.numeric(RelatedComm[[n]]$likeCount)
# }
# rm(n,seq) #remove n and sequence used to itterate over list
RelatedComm<-readRDS("RelatedComm.rds")#loads comments from related videos
#------

#--determine emotional expression for reccomended videos--
seq<-1:length(RelatedVids$rel_video_id) #set iteration length
RelatedEmo<-list()
for (n in seq){ #itterate over all comments and get emotional sentiment
  RelatedEmo[[n]]<-get_nrc_sentiment(RelatedComm[[n]]$sanitary)
}
rm(n,seq) 
#------

#--calculate emotional averages for recommended videos--
seq<-1:length(RelatedEmo) #set iteration length
RelatedEmoAvg<-list() 
for (n in seq){ #calulate averages of emotional senitment
  RelatedEmoAvg[[n]]<-apply(RelatedEmo[[n]][1:10], 2, mean)
}
rm(n,seq)
#------

#--get the number of comments analyzed--
CommentCount<-0
seq<-1:length(RelatedComm)
for (n in seq){ ##count the number of comments
  CommentCount<-CommentCount+length(RelatedComm[[n]]$authorDisplayName)
}
rm(n,seq)
#------

#--reshape data--
RelatedEmoAvg<-data.frame(RelatedEmoAvg) #reshape the dataframe
RelatedEmoAvg<-data.frame(t(RelatedEmoAvg))
rownames(RelatedEmoAvg)<-RelatedVids$rel_video_id #set row names
Baseavg<-data.frame(t(Baseavg)) #transpose the baseavg df
#------

#--calculate absolute difference of related video averages from base video--
AbsoluteEmo<-data.frame() #initialize ABS difference dataframe
seq<-1:length(RelatedEmo) 
for (n in seq){ #calculate difference between base emotions and related video emotions
  AbsoluteEmoTemp<-abs(Baseavg[1,]-RelatedEmoAvg[n,])
  AbsoluteEmo<-rbind(AbsoluteEmo,AbsoluteEmoTemp)
}
rm(n,seq,AbsoluteEmoTemp) 
rownames(AbsoluteEmo)<-RelatedVids$rel_video_id #set row names
#------

#--reshape and rank data, then build rankings with links--
AbsoluteEmo<-data.frame(t(AbsoluteEmo)) #transpose dataframe
AbsoluteEmo<-apply(AbsoluteEmo,2,rank) #get rank of absolute difference of each emotion for a video
AbsoluteEmo<-t(AbsoluteEmo) #transose dataframe
AbsoluteEmo<-apply(AbsoluteEmo,2,rank) #rank columns to get rank of each emotion across all video 
AbsoluteEmo<-data.frame(AbsoluteEmo) #convert back to dataframe
AbsoluteEmo$sum<-rowSums(AbsoluteEmo) #add sum of rows
AbsoluteEmo$title<-RelatedVids$title #add titles to emo df
AbsoluteEmo$link<-paste("https://www.youtube.com/watch?v=",RelatedVids$rel_video_id, sep="") #build links to videos
RankedList<-data.frame(rank=rank(AbsoluteEmo$sum, ties.method ="first"),title=AbsoluteEmo$title,link=AbsoluteEmo$link) #generate a list of recommended videos and rank of best emotional fit
#------

#--plot top ranked video against base video--
TopRank<-which.min(RankedList$rank)# get the topranked video index value
ClosestMatch<-RelatedEmoAvg[which.min(RankedList$rank),] #pull emotional avg values from closest match
ClosestMatch<-data.frame(t(ClosestMatch)) #transpose df
colnames(ClosestMatch)<-"1" #set column name to set value for graphing
Recc<-Avg+geom_point(data=ClosestMatch,aes(shape=18, size=5, y=ClosestMatch$'1', x=colnames(Baseavg)))+scale_shape_identity()+ggtitle(paste(BaseTitle,"-",baseVid$videoId[1]),paste("Reccomended Video: ",RelatedVids$title[TopRank]," - ",RelatedVids$rel_video_id[TopRank]))#add to ggplot to show how close best match video is
Recc
#------

#--plot all videos against base video--
RelatedEmoAvgTrans<-data.frame(t(RelatedEmoAvg)) # reshape df for graphing
seq<-c(1:length(RelatedVids$rel_video_id)) #set number of itterations
for (n in seq){ #add data points for each video to the graph to show spread.
  Recc<-Recc+geom_point(data=RelatedEmoAvgTrans,aes_(y=RelatedEmoAvgTrans[,n]), alpha=0.2)
}
rm(n,seq) 
Recc #display final graph
#------

#--export ranked list as a CSV--
write.csv(RankedList,"RankedList.csv", row.names = FALSE) #output ranked list to csv
RankedList #view ranked list in R studio
#------