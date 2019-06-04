# YTSentiment
Sentiment analysis and ranking of YouTube reccomendations.
This script will read comments from youtube videos and the next 10 reccomendations then use basic sentiment analysis to build average emotional profiles of the comments. Then rank the reccomendations based on how closely they match the emotional proficle of the base video.

**Requires an API key**
See https://developers.google.com/youtube/v3/getting-started  

Report PDF: https://github.com/dteck/YTSentiment/raw/master/SentimentRanker.pdf
<br><br>
Basic sentiment analysis showing the emotional averages of the "base" video.
<img align="left" width="100%" src="https://github.com/dteck/YTSentiment/raw/master/base_sent.png">
<br><br>
Plot of the "base" video emotinal averages and a plot of the closest matchs averages.
<img align="left" width="100%" src="https://github.com/dteck/YTSentiment/raw/master/Match.png">
<br><br>
For comparison and range all of the reccomended videos plotted over the "base" video.
<img align="left" width="100%" src="https://github.com/dteck/YTSentiment/raw/master/all.png">
