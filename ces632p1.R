############################################################################################################
############################################################################################################

#PROJECT 1 Code

############################################################################################################
############################################################################################################

setwd("/home/tina/Desktop/Data Mining")
#load packages for general data cleaning
library(tidyverse)
library(dplyr)
library(stringr)
library(tidytext)
library(tm)

#read in historical data
election=read.csv("election.csv", header = TRUE, stringsAsFactors=FALSE)
is.data.frame(election)
ncol(election)
nrow(election)
#read in new data
etweets=read.csv("etweets.csv", header = TRUE, stringsAsFactors=FALSE)
is.data.frame(etweets)
ncol(etweets)
nrow(etweets)

#dates and times data was created
election$created_at[1]
election$created_at[nrow(election)]
etweets$created_at[1]
etweets$created_at[nrow(etweets)]

#*****************************************************************************************#
#table of NA counts
na_count=sapply(election, function(y) sum(is.na(y)))
na_count=data.frame(na_count)
na_count
#remove columns where the entire column is NA
cl.etweets=etweets[,colSums(is.na(etweets))!=nrow(etweets)]
#remove columns where there is at least 1 NA
cl.election=cl.election[,colSums(is.na(cl.election))<1]

ncol(cl.election)
cl.etweets=cl.etweets[,colSums(is.na(cl.etweets))<1]
ncol(cl.etweets)

#output a vector of the number of different levels or categories in each feature
categories=function(D=data, c=column) {
  cat=length(unique(D[,c]))
  print(cat)
}
D=cl.election
n=length(cl.election)
p <- 1
for (i in 1:n) {
  p[i] <- categories(D,i)
}
#find which columns have only one entry type
which(p==1)
#remove columns where the entire column is one level
cl.election=cl.election[,which(p>1)]
#for the new data
D=cl.etweets
n=length(cl.etweets)
p <- 1
for (i in 1:n) {
  p[i] <- categories(D,i)
}
#find which columns have only one entry type
which(p==1)
#remove columns where the entire column is one level
cl.etweets=cl.etweets[,which(p>1)]
#*****************************************************************************************#

#now we go through the twitter data dictionary to further determine what features to keep
#twitter objects: https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/overview/intro-to-tweet-json

#*****************************************************************************************#
#remove columns with user information except user description and screen name
cl.election1=select(cl.election, -starts_with("user"))
user.desc=cl.election$user.description
user.sn=cl.election$user.screen_name  
cl.election1=cbind(cl.election1, user.desc, user.sn)
cl.etweets1=select(cl.etweets, -starts_with("user"))
euser.desc=cl.etweets$user.description
euser.sn=etweets$user.screen_name
cl.etweets1=cbind(cl.etweets1,euser.desc, euser.sn)
#remove columns with entities information except hashtag text
cl.election1=select(cl.election1, -starts_with("entities"))
hashtag.text=cl.election$entities.hashtags.hashtags.text
cl.election1=cbind(cl.election1,hashtag.text)
cl.etweets1=select(cl.etweets1, -starts_with("entities"))
ehashtag.text=cl.etweets$entities.hashtags.hashtags.text
cl.etweets1=cbind(cl.etweets1, ehashtag.text)
#remove extended entities - it is non text data like images
cl.election1=select(cl.election1, -starts_with("extended"))
cl.etweets1=select(cl.etweets1, -starts_with("extended"))
#remove anything concerning media
cl.election1=select(cl.election1, -contains("media"))
cl.etweets1=select(cl.etweets1, -contains("media"))
#remove all retweet information except text and user.screen_name
#remove all quoted information except text and user screen name
#extract dates
date=str_extract(cl.election$created_at, "[A-Z]{1}[a-z]{2} [A-Z]{1}[a-z]{2} [0-9]{2}")
#*****************************************************************************************#

#remove non english rows
cl.election<-election[(election$lang=="en"),]
cl.etweets<-etweets[(etweets$lang=="en"),]

#extract individual times
library(chron)
cl.election$time=str_extract(cl.election$created_at, "[0-9]{2}:[0-9]{2}:[0-9]{2}")
cl.election$time=chron(times=cl.election$time)
cl.etweets$time=str_extract(cl.etweets$created_at, "[0-9]{2}:[0-9]{2}:[0-9]{2}")
cl.etweets$time=chron(times=cl.etweets$time)

#remove all urls
cl.election$text <- gsub("http.*","",cl.election$text)
cl.election$text <- gsub("https.*","",cl.election$text)
cl.etweets$text <- gsub("http.*","",cl.etweets$text)
cl.etweets$text <- gsub("https.*","",cl.etweets$text)

#remove emojis and special characters
cl.election$text =sapply(cl.election$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
cl.etweets$text = sapply(cl.etweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

cl.election$retweeted_status.user.name =sapply(cl.election$retweeted_status.user.name,function(row) iconv(row, "latin1", "ASCII", sub=""))
cl.etweets$retweeted_status.user.name= sapply(cl.etweets$retweeted_status.user.name,function(row) iconv(row, "latin1", "ASCII", sub=""))

cl.election$quoted_status.user.screen_name=sapply(cl.election$quoted_status.user.screen_name,function(row) iconv(row, "latin1", "ASCII", sub=""))
cl.etweets$quoted_status.user.screen_name= sapply(cl.etweets$quoted_status.user.screen_name,function(row) iconv(row, "latin1", "ASCII", sub=""))

#remove all mentions
cl.election$text <- gsub("@[[:alpha:]]*","",cl.election$text)
cl.etweets$text <- gsub("@[[:alpha:]]*","",cl.etweets$text)

#remove all RT from text
cl.election$text <- gsub("RT","",cl.election$text)
cl.etweets$text <- gsub("RT","",cl.etweets$text)

#remove hashtag text from text
cl.election$text=gsub("#[A-Za-z0-9]+|@[A-Za-z0-9]+|\\w+(?:\\.\\w+)*/\\S+", "", cl.election$text)
cl.etweets$text=gsub("#[A-Za-z0-9]+|@[A-Za-z0-9]+|\\w+(?:\\.\\w+)*/\\S+", "", cl.etweets$text)

#remove new line \n
cl.election$text=gsub("\n", "", cl.election$text)
cl.etweets$text=gsub("\n", "", cl.etweets$text)

#lowecase all text
cl.election$text=tolower(cl.election$text)
cl.etweets$text=tolower(cl.etweets$text)

cl.election$retweeted_status.user.name=tolower(cl.election$retweeted_status.user.name)
cl.etweets$retweeted_status.user.name=tolower(cl.etweets$retweeted_status.user.name)

cl.election$quoted_status.user.screen_name=tolower(cl.election$quoted_status.user.screen_name)
cl.etweets$quoted_status.user.screen_name=tolower(cl.etweets$quoted_status.user.screen_name)


cl.election$entities.hashtags.hashtags.text=tolower(cl.election$entities.hashtags.hashtags.text)
cl.etweets$entities.hashtags.hashtags.text=tolower(cl.etweets$entities.hashtags.hashtags.text)

#remove punctuation
cl.election$text=removePunctuation(cl.election$text)
cl.etweets$text=removePunctuation(cl.etweets$text)

#################################################################################################################
#at this point it's just easier to make a new data frame with the columns we want to keep
#so we can skip things between the *
new.election=tibble(line=1:nrow(cl.election), 
                    time.created= cl.election$time, 
                    rt.user= cl.election$retweeted_status.user.screen_name,  
                    qu.user=cl.election$quoted_status.user.screen_name,
                    text=cl.election$text, 
                    hashtag=cl.election$entities.hashtags.hashtags.text)
str(new.election)
new.etweets=tibble(line=1:nrow(cl.etweets), 
                   time.created= cl.etweets$time, 
                   rt.user=cl.etweets$retweeted_status.user.screen_name, 
                   qu.user=cl.etweets$quoted_status.user.screen_name,
                   text=cl.etweets$text, 
                   hashtag=cl.etweets$entities.hashtags.hashtags.text)
str(new.etweets)
#################################################################################################################
#constant to coerce time to numeric is 24
a=min(as.numeric(new.etweets$time.created))
b=max(as.numeric(new.etweets$time.created))
histhist=hist(as.numeric(new.election$time.created), 
               xlim = c(a,b),
               ylim = c(0,4000),
               breaks = 22 , 
               xaxt = "n",
              plot = FALSE
)

newhist=hist(as.numeric(new.etweets$time.created), 
              xlim = c(a,b),
              ylim = c(0,4000),
              breaks = 22 , #approximately every 10 min
              xaxt = "n",
             plot = FALSE
)

plot(histhist, 
     col  = adjustcolor('red', alpha=0.3),
     main="Histogram of Tweet frequency over time",
     xlab = "Time in UTC", 
     ylab = "Tweet Frequency", 
     xlim = c(a,b),
     ylim = c(0,4000),
     xaxt = "n")
plot(newhist, 
     col  = adjustcolor('blue', alpha=0.3), 
     xlim = c(a,b),
     ylim = c(0,4000),
     xaxt = "n",
     add=TRUE)
axis(side=1, at=c(a,a+((b-a)/4),a+((b-a)/2),a+(3*(b-a)/4), b), labels=c("15:15","16:08","17:01","17:50","18:47"))
legend(.7,4000,legend=c("historical data","new data"),
       col=c(adjustcolor('red', alpha=0.3),adjustcolor('blue', alpha=0.3)),
       lty=1, lwd=10,cex=1)



#load packages for visualization
require(ggplot2)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#combine hashtag and tweet text
#break the text into individual tokens tweet this creates a new data frame
new.election.text=tibble(line=1:(2*nrow(new.election)), text=c(new.election$text, new.election$hashtag))
str(new.election.text)
new.election.text <- new.election.text %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text)
#remove stop words
data("stop_words")
new.election.text <- new.election.text %>%
  anti_join(stop_words)

# plot the top 15 words
new.election.text %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets and hashtags")

#break the text into individual tokens tweet this creates a new data frame
new.etweets.text=tibble(line=1:(2*nrow(new.etweets)), text=c(new.etweets$text, new.etweets$hashtag))
str(new.etweets.text)
new.etweets.text <- new.etweets.text %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text)
#remove stop words
data("stop_words")
new.etweets.text <- new.etweets.text %>%
  anti_join(stop_words)

# plot the top 15 words
new.etweets.text %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets and hashtags")

#for tweet text only
#break the text into individual tokens tweet this creates a new data frame
new.election.text <- new.election %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text)
#remove stop words
data("stop_words")
new.election.text <- new.election.text %>%
  anti_join(stop_words)

# plot the top 15 words
new.election.text %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in novel tweet text - historical data")

#break the text into individual tokens tweet this creates a new data frame
new.etweets.text <- new.etweets %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text)
#remove stop words
data("stop_words")
new.etweets.text <- new.etweets.text %>%
  anti_join(stop_words)

# plot the top 15 words
new.etweets.text %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in novel tweet text - new data")

#for hashtag text only
#break the text into individual tokens tweet this creates a new data frame
new.election.hashtag <- new.election %>%
  dplyr::select(hashtag) %>%
  unnest_tokens(word, hashtag)
#remove stop words
data("stop_words")
new.election.hashtag <- new.election.hashtag %>%
  anti_join(stop_words)

# plot the top 15 words
new.election.hashtag %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in hashtag text - historical data")
#break the text into individual tokens tweet this creates a new data frame
new.etweets.hashtag<- new.etweets %>%
  dplyr::select(hashtag) %>%
  unnest_tokens(word, hashtag)
#remove stop words
data("stop_words")
new.etweets.hashtag <- new.etweets.hashtag %>%
  anti_join(stop_words)

# plot the top 15 words
new.etweets.hashtag %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in hashtag text - new data")

#wordcloud
# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#combine retweeted user name and quoted user name
user=c(new.election$rt.user, new.election$qu.user)
length(user)
#delete blanks
user=user[-which(user == "")]
docs <- Corpus(VectorSource(user))
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#combine retweeted user name and quoted user name
user=c(new.etweets$rt.user, new.etweets$qu.user)
length(user)
#delete blanks
user=user[-which(user == "")]
docs <- Corpus(VectorSource(user))
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

