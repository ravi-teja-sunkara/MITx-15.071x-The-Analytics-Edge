Sys.setlocale("LC_ALL", "C")

################################################################################
#                           Problem 1.1 - Preparing the Data                   #
################################################################################
tweets <- read.csv('tweets.csv', stringsAsFactors=F)
str(tweets)

#pre-processing
library(tm)
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

#dtm
dtm <- DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(dtm))
str(allTweets)

################################################################################
#                      Problem 2.1 - Building a Word Cloud                     #
################################################################################
install.packages('wordcloud')
library(wordcloud)
?wordcloud
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2,0.25))

################################################################################
#                  Problem 3.1 - Size and Color                                #
################################################################################
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c(stopwords('english'),'apple'))

#dtm
dtm <- DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(dtm))

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2,0.25))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2,0.2), random.color=T)

################################################################################
#                  Problem 4.2 - Selecting a Color Palette                     #
################################################################################
brewer.pal()
