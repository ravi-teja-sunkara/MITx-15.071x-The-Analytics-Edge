tweets <- read.csv('tweets.csv', stringsAsFactors = F)
str(tweets)

tweets$negative <- as.factor(tweets$Avg <= -1)
table(tweets$negative)

install.packages('tm')
library(tm)
install.packages('SnowballC')
library(SnowballC)

#creating corpus
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]$content

#lower case
corpus <- tm_map(corpus, tolower)
corpus[[1]]$content

#remove punctiation
corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]$content

#removing stop words
stopwords('english')[1:10]
corpus <- tm_map(corpus, removeWords, c('apple', stopwords('english')))
corpus[[1]]$content

#stemming
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content

#frequencies
frequencies <- DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005, 505:515])

findFreqTerms(frequencies, lowfreq = 20)

sparse <- removeSparseTerms(frequencies, 0.995)
sparse

tweetsparse <- as.data.frame(as.matrix(sparse))
colnames(tweetsparse) <- make.names(colnames(tweetsparse))
tweetsparse$negative <- tweets$negative

#splitting
library(caTools)
set.seed(123)
split <- sample.split(tweetsparse$negative, SplitRatio = .7)
trainsparse <- subset(tweetsparse, split==T)
testsparse <- subset(tweetsparse, split==F)

#Building CART model
library(rpart)
library(rpart.plot)

tweetcart <- rpart(negative ~ ., data=trainsparse, method='class')
prp(tweetcart)

predictcart <- predict(tweetcart, newdata = testsparse, type = 'class')
table(testsparse$negative, predictcart)
(294+18)/(nrow(testsparse))

#baseline model
table(testsparse$negative)
300/355

#random forest model
library(randomForest)
set.seed(123)
tweetrf <- randomForest(negative ~., data=trainsparse)
predictRF <- predict(tweetrf, newdata = testsparse)
table(testsparse$negative, predictRF)
(293+21)/nrow(testsparse)

#logistic model
tweetlog <- glm(negative ~., data=trainsparse, family=binomial)
predictlog <- predict(tweetlog, newdata=testsparse, type='response')
table(testsparse$negative, predictlog>0.5)
(251+36)/nrow(testsparse)

predictTrainlog <- predict(tweetlog, type = 'response')
table(trainsparse$negative, predictTrainlog>0.5)
(677+107)/nrow(trainsparse)
