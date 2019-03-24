Sys.setlocale("LC_ALL", "C")
############## Problem 1 - Bags of Words ########################

wiki <- read.csv('wiki.csv', stringsAsFactors=F)
str(wiki)
wiki$Vandal <- as.factor(wiki$Vandal)
table(wiki$Vandal)

#pre-processing
library(tm)
corpusAdded <- Corpus(VectorSource(wiki$Added))
strwrap(corpusAdded[1])

corpusAdded <- tm_map(corpusAdded, removeWords, stopwords('english'))
corpusAdded <- tm_map(corpusAdded, stemDocument)

#matrix
dtmAdded <- DocumentTermMatrix(corpusAdded)
dtmAdded
length(stopwords('english'))

sparseAdded <- removeSparseTerms(dtmAdded,0.997)
sparseAdded

wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) <- make.names(colnames(wordsAdded))
colnames(wordsAdded) <- paste('A', colnames(wordsAdded))

# repeating from the pre-processing for REMOVED words
corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords('english'))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)

dtmRemoved <- DocumentTermMatrix(corpusRemoved)
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)

wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) <- make.names(colnames(wordsRemoved))
colnames(wordsRemoved) <- paste('R', colnames(wordsRemoved))

#combining the data frames
wikiWords <- cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal

#building model
library(caTools)
set.seed(123)
sample <- sample.split(wikiWords$Vandal, SplitRatio = .7)
train <- subset(wikiWords, sample==T)
test <- subset(wikiWords, sample==F)

#baseline
table(test$Vandal)
618/nrow(test)

#CART
library(rpart)
library(rpart.plot)
wikiCART <- rpart(Vandal ~ ., data=train, method='class')
prp(wikiCART)

pred <- predict(wikiCART, newdata=test)
pred[1:10,]
predicted <- pred[, 2]

table(test$Vandal, predicted>0.5)
(614+19)/nrow(test)

pred5 <- predict(wikiCART, type='class')
table(train$Vandal, pred5)
(1443+46)/nrow(train)
################################ Problem 2 - Problem-specific Knowledge #######################
wikiWords2 <- wikiWords

#Make a new column in wikiWords2 that is 1 if "http" was in Added:
wikiWords2$http <- ifelse(grepl('http', wiki$Added, fixed=T), 1, 0)
table(wikiWords2$http)

wikiTrain2 <- subset(wikiWords2, sample==T)
wikiTest2 <- subset(wikiWords2, sample==F)

wikiCART2 <- rpart(Vandal ~., data=wikiTrain2, method='class')
prp(wikiCART2)
pred2 <- predict(wikiCART2, newdata=wikiTest2, type='class')
table(test$Vandal, pred2)
(605+64)/nrow(test)

# 2.3 
wikiWords2$NumWordsAdded <- rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved <- rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

wikiTrain3 <- subset(wikiWords2, sample==T)
wikiTest3 <- subset(wikiWords2, sample==F)
wikiCART3 <- rpart(Vandal ~ . , data=wikiTrain3, method='class')
prp(wikiCART3)
pred3 <- predict(wikiCART3, newdata=wikiTest3, type='class')
table(wikiTest3$Vandal, pred3)
(514+248)/nrow(wikiTest3)

########################## Problem 3.1 - Using Non-Textual Data ##############
wikiWords3 <- wikiWords2
wikiWords3$Minor <- wiki$Minor
wikiWords3$loggedin <- wiki$Loggedin
train4 <- subset(wikiWords3, sample==T)
test4 <- subset(wikiWords3, sample==F)
wikicart4 <- rpart(Vandal ~ ., data=train4, method='class')
prp(wikicart4)
pred4 <- predict(wikicart4, newdata=test4, type='class')
table(test4$Vandal, pred4)
(595+241)/nrow(test4)
