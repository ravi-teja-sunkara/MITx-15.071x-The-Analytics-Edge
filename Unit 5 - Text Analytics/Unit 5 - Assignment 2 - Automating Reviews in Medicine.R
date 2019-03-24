Sys.setlocale("LC_ALL", "C")

#################### Problem 1 - Loading the Data #######################
trials <- read.csv('clinical_trial.csv', stringsAsFactors=F)
str(trials)
summary(trials)

max(nchar(trials$abstract)) #The nchar() function counts the number of characters in a piece of text.

#How many search results provided no abstract? 
#(HINT: A search result provided no abstract if the number of characters in the abstract field is zero.)
sum(as.numeric((nchar(trials$abstract)==0)))
sum(nchar(trials$abstract)==0)

#min characters in title
which.min(nchar(trials$title))
trials[1258,'title']

##########################################################################
#           Problem 2.1 - Preparing the Corpus                           #
##########################################################################

#pre-processing
library(tm)
corpustitle <- Corpus(VectorSource(trials$title))
corpusabstract <- Corpus(VectorSource(trials$abstract))
strwrap(corpustitle[1])

corpustitle <- tm_map(corpustitle, tolower)
corpusabstract <- tm_map(corpusabstract, tolower)

corpustitle <- tm_map(corpustitle, removePunctuation)
corpusabstract <- tm_map(corpusabstract, removePunctuation)

corpustitle <- tm_map(corpustitle, removeWords, stopwords('english'))
corpusabstract <- tm_map(corpusabstract, removeWords, stopwords('english'))

corpustitle <- tm_map(corpustitle, stemDocument)
corpusabstract <- tm_map(corpusabstract, stemDocument)

# to data frame
dtmtitle <- DocumentTermMatrix(corpustitle)
sparsetitle <- removeSparseTerms(dtmtitle, .95)
dtmtitle <- as.data.frame(as.matrix(sparsetitle))

dtmabstract <- DocumentTermMatrix(corpusabstract)
sparseabstract <- removeSparseTerms(dtmabstract, .95)
dtmabstract <- as.data.frame(as.matrix(sparseabstract))

colnames(dtmtitle) <- make.names(colnames(dtmtitle))
colnames(dtmabstract) <- make.names(colnames(dtmabstract))

dtmtitle
dtmabstract

#frequency of words
sort(colSums(dtmtitle))
sort(colSums(dtmabstract))

################################################################################
#                       Problem 3.1 - Building a model                         #
################################################################################
colnames(dtmtitle) <- paste0('T', colnames(dtmtitle))
colnames(dtmabstract) <- paste0('A', colnames(dtmabstract))

dtm <- cbind(dtmtitle, dtmabstract)
dtm$trial <- trials$trial

library(caTools)
set.seed(144)
sample <- sample.split(dtm$trial, SplitRatio=.7)
train <- subset(dtm, sample==T)
test <- subset(dtm, sample==F)

#baseline
table(train$trial)
730/nrow(train)

#CART model
library(rpart)
library(rpart.plot)

cart <- rpart(trial~., data=train, method='class')
prp(cart)

trainPred <- predict(cart)
trainPred <- trainPred[,2]
max(trainPred)

table(train$trial, trainPred>0.5)
(631+441)/nrow(train)

631/(631+99) #specificity - TN
441/(131+441) #sensitivity - TP

testPred <- predict(cart, newdata=test) #on test data set
testPred <- testPred[,2]
table(test$trial, testPred>0.5)
(261+162)/nrow(test)

#AUC
library(ROCR)
predrocr <- prediction(testPred, test$trial)
perfrocr <- performance(predrocr, 'tpr', 'fpr')
plot(perfrocr, colorize=T)

performance(predrocr,'auc')@y.values
