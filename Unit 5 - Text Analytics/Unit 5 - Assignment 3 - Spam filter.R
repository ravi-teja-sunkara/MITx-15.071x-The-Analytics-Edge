###############################################################################
#                        Problem 1.1 - Loading the Dataset                    #
###############################################################################
Sys.setlocale("LC_ALL", "C")
emails <- read.csv('emails.csv', stringsAsFactors=F)
str(emails)
table(emails$spam)

emails[1,1]

max(nchar(emails$text))
which.min(nchar(emails$text))

###############################################################################
#               Problem 2.1 - Preparing the Corpus                            #
###############################################################################
#pre-processing
library(tm)
corpus <- Corpus(VectorSource(emails$text))
strwrap(corpus[1])
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)

#data frame
dtm <- DocumentTermMatrix(corpus)
dtm
spdtm <- removeSparseTerms(dtm, .95)
spdtm
emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) <- make.names(colnames(emailsSparse))

#finding most occuring words in ham and spam datasets
which.max(colSums(emailsSparse))
sort(colSums(emailsSparse))

emailsSparse$spam <- emails$spam
ham <- subset(emailsSparse, emailsSparse$spam==0)
sort(colSums(ham))

spam <- subset(emailsSparse, emailsSparse$spam==1)
sort(colSums(spam))

################################################################################
#                          Problem 3.1 - Building machine learning models      #
################################################################################
emailsSparse$spam <- as.factor(emailsSparse$spam)

#splitting
library(caTools)
set.seed(123)
sample <- sample.split(emailsSparse$spam, SplitRatio=.7)
train <- subset(emailsSparse, sample==T)
test <- subset(emailsSparse, sample==F)

#logistic model
spamlog <- glm(spam ~., data=train, family='binomial')
predictlog <- predict(spamlog, newdata=test, type='response')
predictlogtrain[1:10 ]

table(test$spam, predictlog>0.5)
(1257+376)/nrow(test) #accuracy on test dataset

logtestAUC <- prediction(predictlog, test$spam)
performance(logtestAUC, 'auc')@y.values #auc for TEST dataset

predictlogtrain <- predict(spamlog, type='response') #on TRAINING set
length(predictlogtrain)
length(predictlogtrain[predictlogtrain<0.00001]) #How many of the training set predicted probabilities from spamLog are less than 0.00001?
length(predictlogtrain[predictlogtrain > 0.99999]) #how many  predicted probabilities from spamLog are more than 0.99999
length(predictlogtrain[predictlogtrain>=0.00001 & predictlogtrain<=0.99999]) # in between
summary(spamlog) #summary of log model

table(train$spam, predictlogtrain>0.5)
(3052+954)/nrow(train) #accuracy on training dataset

logtrainauc <- prediction(predictlogtrain, train$spam)
performance(logtrainauc, 'auc')@y.values # auc for training set

#CART model
library(rpart)
library(rpart.plot)
spamcart <- rpart(spam ~. , data=train, method='class')
prp(spamcart)

predictcart <- predict(spamcart, newdata=test)
predictcart[1:10,]
predictcart <- predictcart[,2] #choosing only second column

table(test$spam, predictcart>0.5)
(1228+386)/nrow(test) # accuracy on Test

carttestAUC <- prediction(predictcart, test$spam)
performance(carttestAUC, 'auc')@y.values #auc on Test

predictcarttrain <- predict(spamcart) #perdictions on training set
predictcarttrain <- predictcarttrain[,2]

table(train$spam, predictcarttrain>0.5)
(2885+894)/nrow(train) #accuracy on training

carttrainAUC <- prediction(predictcarttrain, train$spam)
performance(carttrainAUC, 'auc')@y.values #auc on training

#RandomForest model
library(randomForest)
set.seed(123)
spamRF <- randomForest(spam ~., data=train)
predictRF <- predict(spamRF, newdata=test, type='prob') #type='class' for classification but here we need probabilities to calculate AUC values
predictRF[1:10, ]
predictRF <- predictRF[ ,2] #choosing 2nd col corresponding to being SPAM

table(test$spam, predictRF>0.5)
(1290+387)/nrow(test) #accuracy on Test

RFtestAUC <- prediction(predictRF, test$spam)
performance(RFtestAUC, 'auc')@y.values #AUC on Test

predictRFtrain <- predict(spamRF, type='prob') #for TRAINING dataset 
predictRFtrain <- predictRFtrain[,2]

table(train$spam, predictRFtrain>0.5)
(3013+912)/nrow(train) #accuracy on training

RFtrainAUC <- prediction(predictRFtrain, train$spam)
performance(RFtrainAUC, 'auc')@y.values
