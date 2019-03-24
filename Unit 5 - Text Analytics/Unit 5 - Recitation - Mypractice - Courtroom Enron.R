#looking at data
emails <- read.csv('energy_bids.csv', stringsAsFactors = F)
str(emails)

Nemails$email[1]
strwrap(emails$email[1]) #for text wrapping
emails$responsive[1]

strwrap(emails$email[2])
emails$responsive[2]

table(emails$responsive)

################### Video 3: Pre-Processing #################
library(tm)
corpus <- Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords('english')) #removing stop words
corpus <- tm_map(corpus, stemDocument)
strwrap(corpus[[1]])

################# Video 4: Bag of Words ##################
dtm <- DocumentTermMatrix(corpus)
dtm #here we have 21877 terms which appear atleast once. so we remove sparse terms
inspect(dtm[850:855, 21870:21877])
findFreqTerms(dtm, lowfreq = 200) #words that appear at least 200 terms

dtm <- removeSparseTerms(dtm, 0.97) #removing terms that doesn't appear in atleast 3% of the documents
dtm

labeledTerms <- as.data.frame(as.matrix(dtm))
colnames(labeledTerms) <- make.names(colnames(labeledTerms)) #converting variable names to R standard such that no variables starts with numbers/#/...
#run make.names everytime when doing text analytics
labeledTerms$responsive <- emails$responsive
str(labeledTerms)

################## Video 5: Building Models ###############
library(caTools)
set.seed(144)
split <- sample.split(labeledTerms$responsive, SplitRatio = .7)
train <- subset(labeledTerms, split==T)
test <- subset(labeledTerms, split==F)

library(rpart)
library(rpart.plot)
emailCART <- rpart(responsive ~., data=train, method='class')
prp(emailCART)

################## Video 6: Evaluating the Model #################
pred <- predict(emailCART, newdata = test)
pred[1:10,]
pred.prob <- pred[, 2] # we are interested in responsive being 1 so 2 columns

table(test$responsive, pred.prob>0.5)
(195+25)/(nrow(test)) #accuracy

#baseline
table(test$responsive)
215/nrow(test)

################## Video 7: The ROC Curve ###############
library(ROCR)
predROCR <- prediction(pred.prob, test$responsive)
perfROCR <- performance(predROCR, 'tpr', 'fpr')
plot(perfROCR, colorize=T)

performance(predROCR, 'auc')@y.values
