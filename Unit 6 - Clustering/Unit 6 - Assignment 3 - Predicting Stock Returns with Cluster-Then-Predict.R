Sys.setlocale("LC_ALL", "C")

###############################################################################
#                       Problem 1 - Exploring the Dataset                   #
##############################################################################
stocks <- read.csv('StocksCluster.csv')
str(stocks)
table(stocks$PositiveDec)
6324/nrow(stocks)

cor(stocks) #correlation
summary(stocks)

###############################################################################
#               Problem 2.1 - Initial Logistic Regression Model               #
###############################################################################
library(caTools)
set.seed(144)
spl <- sample.split(stocks$PositiveDec, SplitRatio=.7)
train <- subset(stocks, spl==T)
test <- subset(stocks, spl==F)

#logistic model - simple
log1 <- glm(PositiveDec ~ ., data=train, family='binomial')
predict1 <- predict(log1, type='response')
table(train$PositiveDec, predict1>0.5)
(990+3640)/nrow(train) #train set accuracy

predict2 <- predict(log1, newdata=test, type='response')
table(test$PositiveDec, predict2 > 0.5)
(417+1553)/nrow(test) #test set accuracy

#baseline
table(test$PositiveDec)
1897/nrow(test)

###############################################################################
#                          Problem 3.1 - Clustering Stocks                    #
##############################################################################
limitedtrain <- train
limitedtrain$PositiveDec <- NULL
limitedtest <- test
limitedtest$PositiveDec <- NULL

#pre-processing/Standardizing
library('caret')
preproc <- preProcess(limitedtrain)
normtrain <- predict(preproc, limitedtrain)
normtest <- predict(preproc, limitedtest)

summary(normtrain)
summary(normtest)

#k-means
set.seed(144)
km <- kmeans(normtrain, centers=3)
str(km)
table(km$cluster)

#clustering stocks
library('flexclust')
km.kcca <- as.kcca(km, normtrain)
clustertrain <- predict(km.kcca) #just another way to make clusters on training set
str(clustertrain)
clustertrain[1:10]
clustertest <- predict(km.kcca, newdata=normtest) #clusters on test set
table(clustertest)
table(clustertrain)

###############################################################################
#                    Problem 4 - Cluster-Specific Predictions               #
###############################################################################
train1 <- subset(train, km$cluster==1)
train2 <- subset(train, clustertrain==2) #same as above
train3 <- subset(train, clustertrain==3)
summary(train3)
summary(train2)
summary(train1)
test1 <- subset(test, clustertest==1)
test2 <- subset(test, clustertest==2)
test3 <- subset(test, clustertest==3)

mean(train1$PositiveDec)
mean(train2$PositiveDec)
mean(train3$PositiveDec)

logc1 <- glm(PositiveDec ~., data=train1, family='binomial')
logc2 <- glm(PositiveDec ~., data=train2, family='binomial')
logc3 <- glm(PositiveDec ~., data=train3, family='binomial')

testpredict1 <- predict(logc1, newdata=test1, type='response')
testpredict2 <- predict(logc2, newdata=test2, type='response')
testpredict3 <- predict(logc3, newdata=test3, type='response')

table(test1$PositiveDec, testpredict1 > 0.5)
(30+774)/nrow(test1)
table(test2$PositiveDec, testpredict2 > 0.5)
(388+757)/nrow(test2)
table(test3$PositiveDec, testpredict3 > 0.5)
(49+13)/nrow(test3)

#combinig all - to determine overall
allpredictions <- c(testpredict1, testpredict2, testpredict3)
alloutcomes <- c(test1$PositiveDec, test2$PositiveDec, test3$PositiveDec)
table(alloutcomes, allpredictions>0.5)
(467+1544)/3474
