Sys.setlocale("LC_ALL", "C")

####################### Problem 1 - Predicting B or not B ##############
letters <- read.csv('letters_ABPR.csv')
letters$isB <- as.factor(letters$letter=='B')

#splitting
set.seed(1000)
sample = sample.split(letters$isB, SplitRatio = .5)
train = subset(letters, sample==T)
test = subset(letters, sample==F)

#baseline model
table(letters$isB) #most frequent is NOT B
2350/nrow(letters) #accuracry of the baseline model

#building a classification tree
library('rpart')
library('rpart.plot')
cartb <- rpart(isB ~ .-letter, data=train, method = 'class')
predb <- predict(cartb, newdata = test, type = 'class')
table(test$isB, predb)
(1118+340)/nrow(test)

#Random Forest Model
library('randomForest')
set.seed(1000)
forestb <- randomForest(isB ~ .-letter, data=train)
predforestb <- predict(forestb, newdata = test, type='class')
table(test$isB, predforestb)
(1165+374)/nrow(test) #accuracy

######################## Problem 2 - Predicting the letters A, B, P, R ######################
letters$letter <- as.factor(letters$letter)
str(letters)
letters$isB <- NULL

#splitting
set.seed(2000)
sample2 <- sample.split(letters$letter, SplitRatio = .5)
train2 <- subset(letters, sample2==T)
test2 <- subset(letters, sample2==F)

# Baseline model
#In a multiclass classification problem, a simple baseline model is to predict the 
#most frequent class of all of the options.
table(test2$letter)
401/nrow(test2) #accuracy

# Classification Trees
cart2 <- rpart(letter ~ ., data=train2, method='class')
pred2 <- predict(cart2, newdata = test2, type='class')
table(test2$letter, pred2)
(348+318+363+340)/nrow(test)

# Random Forest Model
forest2 <- randomForest(letter ~ ., data=train2)
predforest2 <- predict(forest2, newdata = test2, type = 'class')
table(test2$letter, predforest2)
(390+380+393+369)/nrow(test2)
