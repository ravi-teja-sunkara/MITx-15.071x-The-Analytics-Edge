Sys.setlocale("LC_ALL", "C")

########################## Problem 1 - A Logistic Regression Model #####################
census <- read.csv('census.csv')
str(census)

#splitting
set.seed(2000)
split1 <- sample.split(census$over50k, SplitRatio = .6)
train1 <- subset(census, split1==T)
test1 <- subset(census, split1==F)

#logistic model
logmod <- glm(over50k ~ ., data=train1, family = binomial)
summary(logmod)
pred1 <- predict(logmod, newdata = test1, type='response')
table(test1$over50k, pred1 >= 0.5)
(9051+1888)/nrow(test1)

#baseline model
table(train1$over50k)
9713/(nrow(test1))

#AUC
library('ROCR')
rocrpred1 <- prediction(pred1, test1$over50k)
as.numeric(performance(rocrpred1, 'auc')@y.values)

########################### Problem 2 - A CART Model ######################
library('rpart')
library('rpart.plot')
cart1 <- rpart(over50k ~ ., data=train1, method='class')
prp(cart1)

pred2 <- predict(cart1, newdata = test1, type = 'class')
table(test1$over50k, pred2)
(9243+1596)/nrow(test1)

#ROC curve
#Remember that you can do this by removing the type="class" argument when making predictions, 
#and taking the second column of the resulting object.
library(ROCR)
predROC <- predict(cart1, newdata = test1)
predROC2 <- prediction(predROC[,2], test1$over50k)
perform <- performance(predROC2, 'tpr','fpr')
plot(perform)

#AUC
as.numeric(performance(predROC2,'auc')@y.values)


######################## Problem 3 - A Random Forest Model ######################
library(randomForest)

#creating a smaller subset for RF model from train1 data
set.seed(1)
train1small <- train1[sample(nrow(train1),2000),]
set.seed(1)
forest1 <- randomForest(train1small$over50k ~ ., data=train1small)
pred3 <- predict(forest1, newdata = test1, type = 'class')
table(test1$over50k, pred3)
(9586+1093)/nrow(test1)

#One metric that we can look at is the number of times, aggregated over all of the trees 
#in the random forest model, that a certain variable is selected for a split.
vu <- varUsed(forest1, count = T)
vusorted <- sort(vu, decreasing = F, index.return=T)
dotchart(vusorted$x, names(forest1$forest$xlevels[vusorted$ix]))

#impurity
varImpPlot(forest1)

##################### Problem 4 - Selecting cp by Cross-Validation ####################
library(caret)
library(e1071)
set.seed(2)
folds <- trainControl(method = 'cv', number=10)
cartgrid <- expand.grid(.cp=seq(0.002,0.1,0.002))
cp <- train(over50k ~ ., data=train1, method='rpart', trControl=folds, tuneGrid=cartgrid)
cp

cart2 <- rpart(over50k ~ ., data=train1, method = 'class', cp=.002)
pred4 <- predict(cart2, newdata = test1, type = 'class')
table(test1$over50k, pred4)
(9178+1838)/nrow(test1)

prp(cart2)
