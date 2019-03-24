Sys.setlocale("LC_ALL", "C")

stevens <- read.csv('stevens.csv')
str(stevens)

#splitting
require('caTools')
set.seed(3000)
sample <- sample.split(stevens$Reverse, SplitRatio = 0.7)

train <- subset(stevens, sample==T)
test <- subset(stevens, sample==F)

#Tress
install.packages(c('rpart','rpart.plot'))
library('rpart')
library('rpart.plot')

stevenstree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                     data=train, method='class', minbucket=25)
prp(stevenstree)

#on testing set
predictcart <- predict(stevenstree, newdata = test, type='class')
table(test$Reverse, predictcart)
(41+71)/(36+22+41+71)

#ROC curve
library('ROCR')
predictROC <- predict(stevenstree, newdata = test)

pred <- prediction(predictROC[,2], test$Reverse)
perform <- performance(pred, 'tpr','fpr')
plot(perform)

#AUC
as.numeric(performance(pred,'auc')@y.values)

# Trees with Different Bucket size
stevenstree2 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                     data=train, method='class', minbucket=4)
prp(stevenstree2)

stevenstree3 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                      data=train, method='class', minbucket=100)
prp(stevenstree3)

#Video 5: Random Forests
install.packages('randomForest')
library('randomForest')

stevensforest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data=train, nodesize=25, ntree=200)
train$Reverse <- as.factor(train$Reverse)
test$Reverse <- as.factor(test$Reverse)
stevensforest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data=train, nodesize=25, ntree=200)
predictforest <- predict(stevensforest, newdata = test)
table(test$Reverse, predictforest)
(40+74)/(40+74+37+19)

#random forest - Quiz
set.seed(100)
stevensforest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data=train, nodesize=25, ntree=200)
train$Reverse <- as.factor(train$Reverse)
test$Reverse <- as.factor(test$Reverse)
stevensforest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data=train, nodesize=25, ntree=200)
predictforest <- predict(stevensforest, newdata = test)
table(test$Reverse, predictforest)
(40+75)/(40+75+37+18)

set.seed(200)
stevensforest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data=train, nodesize=25, ntree=200)
train$Reverse <- as.factor(train$Reverse)
test$Reverse <- as.factor(test$Reverse)
stevensforest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data=train, nodesize=25, ntree=200)
predictforest <- predict(stevensforest, newdata = test)
table(test$Reverse, predictforest)
(40+73)/(40+73+37+20)

#Video 6: Cross-Validation
#K-fold cross validation
install.packages('caret')
library('caret')
install.packages('e1071')
library('e1071')

set.seed(3000)
numFolds <- trainControl(method='cv', number=10)
cpGrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
      data=train,method='rpart', trControl=numFolds, tuneGrid=cpGrid)

stevenstreeCV <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                       data=train, method='class', cp=0.19)

predictCV <- predict(stevenstreeCV, newdata = test, type = 'class')
table(test$Reverse, predictCV)
(59+64)/(59+64+18+29)
prp(stevenstreeCV)
