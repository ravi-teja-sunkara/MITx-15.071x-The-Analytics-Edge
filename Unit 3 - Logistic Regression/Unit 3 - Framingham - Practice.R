Sys.setlocale("LC_ALL", "C")

#Video 3: A Logistic Regression Model
framingham <- read.csv('framingham.csv')
str(framingham)

#splitting
require('caTools')
set.seed(1000)
split <- sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train <- subset(framingham, split==T)
test <- subset(framingham, split==F)

#model creation
framinghamlog <- glm(TenYearCHD ~ ., data=train, family=binomial) # . for using all the variables
summary(framinghamlog)

#predicting on test dataset
predicttest <- predict(framinghamlog, type='response', newdata = test)
table(test$TenYearCHD, predicttest>0.5)
(1069+11)/(1069+11+6+187) #Accuracy

#baseline model - more frequent is 0 
(1069+6)/(1069+11+6+187) #accuracy of baseline mode

#Out of sample - AUC
library(ROCR)
rocrpred <- prediction(predicttest, test$TenYearCHD)
as.numeric(performance(rocrpred, 'auc')@y.values)
