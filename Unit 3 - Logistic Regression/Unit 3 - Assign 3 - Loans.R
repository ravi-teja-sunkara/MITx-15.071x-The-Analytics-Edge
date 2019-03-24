Sys.setlocale("LC_ALL", "C")

loans <- read.csv('loans.csv')
str(loans)
table(loans$not.fully.paid)
summary(loans)
is.na(loans$pub.rec)

#missing values imputed
loans <- read.csv('loans_imputed.csv')
summary(loans)

#splitting the data
set.seed(144)
library(caTools)
split=sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train=subset(loans, split==T)
test=subset(loans, split==F)

#model
mod1 <- glm(not.fully.paid ~ ., data=train, family=binomial)
summary(mod1)

#predicting
predtest <- predict(mod1, newdata = test, type='response')
table(test$not.fully.paid,predtest>0.5)
test$predicted.risk <- predtest
table(test$not.fully.paid)

#Out of sample - AUC
library(ROCR)
rocrpred <- prediction(predtest, test$not.fully.paid)
as.numeric(performance(rocrpred,'auc')@y.values)

#Smart Baseline
base <- glm(not.fully.paid ~ int.rate, data=train, family=binomial)
summary(base)

predbase <- predict(base, newdata = test, type='response')
summary(predbase)
table(test$not.fully.paid, predbase>0.5)

rocrpred <- prediction(predbase, test$not.fully.paid)
as.numeric(performance(rocrpred,'auc')@y.values)

#Problem 5.1 - A Simple Investment Strategy
test$profit <- exp(test$int.rate*3)-1
test$profit[test$not.fully.paid==1] <- -1
max(test$profit)

#Problem 6.1 - An Investment Strategy Based on Risk
highinterest <- subset(test, test$int.rate>=0.15)
summary(highinterest$profit)
table(highinterest$not.fully.paid)

cutoff <- sort(highinterest$predicted.risk, decreasing = F)[100]
selectedloans <- subset(highinterest, predicted.risk<=cutoff)
sum(selectedloans$profit)
table(selectedloans$not.fully.paid)
