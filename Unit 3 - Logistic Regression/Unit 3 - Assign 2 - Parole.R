Sys.setlocale("LC_ALL", "C")

#understanding the data
parole <- read.csv('parole.csv')
str(parole)
summary(parole$state)

table(parole$violator)

#converting to factors
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
str(parole)
summary(parole$state)

#Splitting into a Training and Testing Set
set.seed(144)
library(caTools)
split=sample.split(parole$violator, SplitRatio = 0.7)
train=subset(parole,split==T)
test=subset(parole, split==F)

#modelling
mod1 <- glm(violator ~ ., data=train, family=binomial)
summary(mod1)

(-4.2411574+0.3869904+0.8867192+(50*-0.0001756)+(3*-0.1238867)+(12*0.0802954)+0.6837143)
exp(-1.700629)
1/(1+exp(1.700629))

#predicting
predtest <- predict(mod1, newdata = test, type = 'response')
max(predtest)

table(test$violator, predtest>0.5)
12/(12+11) #sensitivity
167/(167+12)
(167+12)/(167+12+12+11)

table(test$violator, predtest>0)

#Out of sample - AUC
library(ROCR)
rocrpred <- prediction(predtest, test$violator)
as.numeric(performance(rocrpred, 'auc')@y.values)
