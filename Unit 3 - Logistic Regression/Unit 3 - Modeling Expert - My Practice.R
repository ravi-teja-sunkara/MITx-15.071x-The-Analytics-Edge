Sys.setlocale("LC_ALL", "C")

#Video 4: Logistic Regression in R
quality <- read.csv('quality.csv')
str(quality)
table(quality$PoorCare)
r2 <- 98/131 #mode will be considered the baseline. 0 is good care; 1 - Poor care as 98 patients are receiving good care r2 will be calculated on that

#dividing into training and testing data sets
install.packages('caTools')
require('caTools')
set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)
qualitytrain <- subset(quality, split==T)
qualitytest <- subset(quality, split==F)

#model
qualitylog <- glm(PoorCare ~ OfficeVisits + Narcotics, data=qualitytrain, family=binomial)
summary(qualitylog)

predicttrain <- predict(qualitylog, type='response')
summary(predicttrain)
tapply(predicttrain, qualitytrain$PoorCare, mean)

#model 2
qualitylog2 <- glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualitytrain, family = binomial)
summary(qualitylog2)

#Video 5: Thresholding
table(qualitytrain$PoorCare, predicttrain>0.5)
10/25 #sensitivity or True Positive
70/74 #specificity or True Neg

table(qualitytrain$PoorCare, predicttrain>0.7)
8/25
73/74

table(qualitytrain$PoorCare, predicttrain>0.2)
16/25
54/74

#Video 6: ROC Curves
install.packages('ROCR')
require('ROCR')
ROCRpred <- prediction(predicttrain, qualitytrain$PoorCare)
ROCperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCperf)
plot(ROCperf, colorize=T)
plot(ROCperf, colorize=T, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))

#Video 7: Interpreting the Model
qualitylog3 <- glm(PoorCare ~ OfficeVisits + Narcotics, data=qualitytrain, family = binomial)
summary(qualitylog3)

predicttest <- predict(qualitylog3, type='response', newdata=qualitytest)
rocpredtest <- prediction(predicttest, qualitytest$PoorCare)
auc <- as.numeric(performance(rocpredtest,'auc')@y.values)
