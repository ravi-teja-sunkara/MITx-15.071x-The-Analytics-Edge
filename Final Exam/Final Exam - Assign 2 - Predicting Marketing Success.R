Sys.setlocale("LC_ALL", "C")

#data
bank <- read.csv('bank.csv')
str(bank)
head(bank)

mean(bank$age)

#Problem 2 - Call Durations by Job
ggplot(bank, aes(x=job, y=duration)) + geom_boxplot()
sort(tapply(bank$duration, bank$job, mean))

#Problem 3 - Multicolinearity
cor(bank$emp.var.rate, bank$cons.price.idx)
cor(bank$emp.var.rate, bank$nr.employed)
cor(bank$cons.price.idx, bank$cons.conf.idx)
cor(bank$cons.conf.idx, bank$emp.var.rate)
cor(bank$emp.var.rate, bank$euribor3m)

#Problem 4 - Splitting into a Training and Testing Set
bank$y <- as.factor(bank$y)
library('caTools')
set.seed(201)
spl <- sample.split(bank$y, 0.7)

train <- subset(bank, spl==T)
test <- subset(bank, spl==F)

#Problem 5 - Training a Logistic Regression Model
logmod <- glm(y ~ age+ job+ marital+ education+ default+ housing+ loan+ contact+ month+ day_of_week+ campaign+ pdays+ 
                      previous+ poutcome+ emp.var.rate+ cons.price.idx+ cons.conf.idx, data=train, family=binomial)
summary(logmod)

#Problem 7 - Obtaining Test Set Predictions
pred <- predict(logmod, newdata=test, type='response')
table(test$y, pred>0.5)

table(test$y)

#AUC
library('ROCR')
rocrpred <- prediction(pred, test$y)
as.numeric(performance(rocrpred, 'auc')@y.values)

rocrpref <- performance(rocrpred, 'tpr', 'fpr')
plot(rocrpref, colorize=T)

#CART
library('caret')
library('e1071')
set.seed(201)
folds <- trainControl(method='cv', number=10)
cartgrid <- expand.grid(.cp=seq(0.001, 0.05, 0.001))
cp <- train(y ~ age+ job+ marital+ education+ default+ housing+ loan+ contact+ month+ day_of_week+ campaign+ pdays+ 
                    previous+ poutcome+ emp.var.rate+ cons.price.idx+ cons.conf.idx, data=train, method='rpart', trControl=folds, tuneGrid=cartgrid)
cp

library('rpart')
library(rpart.plot)
treemod <- rpart(y ~ age+ job+ marital+ education+ default+ housing+ loan+ contact+ month+ day_of_week+ campaign+ pdays+ 
                         previous+ poutcome+ emp.var.rate+ cons.price.idx+ cons.conf.idx, data=train, cp=0.016)
prp(treemod)

pred2 <- predict(treemod, newdata=test, type='class')
table(test$y, pred2)
