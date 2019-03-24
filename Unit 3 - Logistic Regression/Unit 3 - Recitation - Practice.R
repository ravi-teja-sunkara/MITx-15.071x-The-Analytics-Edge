Sys.setlocale("LC_ALL", "C")

#Video 2: Dealing with Missing Data
#read data
polling <- read.csv('PollingData.csv')
str(polling)
table(polling$Year)

summary(polling)

#dealing with missing values
#multiple imputation
install.packages('mice')
require('mice')
simple <- polling[,c('Rasmussen','SurveyUSA','PropR','DiffCount')]
summary(simple)
set.seed(144)
imputed <- complete(mice(simple))
summary(imputed)

polling$Rasmussen <- imputed$Rasmussen
polling$SurveyUSA <- imputed$SurveyUSA
summary(polling)

#loading file with values of class as I got different values for imputation
polling <- read.csv('PollingData_Imputed.csv')
str(polling)
summary(polling)

#Video 3: A Sophisticated Baseline Method
train <- subset(polling, Year==2004 | Year==2008)
test <- subset(polling, Year==2012)

table(train$Republican)
sign(20)
sign(-10)
sign(0)

table(sign(train$Rasmussen))
table(train$Republican, sign(train$Rasmussen))

#Video 4: Logistic Regression Models
#multicollinearity - correlation matrix
cor(train[c('Rasmussen','SurveyUSA','PropR','DiffCount', 'Republican')])

mod1 <- glm(Republican ~ PropR, data=train, family=binomial)
summary(mod1)

pred1 <- predict(mod1, type='response')
table(train$Republican, pred1>=0.5)

mod2 <- glm(Republican ~ SurveyUSA+DiffCount, data=train, family=binomial)
pred2 <- predict(mod2, type='response')
table(train$Republican, pred2>=0.5)

#Video 5: Test Set Predictions
table(test$Republican, sign(test$Rasmussen))

testpred <- predict(mod2, newdata = test, type='response')
table(test$Republican, testpred>=0.5)
subset(test, testpred >=0.5 & Republican==0)
