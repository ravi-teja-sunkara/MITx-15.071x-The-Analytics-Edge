Sys.setlocale("LC_ALL", "C")

#data
visits <- read.csv("park_visits.csv")
str(visits)
summary(visits)

#Problem 1  - Number of National Parks in Jan 2016
visits2016jul <- subset(visits, Month==7 & Year==2016)
str(visits2016jul)

sort(table(visits2016jul$ParkType))
which.max(visits2016jul$logVisits)
visits2016jul[138,]

#Problem 2 - Relationship Between Region and Visits
sort(tapply(visits2016jul$logVisits, visits2016jul$Region, mean, na.rm=T))

cor(visits2016jul$cost, visits2016jul$logVisits)

#Problem 4 - Time Series Plot of Visits
ys <- subset(visits, ParkName=='Yellowstone NP')
ys_ts <- ts(ys$logVisits, start=c(2010, 1), freq=12)
plot(ys_ts)

#Problem 5 - Missing Values
colSums(is.na(visits))
visits <- visits[rowSums(is.na(visits))==0, ]

#Problem 6 - Predicting Visits
visits$Month <- as.factor(visits$Month)
train <- subset(visits, Year== 2010 | Year==2011 | Year==2012 | Year==2013 | Year==2014)
test <- subset(visits, Year== 2015 | Year==2016)

mod <- lm(logVisits~laglogVisits, data=train)
summary(mod)

pred <- predict(mod, newdata=test)
sse <- sum((pred - test$logVisits)^2)
sst <- sum((test$logVisits - mean(train$logVisits))^2)
r2 <- 1 - (sse/sst)

# Problem 7 - Add New Variables
mod1 <- lm(logVisits ~ laglogVisits+laglogVisitsYear+Year+Month+Region+ParkType+cost, data=train)
summary(mod1)

pred1 <- predict(mod1, newdata=test)
sse <- sum((pred1 - test$logVisits)^2)
sst <- sum((test$logVisits - mean(train$logVisits))^2)
r2 <- 1 - (sse/sst)

#Problem 9 - Regression Trees
library('rpart')
library('rpart.plot')

tree <- rpart(logVisits ~ laglogVisits+laglogVisitsYear+Year+Month+Region+ParkType+cost, data=train, cp=0.05)
prp(tree)

treepred <- predict(tree, newdata=test)
sse <- sum((treepred - test$logVisits)^2)
sst <- sum((test$logVisits - mean(train$logVisits))^2)
r2 <- 1 - (sse/sst)

# Problem 10 - Regression Trees with CV
library('caret')
library('e1071')
set.seed(201)
folds <- trainControl(method='cv', number=10)
cartgrid <- expand.grid(.cp=seq(0.0001, 0.0001, 0.005))
cp <- train(logVisits ~ laglogVisits+laglogVisitsYear+Year+Month+Region+ParkType+cost, data=train, method='rpart', trControl=folds, tuneGrid=cartgrid)
cp

tree1 <- rpart(logVisits ~ laglogVisits+laglogVisitsYear+Year+Month+Region+ParkType+cost, data=train, cp=0.0001)
treepred <- predict(tree1, newdata=test)
sse <- sum((treepred - test$logVisits)^2)
sst <- sum((test$logVisits - mean(train$logVisits))^2)
r2 <- 1 - (sse/sst)

# Problem 12 - Random Forest
library('randomForest')

forest <- randomForest(logVisits ~ laglogVisits+laglogVisitsYear+Year+Month+Region+ParkType+cost, data=train)
forestpred <- predict(forest, newdata=test)
sse <- sum((forestpred - test$logVisits)^2)
sst <- sum((test$logVisits - mean(train$logVisits))^2)
r2 <- 1 - (sse/sst)
