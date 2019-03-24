Sys.setlocale("LC_ALL", "C")

####################### Video 2: The Data ###################
boston <- read.csv('boston.csv')
str(boston)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS==1], col='red', pch=19)
points(boston$LON[boston$TRACT==3531], boston$LAT[boston$TRACT==3531], col='blue', pch=19)
summary(boston$NOX)
points(boston$LON[boston$NOX>=0.55], boston$LAT[boston$NOX>=.55], col='green', pch=19)
summary(boston$MEDV)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col='maroon', pch=1)

#################### Video 3: Geographical Predictions ###############
#fitting a linear model
linearmod <- lm(MEDV ~ LAT + LON, data=boston)
summary(linearmod)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col='navy', pch=19)
linearmod$fitted.values #the values linear reg has predicted
points(boston$LON[linearmod$fitted.values>=21.2], boston$LAT[linearmod$fitted.values>=21.2], col='red', pch="^")

################### Video 4: Regression Trees ###############
library(rpart)
library(rpart.plot)
latlontree <- rpart(MEDV ~ LAT + LON, data=boston)
prp(latlontree)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col='navy', pch=19)
fittedvaluestreee <- predict(latlontree)
points(boston$LON[fittedvaluestreee>=21.2], boston$LAT[fittedvaluestreee>=21.2], col='red', pch='$')

latlontree2 <- rpart(MEDV ~ LAT + LON, data=boston, minbucket=50)
plot(latlontree2)
text(latlontree2)
plot(boston$LON, boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col='navy', pch=19)

################# Video 5: Putting it all Together ##############
library(caTools)
set.seed(123)
split=sample.split(boston$MEDV, SplitRatio = .7 )
train <- subset(boston, split==T)
test <- subset(boston, split==F)

#linear reg
linmod1 <- lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO,
              data=train)
summary(linmod1)
linmod1.pred <- predict(linmod1, newdata = test)
linmod1.sse <- sum((linmod1.pred - test$MEDV)^2)
linmod1.sse

#trees
tree <- rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO,
              data=train)
prp(tree)
tree.pred <- predict(tree, newdata = test)
tree.sse <- sum((tree.pred - test$MEDV)^2)
tree.sse

######################## Video 7: Cross-Validation ############
library('caret')
library('e1071')
tr.control = trainControl(method='cv', number=10)
cpgrid <- expand.grid(.cp=(0:10)*0.001)
tr <- train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO,
            data=train, method='rpart', trControl=tr.control, tuneGrid=cpgrid)
tr

best.tree <- tr$finalModel
prp(best.tree)
best.tree.pred <- predict(best.tree, newdata=test)
best.tree.sse <- sum((best.tree.pred - test$MEDV)^2)
best.tree.sse
