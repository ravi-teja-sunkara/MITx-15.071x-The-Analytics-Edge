Sys.setlocale("LC_ALL", "C")
library(ggplot2)
library(maps)
library(ggmap)

################################################################################
#                   Problem 1 - Drawing a Map of the US                      #
################################################################################
statesMap <- map_data('state')
str(statesMap)
table(statesMap$group)

#map using ggplot
ggplot(statesMap, aes(x=long, y=lat, grou=group)) + geom_polygon(fill='white', color='black')

################################################################################
#                  Problem 2.1 - Coloring the States by Predictions            #
################################################################################
polling <- read.csv('PollingImputed.csv')
str(polling)
Train <- subset(polling, Year!= 2012)
str(Train)
Test <- subset(polling, Year==2012)
str(Test)

#logistic model
mod <- glm(Republican ~ SurveyUSA+DiffCount, data=Train, family='binomial')
pred <- predict(mod, newdata=Test, type='response')
TestPredBinary <- as.numeric(pred>0.5)
predictiondf <- data.frame(pred, TestPredBinary, Test$State)
str(predictiondf)
table(predictiondf$TestPredBinary)
mean(pred)

#merging with the map data
predictiondf$region <- tolower(predictiondf$Test.State)
predictionMap <- merge(statesMap, predictiondf, by='region')
predictionMap <- predictionMap[order(predictionMap$order),] #ordering
str(predictionMap)

#map of prediction
ggplot(predictionMap, aes(x=long, y=lat, group=group, fill=TestPredBinary)) + geom_polygon(color='black')

#changing colors Dem=blue , Rep=red
ggplot(predictionMap, aes(x=long, y=lat, group=group, fill=TestPredBinary)) + geom_polygon(color='black') + scale_fill_gradient(low='blue', high='red', guide='legend', breaks=c(0,1), labels=c('Democrat', 'Republican'), name='Prediction 2012')
ggplot(predictionMap, aes(x=long, y=lat, group=group, fill=pred)) + geom_polygon(color='black') + scale_fill_gradient(low='blue', high='red', guide='legend', breaks=c(0,1), labels=c('Democrat', 'Republican'), name='Prediction 2012')

################################################################################
#                       Problem 4 - Parameter Settings                         #
################################################################################
ggplot(predictionMap, aes(x=long, y=lat, group=group, fill=TestPredBinary)) + geom_polygon(linetype=2, color='black') + scale_fill_gradient(low='blue', high='red', guide='legend', breaks=c(0,1), labels=c('Democrat', 'Republican'), name='Prediction 2012')

ggplot(predictionMap, aes(x=long, y=lat, group=group, fill=TestPredBinary)) + geom_polygon(size=2, color='black') + scale_fill_gradient(low='blue', high='red', guide='legend', breaks=c(0,1), labels=c('Democrat', 'Republican'), name='Prediction 2012')
