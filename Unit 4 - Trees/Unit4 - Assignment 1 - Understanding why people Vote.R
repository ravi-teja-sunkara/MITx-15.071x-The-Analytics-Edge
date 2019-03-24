Sys.setlocale("LC_ALL", "C")

############# Problem 1 - Exploration and Logistic Regression #############
gerber <- read.csv('gerber.csv')
str(gerber)
summary(gerber)

table(gerber$voting)/nrow(gerber)

#groups and voting relation
table(gerber$voting, gerber$civicduty)/nrow(gerber)
table(gerber$voting, gerber$hawthorne)/nrow(gerber)
table(gerber$voting, gerber$self)/nrow(gerber)
table(gerber$voting, gerber$neighbors)/nrow(gerber)

#logistic regression model
logmod <- glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family=binomial)
summary(logmod)

predicted <- predict(logmod, type='response')
table(gerber$voting, predicted>0.3)
(134513+51966)/nrow(gerber) #accuracy

table(gerber$voting, predicted>0.5)
235388/nrow(gerber)

#baseline model
table(gerber$voting)/nrow(gerber) #for threshold of 0.5 is same as the baseline
library('ROCR')
rocrpred <- prediction(predicted, gerber$voting)
as.numeric(performance(rocrpred, 'auc')@y.values)

################################ Problem 2 - TREES #########################
library('rpart')
library('rpart.plot')

treemod <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(treemod)

treemod2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0) #to force the complete tree to be built.
prp(treemod2)

treemod3 <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(treemod3)

########################### Problem 3 - Interaction Terms ###################
treemod4 <- rpart(voting ~ control, data=gerber, cp=0.0)
treemod5 <- rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(treemod4, digits = 6)
prp(treemod5, digits = 6)

#going back to logistic reg
logmod2 <- glm(voting ~ control + sex, data=gerber, family=binomial)
summary(logmod2)

#The regression tree calculated the percentage voting exactly for every one of the four possibilities
#(Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control). Logistic regression has
#attempted to do the same, although it wasn't able to do as well because it can't consider exactly the
#joint possibility of being a women and in the control group.
#The four values in the results correspond to the four possibilities in the order they are stated above 
#( (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control) ).
possibilities <- data.frame(sex=c(0,0,1,1), control=c(0,1,0,1))
predict(logmod2, newdata = possibilities, type='response')

logmod3 <- glm(voting ~ control + sex + sex:control, data=gerber, family=binomial)
summary(logmod3)
predict(logmod3, newdata = possibilities, type='response')
