Sys.setlocale("LC_ALL", "C")

###################### Video 6: Claims Data in R ####################
claims <- read.csv('ClaimsData.csv')
str(claims)
table(claims$bucket2008)/nrow(claims)

#splitting the data
library('caTools')
set.seed(88)
sample <- sample.split(claims$bucket2009, SplitRatio = .6)
ctrain <- subset(claims, sample == T)
ctest <- subset(claims, sample == F)


################ Video 7: Baseline Method and Penalty Matrix ###########
# the cost bucket for a patient in 2009 will be same as in 2008 - Baseline model
table(ctest$bucket2009, ctest$bucket2008)
(110138+10721+2774+1539+104)/nrow(ctest) #accuracy

penaltymatrix <- matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow=T,nrow=5) #predicted/forecasted on TOP/Columns. ACTUAL on ROWS
penaltymatrix

#calculating penalty by Multiplying with the number of wrong classifications
as.matrix(table(ctest$bucket2009, ctest$bucket2008)) * penaltymatrix
penaltyerror <- sum(as.matrix(table(ctest$bucket2009, ctest$bucket2008)) * penaltymatrix)/nrow(ctest) #total penalty error
penaltyerror

# quiz video 7 - different baseline model
# we used the baseline method of predicting the most frequent outcome for all observations. This new baseline method would predict cost bucket 1 for everyone.
table(ctest$bucket2009)
#as the most frequent outcome is costbucket of 1. so treating that as the predicted output for all
122978/nrow(ctest)  #accuracry
predictedmatrix <- matrix(c(table(ctest$bucket2009),0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), byrow=F, nrow=5)
predictedmatrix * penaltymatrix
sum(predictedmatrix * penaltymatrix)/nrow(ctest)

################### Video 8: Predicting Healthcare Costs in R ###############
library(rpart)
library(rpart.plot)
claimstree <- rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes +
                      heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008,
                    data=ctrain, method='class', cp=0.00005)
prp(claimstree)

predicttest <- predict(claimstree, newdata = ctest, type='class')
table(ctest$bucket2009, predicttest)
(114141+16102+118+201+0)/nrow(ctest) #accuracy
sum(as.matrix(table(ctest$bucket2009, predicttest)) * penaltymatrix)/nrow(ctest) #penaltyerror

#using 'parms' to define the penalty as per penalty matrix
claimstree <- rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes +
                      heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008,
                    data=ctrain, method='class', cp=0.00005, parms=list(loss=penaltymatrix))
predicttest <- predict(claimstree, newdata = ctest, type='class')
table(ctest$bucket2009, predicttest)
(94310+18942+4692+636+2)/nrow(ctest)
sum(as.matrix(table(ctest$bucket2009, predicttest)) * penaltymatrix)/nrow(ctest) #penaltyerror
