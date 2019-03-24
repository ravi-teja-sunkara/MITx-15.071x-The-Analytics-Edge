Sys.setlocale("LC_ALL", "C")

###############################################################################
#                       Problem 1 - Normalizing the Data                    #
###############################################################################
airlines <- read.csv('AirlinesCluster.csv')
str(airlines)
summary(airlines)

#using 'caret' package
install.packages('caret')
library(caret)
preproc <- preProcess(airlines)
str(preproc)
airlinesNorm <- predict(preproc, airlines)
str(airlinesNorm)
summary(airlinesNorm)
airlinesNorm[1:10,]

################################################################################
#                      Problem 2.1 - Hierarchical Clustering                   #
################################################################################
distances <- dist(airlinesNorm, method='euclidian')
airnormclust <- hclust(distances, method='ward.D')
plot(airnormclust)
rect.hclust(airnormclust, k=7, border='red')

clustergroups <- cutree(airnormclust, k=5)

c1 <- subset(airlinesNorm, clustergroups==1)
c2 <- subset(airlinesNorm, clustergroups==2)
c3 <- subset(airlinesNorm, clustergroups==3)
c4 <- subset(airlinesNorm, clustergroups==4)
c5 <- subset(airlinesNorm, clustergroups==5)

table(clustergroups)

tapply(airlines$Balance, clustergroups, mean)
tapply(airlines$QualMiles, clustergroups, mean)
tapply(airlines$BonusMiles, clustergroups, mean)
tapply(airlines$BonusTrans, clustergroups, mean)
tapply(airlines$FlightMiles, clustergroups, mean)
tapply(airlines$FlightTrans, clustergroups, mean)
tapply(airlines$DaysSinceEnroll, clustergroups, mean)

###############################################################################
#                            Problem 3.1 - K-Means Clustering                 #
###############################################################################
set.seed(88)
kcluster <- kmeans(airlinesNorm, centers=5, iter.max=1000)
str(kcluster)
