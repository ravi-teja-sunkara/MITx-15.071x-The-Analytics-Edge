Sys.setlocale("LC_ALL", "C")

#Data
kos <- read.csv('dailykos.csv')
str(kos)

#hierarchial clustering
distances <- dist(kos, method='euclidian')
clusterkos <- hclust(distances, method='ward.D')
plot(clusterkos)

clustergroups <- cutree(clusterkos, k=7)

#creating subsets based on clusters
c1 <- subset(kos, clustergroups==1)
c2 <- subset(kos, clustergroups==2)
c3 <- subset(kos, clustergroups==3)
c4 <- subset(kos, clustergroups==4)
c5 <- subset(kos, clustergroups==5)
c6 <- subset(kos, clustergroups==6)
c7 <- subset(kos, clustergroups==7)

#top 6 words in a cluster
tail(sort(colMeans(c1)))
tail(sort(colMeans(c2)))
tail(sort(colMeans(c3)))
tail(sort(colMeans(c4)))
tail(sort(colMeans(c5)))
tail(sort(colMeans(c6)))
tail(sort(colMeans(c7)))

################################################################################
#                            Problem 2 - K-Means Clustering                  #
################################################################################
set.seed(1000)
kmckos <- kmeans(kos, centers=7)
str(kmckos)

kc1 <- subset(kos, kmckos$cluster==1)
kc2 <- subset(kos, kmckos$cluster==2)
kc3 <- subset(kos, kmckos$cluster==3)
kc4 <- subset(kos, kmckos$cluster==4)
kc5 <- subset(kos, kmckos$cluster==5)
kc6 <- subset(kos, kmckos$cluster==6)
kc7 <- subset(kos, kmckos$cluster==7)

#displaying top 6
tail(sort(colMeans(kc1)))
tail(sort(colMeans(kc2)))
tail(sort(colMeans(kc3)))
tail(sort(colMeans(kc4)))
tail(sort(colMeans(kc5)))
tail(sort(colMeans(kc6)))
tail(sort(colMeans(kc7)))

#comparing clusters
table(clustergroups, kmckos$cluster)
