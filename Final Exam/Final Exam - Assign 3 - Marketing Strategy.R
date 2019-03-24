Sys.setlocale("LC_ALL", "C")

#Data
orders <- read.csv('orders.csv')
str(orders)
head(orders)

sort(table(orders$order_hour_of_day))
mean(orders$days_since_prior_order)

#Stats
cor(orders$fresh.fruits, orders$fresh.vegetables)
table(orders$frozen.pizza>=1)

#Normalizing
orders.aisle <- orders[, 5:ncol(orders)]
library('caret')
preproc <- preProcess(orders.aisle)
ordersNorm <- predict(preproc, orders.aisle)

max(ordersNorm$frozen.dessert)
min(ordersNorm$soft.drinks)

#Dendogram
distances <- dist(ordersNorm, method = "euclidean")

ClusterProducts <- hclust(distances, method = "ward.D")

plot(ClusterProducts, labels = FALSE)

#k-means clust
set.seed(200)
kcluster <- kmeans(ordersNorm, centers=4, iter.max = 1000)
str(kcluster)

kc1 <- subset(ordersNorm, kcluster$cluster==1)
kc2 <- subset(ordersNorm, kcluster$cluster==2)
kc3 <- subset(ordersNorm, kcluster$cluster==3)
kc4 <- subset(ordersNorm, kcluster$cluster==4)

tail(sort(colMeans(kc1)))
tail(sort(colMeans(kc2)))
tail(sort(colMeans(kc3)))
tail(sort(colMeans(kc4)))

kc11 <- subset(orders, kcluster$cluster==1)
kc12 <- subset(orders, kcluster$cluster==2)
kc13 <- subset(orders, kcluster$cluster==3)
kc14 <- subset(orders, kcluster$cluster==4)

mean(kc11$order_hour_of_day)
mean(kc12$order_hour_of_day)
mean(kc13$order_hour_of_day)
mean(kc14$order_hour_of_day)

mean(kc11$days_since_prior_order)
mean(kc12$days_since_prior_order)
mean(kc13$days_since_prior_order)
mean(kc14$days_since_prior_order)
