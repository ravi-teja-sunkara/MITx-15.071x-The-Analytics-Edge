################### Video 2: Clustering Pixels #############
#data
flower <- read.csv('flower.csv', header=F)
str(flower)

flowerMatrix <- as.matrix(flower)
str(flowerMatrix)

#making intensity vector
flowerVector <- as.vector(flowerMatrix)
str(flowerVector)

#*important: convert to matrix then to vector. Directly converting to vector from Data Frame produces 50*50 again

#distance matrix
distance <- dist(flowerVector, method='euclidian')

################ Video 3: Hierarchical Clustering ############
clusterIntensity <- hclust(distance, method='ward.D')
plot(clusterIntensity)
rect.hclust(clusterIntensity, k=3, border='red') #plotting rectangles
flowerClusters <- cutree(clusterIntensity, k=3)
flowerClusters

#to find mean intensity
tapply(flowerVector, flowerClusters, mean)

#to output image we need to input matrix
dim(flowerClusters) <- c(50,50)
image(flowerClusters, axes=F)
image(flowerMatrix, axes=F, col=grey(seq(0,1, length=256))) #originial image

#################### Video 4: MRI Image ################
healthy <- read.csv('healthy.csv' , header=F)
healthyMatrix <- as.matrix(healthy)
str(healthyMatrix)

image(healthyMatrix, axes=F, col=grey(seq(0,1,length=256)))

healthyVector <- as.vector(healthyMatrix)
str(healthyVector)
n <- 365636
n*(n-1)/2
#the value is very large and so heirarichal clustering can't be used

################## Video 5: K-Means Clustering #################
k=5
set.seed(1)
kmc <- kmeans(healthyVector, centers=k, iter.max=1000)
str(kmc)
healthyClusters <- kmc$cluster
kmc$centers[2]

dim(healthyClusters) <- c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes=F, col=rainbow(k))

################ Video 6: Detecting Tumors ##################
tumor <- read.csv('tumor.csv', header=F)
tumorMatrix <- as.matrix(tumor)
tumorVector <- as.vector(tumorMatrix)

#we are using healthy data set as training set and tumor as testing
install.packages('flexclust')
library(flexclust)
kmc.kcca <- as.kcca(kmc, healthyVector)
tumorClusters <- predict(kmc.kcca, newdata=tumorVector)
dim(tumorClusters) <- c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes=F, col=rainbow(k))
