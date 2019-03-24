Sys.setlocale("LC_ALL", "C")

############ Video 6: Getting the Data #######
movies <- read.table('movieLens.txt', header=F, sep='|', quote="\"")
str(movies)

colnames(movies) <- c('ID', 'Title', 'ReleaseDate', 'VideoReleaseDate','IMDB','Unknown','Action',
                      'Adventure','Animation','Childrens', 'Comedy', 'Crime','Documentary', 'Drama',
                      'Fantasy', 'FilmNoir','Horror','Musical','Mystery','Romance','SciFi','Thriller',
                      'War','Western')
str(movies)

#removing variables
movies$ID <- NULL
movies$ReleaseDate <- NULL
movies$VideoReleaseDate <- NULL
movies$IMDB <- NULL

#missing values
movies <- unique(movies)
str(movies)

############# Video 7: Hierarchical Clustering in R ######
#clustering only on Genre variable i.e., 2 to 20 in movies. Finding distance before clustering
distances <- dist(movies[2:20], method='euclidian')
clusterMovies <- hclust(distances, method='ward.D')
plot(clusterMovies)
clusterGroups <- cutree(clusterMovies, k=10)
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

#suggestions to Amy who watched 'men in black'
subset(movies, Title=='Men in Black (1997)')
clusterGroups[257] #257 is the observation of 'Men in Black'

#creating movies that are in cluster 2
cluster2 <- subset(movies, clusterGroups==2)
cluster2$Title[1:10]

#Quiz
clusterGroups2 <- cutree(clusterMovies, k=2)
tapply(movies$Action, clusterGroups2, mean)
tapply(movies$Adventure, clusterGroups2, mean)
tapply(movies$Animation, clusterGroups2, mean)
tapply(movies$Childrens, clusterGroups2, mean)
tapply(movies$Documentary, clusterGroups2, mean)
tapply(movies$Horror, clusterGroups2, mean)
tapply(movies$Romance, clusterGroups2, mean)
tapply(movies$Thriller, clusterGroups2, mean)
tapply(movies$Western, clusterGroups2, mean)
tapply(movies$War, clusterGroups2, mean)
tapply(movies$Drama, clusterGroups2, mean)

colMeans(subset(movies[2:20], clusterGroups2 == 1)) #alternate approach ---> 1
colMeans(subset(movies[2:20], clusterGroups2 == 2))

spl <- split(movies[2:20], clusterGroups) # Alternate Approach ------> 2
spl[[1]] #is equal to subset(movies[2:20], clusterGroups == 1)
colMeans(spl[[1]])

lapply(spl, colMeans) #Alternate Approach -----------> 3

######### An Advanced Approach to Finding Cluster Centroids ####################
# 
# In this video, we explain how you can find the cluster centroids by using the function "tapply" for each variable in the dataset. While this approach works and is familiar to us, it can be a little tedious when there are a lot of variables. An alternative approach is to use the colMeans function. With this approach, you only have one command for each cluster instead of one command for each variable. If you run the following command in your R console, you can get all of the column (variable) means for cluster 1:
#         
#         colMeans(subset(movies[2:20], clusterGroups == 1))
# 
# You can repeat this for each cluster by changing the clusterGroups number. However, if you also have a lot of clusters, this approach is not that much more efficient than just using the tapply function.
# 
# A more advanced approach uses the "split" and "lapply" functions. The following command will split the data into subsets based on the clusters:
#         
#         spl = split(movies[2:20], clusterGroups)
# 
# Then you can use spl to access the different clusters, because
# 
# spl[[1]]
# 
# is the same as
# 
# subset(movies[2:20], clusterGroups == 1)
# 
# so colMeans(spl[[1]]) will output the centroid of cluster 1. But an even easier approach uses the lapply function. The following command will output the cluster centroids for all clusters:
#         
#         lapply(spl, colMeans)
# 
# The lapply function runs the second argument (colMeans) on each element of the first argument (each cluster subset in spl). So instead of using 19 tapply commands, or 10 colMeans commands, we can output our centroids with just two commands: one to define spl, and then the lapply command.
# 
# Note that if you have a variable called "split" in your current R session, you will need to remove it with rm(split) so that you can use the split function.
# 
# 
