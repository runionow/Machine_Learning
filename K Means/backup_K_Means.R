# To install package Euclidean distance between two matrices.
# Install.packages("pdist")
#install.packages("dplyr")
library(dplyr)
library(pdist)

ringnorm.data <- read.csv(file="ringnorm_data.csv", header=F)
head(ringnorm.data)
#set.seed(100)



summary(ringnorm.data)
#Assigining colnames
colnames(ringnorm.data) <- c("k1",letters[1:(length(ringnorm.data)-1)])

#Performing cluster on labels for the initialized data.
ggplot(data=ringnorm.data,aes())

# a. Plot the observations.
p <- ggplot(data=ringnorm.data,aes(a,b,colour=factor(k1))) + 
  geom_point(size=2,aes(shape = factor(k1)))+
  ggtitle("Ring Norm. K Means Clustering")+
  xlab("X")+
  ylab("Y")
print(p)

temp  <- select(data=ringnorm.data,letters[1:(length(ringnorm.data)-1)])


# Number of Clusters
k <- 2

# Function to get k number of random centroids.
random.centroids <- function(k){
  ringnorm.data[sample(nrow(ringnorm.data),k,replace = FALSE),]
}

# Initializing the centroids
initialize.centroids <- random.centroids(k)



X=NULL
# Given dataset in Assignment.
data.x <- c(2,1,22,42,15)
data.y <- c(5,5,55,12,16)



# Random Centroids.
centroid.x <- c(16,2)
centroid.y <- c(19,5)



# Building matrices.
data.2d <- cbind(data.x,data.y)
data.centroid <- cbind(centroid.x,centroid.y)
data.threshold = 10

#plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')

#Lloyd K Means Algorithm.
k.means <- function(k,data.2d,data.centroid,threshold){
  # Calculating Distance from Centroid to all the points.
  dists <- pdist(data.2d,data.centroid)
  dist.matrix <- as.matrix(dists)
  
  #Seperating the points and allocating them to the correspoding centroids.
  sep.matrix <- cbind(apply(dist.matrix,1,which.min))
  
  # Debugging.
  #dim(sep.matrix)
  #print(sep.matrix)
  
  # Converting unique values to vector.
  unique.values <- sort(unique(sep.matrix))
  
  cluster.matrix <- cbind(sep.matrix,data.2d)
  #print(cluster.matrix)
  
  #Hard Coding - kxk matrix.
  dimensions.Cluster <- matrix(,nrow = k,ncol = ncol(data.2d))
  
  #Initialize based on number of Cluster.
  #Hard Coding - kxk matrix.
  data.new.Centroids <- matrix(,nrow = k,ncol = ncol(data.2d))
  
  #dev.new()
  #plot(data.2d[,1],data.2d[,2],pch=sample(15:20,1,replace = F),col = sample(1:657,1))
  
  for(i in 1:length(unique.values)){
    dimensions.Cluster <- cluster.matrix[cluster.matrix[,1]==unique.values[i], ]
    data.new.Centroids [i,] <- colMeans(dimensions.Cluster[,2:ncol(dimensions.Cluster)])
    #points(dimensions.Cluster[,2],dimensions.Cluster[,3],pch=sample(15:20,1,replace = F),col = sample(1:657,1))
  }
  
  # Convergence Condition [check the threshold if not rerun the K-Means function with new centroids]
  
  sub.centroids <- data.centroid - data.new.Centroids
  
  kmeans.convergence <-c(0)
  for(i in 1:nrow(sub.centroids)){
    kmeans.convergence <- kmeans.convergence +  sqrt(t(sub.centroids[i,]) %*% (sub.centroids[i,]))/2
  }
  
  if(kmeans.convergence<threshold){
    data.new.Centroids
  }
  else{
    k.means(k,data.2d,data.centroid,threshold)
  }
}


final.centroid <- k.means(k,data.2d,data.centroid,data.threshold)
print(final.centroid)