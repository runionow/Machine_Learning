# To install package Euclidean distance between two matrices.

# Set working Directory.
#setwd("C:\\Users\\arunt\\Desktop\\AML\\Assignment 1\\data")

# Install.packages("pdist")
#install.packages("dplyr")

library(dplyr)
library(pdist)

ringnorm.data <- read.csv(file="ringnorm_data.csv", header=F)
head(ringnorm.data)
#set.seed(100)

# Summary of the ringnorm_data.csv
#summary(ringnorm.data)

#Assigining colnames
colnames(ringnorm.data) <- c("k1",letters[1:(length(ringnorm.data)-1)])

#Performing cluster on labels for the initialized data.
#ggplot(data=ringnorm.data,aes())

# Plot the observations.
# Just a 2 dimensional glimpse of 2 random column doesnt show any information.
p <- ggplot(data=ringnorm.data,aes(a,b,colour=factor(k1))) + 
  geom_point(size=2,aes(shape = factor(k1)))+
  ggtitle("Ring Norm. K Means Clustering")+
  xlab("X")+
  ylab("Y")
print(p)


# Removing 1st column.
# After Watching the data it looks like a label which has only 2 unique values 1 and -1
ringnorm.data.nolabels  <- as.matrix.data.frame(ringnorm.data[,2:ncol(ringnorm.data)])

# Number of Clusters
k <- 2

# Function to get k number of random centroids.
# In this model i am picking up the random rows as centroid value.
random.centroids <- function(k){
  ringnorm.data[sample(nrow(ringnorm.data),k,replace = FALSE),]
}

# Initializing the centroids
initialize.centroids <- random.centroids(k)

# Setting up the threshold.
kmeans.threshold = 10

#plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')

#Lloyd K Means Algorithm.
k.means <- function(k,data.sets,initialize.centroids,threshold){
      
      # Calculating Distance from Centroid to all the points.
      dists <- as.matrix(pdist(data.sets,initialize.centroids))
      
      #Seperating the points and allocating them to the correspoding centroids.
      sep.matrix <- cbind(apply(dists,1,which.min))
      
      # Converting unique values to vector.
      unique.values <- sort(unique(sep.matrix))
      
      # the first matrix after randomly initilizing centroids.
      cluster.matrix <- cbind(sep.matrix,data.sets)
      
      #Empty Matrix
      centroids <- matrix(,ncol = ncol(cluster.matrix))
      
      # b. Computing centroid for each cluster.
      for(i in 1:k){
        temp <- filter(cluster.matrix,sep.matrix==1)
        mean.centroid <- colMeans(temp)
        centroids <- rbind(centroids,mean.centroid)
      }
      
      centroids <- na.omit(centroids)
      
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