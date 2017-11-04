# @Author : Arun Nekkalapudi.

# ==============================================================
#Environment Preperation.
#-----------------------

# 1. Set the working directory to the path where all data files are available.
# setwd("C:\\Users\\arunt\\Desktop\\AML\\Assignment 2\\data")

# 2. Install below mentioned packages.
#install.packages("pdist")
#install.packages("dplyr")
#install.packages("ggplot2")
# ==============================================================

library(dplyr)
library(ggplot2)
library(pdist)

ringnorm.data <- read.csv(file="ringnorm_data.csv", header=F)
head(ringnorm.data)
#set.seed(100)

#Summary of the ringnorm_data.csv
#summary(ringnorm.data)

# Removing 1st column.
# After Watching the data it looks like a label which has only 2 unique values 1 and -1
ringnorm.data.nolabels  <- as.data.frame(ringnorm.data[,2:ncol(ringnorm.data)])
summary(ringnorm.data.nolabels)



pc.ringworm <- princomp(ringnorm.data.nolabels, scores = TRUE)
summary(pc.ringworm)


# Keeping 90% variance
ringnorm.data.nolabels.pca  <- as.matrix.data.frame(pc.ringworm$scores[,1:18])





# ==============================================================
# NAME: random.centroids(k,data.set)
# DESCRIPTION: Function to get k number of random centroids.
# ARGUMENTS: 
# 1. K [integer] - Number of Clusters.
# 2. data.set [data.frame] - Input dataset.
# RETURNS: returns k number of random rows from the dataset.
# ==============================================================
random.centroids <- function(k,data.set){
  data.set[sample(nrow(data.set),k,replace = FALSE),]
}


# ==============================================================
# NAME: k.means(k,data.sets,initialize.centroids,threshold)
# DESCRIPTION: Lloyd K Means Algorithm.
# ARGUMENTS: 
# 1. K[integer] - Number of Clusters.
# 2. data.sets[data.frame] - Input dataset.
# 3. initialize.centroids[data.frame] - Initial Centroids to begin K Means.
# 4. threshold[integer] - Set the threshold.
# RETURNS: returns k number of random rows from the dataset.
# ==============================================================
k.means <- function(k,data.sets,initialize.centroids,threshold){
  #Initializing master centroid data.
  master.centroids <- matrix(,ncol = ncol(data.sets))
  
  # Calculating Distance from Centroid to all the points.
  dists <- as.matrix(pdist(data.sets,initialize.centroids))
  
  #Seperating the points and allocating them to the correspoding centroids.
  sep.matrix <- cbind(apply(dists,1,which.min))
  
  # Converting unique values to vector.
  unique.values <- sort(unique(sep.matrix))
  
  # the first matrix after randomly initilizing centroids.
  cluster.matrix <- as.data.frame(cbind(sep.matrix,data.sets))
  
  #Empty Matrix
  new.centroids <- matrix(,ncol = (ncol(cluster.matrix)-1))
  
  # Renamin the first column to k1 
  colnames(cluster.matrix)[1] <- c("kk")
  
  
  # b. Computing centroid for each cluster.
  for(i in 1:k){
    temp <- filter(cluster.matrix,kk == get("i"))
    mean.centroid <- colMeans(temp[,2:ncol(temp)])
    new.centroids <- rbind(new.centroids,mean.centroid)
  }
  
  cluster.matrix = cluster.matrix[,2:ncol(cluster.matrix)]
  
  new.centroids <- na.omit(new.centroids)
  master.centroids <-rbind(master.centroids,new.centroids)
  
  # Convergence Condition [check the threshold if not rerun the K-Means function with new centroids]
  sub.centroids <- t(as.matrix(initialize.centroids)) - t(as.matrix(new.centroids))
  kmeans.convergence <-c(0)
  for(i in 1:ncol(sub.centroids)){
    kmeans.convergence <- kmeans.convergence +  sqrt(t(sub.centroids[,i]) %*% as.matrix(sub.centroids[,i]))/k
  }
  
  if(kmeans.convergence<threshold){
    master.centroids <- na.omit(master.centroids)
    #return
    new.centroids
  }
  else{
    k.means(k,data.sets,new.centroids,kmeans.threshold)
  }
}

# ==============================================================
# NAME: points.to.centroid(centroid.set,data.set)
# DESCRIPTION: points to cluster mapper.
# ARGUMENTS: 
# 1. centroid.set [data.frame] - Centroid data.
# 2. data.set [data.frame] - Input dataset.
# RETURNS: returns a [data.frame] points belong to a cluster.
# ==============================================================
points.to.centroid <-function(centroid.set,data.set){
  # Calculating Distance from Centroid to all the points.
  dists <- as.matrix(pdist(data.set,centroid.set))
  
  #Seperating the points and allocating them to the correspoding centroids.
  sep.matrix <- cbind(apply(dists,1,which.min))
  
  #the first matrix after randomly initilizing centroids.
  cluster.matrix <- cbind(sep.matrix,data.set)
  
  #return
  cluster.matrix
}

# ==============================================================
# NAME: calculate.good.bad.ringnorm(cluster.matrix,k,j)
# DESCRIPTION: Calculate good bad.
# ARGUMENTS: 
# 1. cluster.matrix [data.frame] - Data Set.
# 2. k [integer] - Number of clusters.
# 3. j [integer] - Iteration.
# RETURNS: returns [data.frame] good bad.
# ==============================================================
calculate.good.bad.ringnorm <- function (cluster.matrix,k,j){
  good.bad <- matrix(,ncol =4,nrow = k)
  cluster.matrix <- as.data.frame(cluster.matrix)
  colnames(cluster.matrix)[2] <- c("cluster")
  
  colnames(cluster.matrix)[1] <- c("label")
  
  for(i in 1:k){
    good <- nrow(filter(cluster.matrix,cluster == get("i") & label == 1))
    bad <- nrow(filter(cluster.matrix,cluster == get("i") & label == -1))
    good.bad <- rbind(good.bad,c(j,k,good,bad))
  }
  good.bad <- na.omit(good.bad)
  
  #return
  good.bad
}



#Assigining colnames
colnames(ringnorm.data) <- c("kk",letters[1:(length(ringnorm.data)-1)])

#Performing cluster on labels for the initialized data.
#ggplot(data=ringnorm.data,aes())

# Plot the observations.
# Just a 2 dimensional glimpse of 2 random column doesnt show any information.
# p <- ggplot(data=ringnorm.data,aes(a,b,colour=factor(k1))) + 
#   geom_point(size=2,aes(shape = factor(k1)))+
#   ggtitle("Ring Norm. K Means Clustering")+
#   xlab("X")+
#   ylab("Y")
# print(p)


# Number of Clusters
k <- 2

# Setting up the threshold.
kmeans.threshold <- 10

ringnorm.data.nolabels <- as.data.frame(ringnorm.data.nolabels)

good.bad.noPCA <- matrix(,ncol =4)
good.bad.PCA <- matrix(,ncol =4)

for(i in 1:20){
  for(j in 2:5){
    initialize.centroids <- random.centroids(j,ringnorm.data.nolabels)
    
    #Initialize centroids with PCA
    initialize.centroids.pca <- random.centroids(j,ringnorm.data.nolabels.pca)
    
    # The Ultimate Showdown w/o PCA
    final.centroid <- k.means(j,ringnorm.data.nolabels,initialize.centroids,kmeans.threshold)
    
    
    # The Ultimate Showdown w PCA
    # final.centroid.pca <- k.means(j,ringnorm.data.nolabels.pca,initialize.centroids.pca,kmeans.threshold)
    
    # print(final.centroid)
    
    cluster.matrix <- points.to.centroid(final.centroid,ringnorm.data.nolabels)
    cluster.matrix.pca <- points.to.centroid(final.centroid.pca,ringnorm.data.nolabels.pca)
    
    compare.ringnorm.noPCA <- cbind(ringnorm.data[,1],cluster.matrix)
    compare.ringnorm.PCA <- cbind(ringnorm.data[,1],cluster.matrix.pca)
    
    good.bad.noPCA.temp <- calculate.good.bad.ringnorm(compare.ringnorm.noPCA,j,i)
    good.bad.PCA.temp <- calculate.good.bad.ringnorm(compare.ringnorm.PCA,j,i)
    
    good.bad.noPCA <- rbind(good.bad.noPCA,good.bad.noPCA.temp)
    good.bad.PCA <- rbind(good.bad.PCA,good.bad.PCA.temp)
    
  }
}

good.bad.noPCA <- na.omit(good.bad.noPCA)
good.bad.PCA <- na.omit(good.bad.PCA)


# Initializing the centroids

Error.rate.noPCA <- good.bad.noPCA[,3]/rowSums(good.bad.noPCA[,3:4])
Error.rate.PCA <- good.bad.PCA[,3]/rowSums(good.bad.PCA[,3:4])

print(Error.rate.noPCA)
print(Error.rate.PCA)