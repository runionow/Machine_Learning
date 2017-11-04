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
library(pdist)
library(ggplot2)

ionosphere.data <- read.table(file="ionosphere.data.txt", sep=",")
#head(ionosphere.data)
#set.seed(100)

# Summary of the ringnorm_data.csv
#summary(ionosphere.data)

# Removing last column(classification) which tells whether data is good or not.
ionosphere.data.nolabels  <- as.matrix.data.frame(ionosphere.data[,1:ncol(ionosphere.data)-1])

pc.ionosphere <- princomp(ionosphere.data.nolabels, scores = TRUE)
summary(pc.ionosphere)


# Keeping 90% variance
ionosphere.data.nolabels.pca  <- as.matrix.data.frame(pc.ionosphere$scores[,1:18])

k.mean.count <- 1


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
# 1. K [integer] - Number of Clusters.
# 2. data.sets[data.frame] - Input dataset.
# 3. initialize.centroids [data.frame] - Initial Centroids to begin K Means.
# 4. threshold [integer] - Set the threshold.
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
    temp <- filter(as.data.frame(cluster.matrix),kk == get("i"))
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
    kmeans.convergence <- kmeans.convergence +  sqrt(t(sub.centroids[,i]) %*% as.matrix(sub.centroids[,i]))/2
  }
  
  if(kmeans.convergence<threshold){
    master.centroids <- na.omit(master.centroids)
    #return
    new.centroids
  }
  else{
    k.mean.count <- k.mean.count+1
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
# NAME: calculate.good.bad(cluster.matrix,k,j)
# DESCRIPTION: Calculate good bad.
# ARGUMENTS: 
# 1. cluster.matrix [data.frame] - Data Set.
# 2. k [integer] - Number of clusters.
# 3. j [integer] - Iteration.
# RETURNS: returns [data.frame] good bad.
# ==============================================================
calculate.good.bad <- function (cluster.matrix,k,j){
  good.bad <- matrix(,ncol =4,nrow = k)
  
  colnames(cluster.matrix)[1] <- c("cluster")
  
  colnames(cluster.matrix)[ncol(cluster.matrix)] <- c("label")
  
  for(i in 1:k){
    good <- nrow(filter(cluster.matrix,cluster == get("i") & label == "g"))
    bad <- nrow(filter(cluster.matrix,cluster == get("i") & label == "b"))
    good.bad <- rbind(good.bad,c(j,k,good,bad))
  }
  good.bad <- na.omit(good.bad)
  
  #return
  good.bad
}

# ==============================================================
# NAME: plot.graph (final.dataset)
# DESCRIPTION: For plotting the graph.
# ARGUMENTS: 
# 1. final.dataset [data.frame] - Input dataset.
# RETURNS: returns a beautiful graph.
# ==============================================================
plot.graph <- function (final.dataset){
  #plotting the final dataset after iterations.
  p <- ggplot(final.dataset, aes(y=value, x=specie, color=specie, fill=specie)) + 
    geom_bar( stat="identity") +    
    facet_wrap(~condition)
  print(p)
}

# Number of Clusters
# k <- 5

# Setting up the threshold.
kmeans.threshold <- 10

ionosphere.data.nolabels <- as.data.frame(ionosphere.data.nolabels)
ionosphere.data.nolables.pca <- as.data.frame(ionosphere.data.nolabels.pca)

good.bad <- matrix(,ncol =4)
good.bad.pca <- matrix(,ncol =4)

for (j in 1:20){
  for(i in 2:5){
    
    # Initializing the centroids
    initialize.centroids <- random.centroids(i,ionosphere.data.nolabels)
    initialize.centroids.pca <- random.centroids(i,ionosphere.data.nolabels.pca)
    
    #Calculate centroids
    final.centroid <- k.means(i,ionosphere.data.nolabels,initialize.centroids,kmeans.threshold)
    
    #Calculate centroids
    final.centroid.pca <- k.means(i,ionosphere.data.nolabels.pca,initialize.centroids.pca,kmeans.threshold)
    
    
    # The Good Bad Story.
    cluster.matrix <- as.data.frame(cbind(points.to.centroid(final.centroid,ionosphere.data.nolabels),ionosphere.data[,ncol(ionosphere.data)]))
    
    # The Good Bad Story.
    cluster.matrix.pca <- as.data.frame(cbind(points.to.centroid(final.centroid.pca,ionosphere.data.nolabels),ionosphere.data[,ncol(ionosphere.data)]))
    
    # Calculating good bad matrices.
    good.bad.temp <- calculate.good.bad(cluster.matrix,i,j)
    good.bad <- rbind(good.bad,good.bad.temp)
    
    # Calculating good bad matrices pca.
    good.bad.temp.pca <- calculate.good.bad(cluster.matrix.pca,i,j)
    good.bad.pca <- rbind(good.bad.pca,good.bad.temp.pca)
  }
}

good.bad <- na.omit(good.bad)
good.bad <- as.data.frame(good.bad)

good.bad.pca <- na.omit(good.bad.pca)
good.bad.pca <- as.data.frame(good.bad.pca)

error.rate.data <- matrix(,ncol=4)
for(j in 1:20){
  for(i in 2:5){
    temp <- filter(good.bad,V1 == get("j") & V2 == get("i"))
    error.rate <- temp[,4]/rowSums(temp[,3:4])
    error.rate.total <- colSums(as.matrix(error.rate))
    error.rate.data <- rbind(error.rate.data,c(j,i,error.rate.total))
  }
}

error.rate.data.pca <- matrix(,ncol=4)
for(j in 1:20){
  for(i in 2:5){
    temp <- filter(good.bad.pca,V1 == get("j") & V2 == get("i"))
    error.rate.pca <- temp[,4]/rowSums(temp[,3:4])
    error.rate.total.pca <- colSums(as.matrix(error.rate.pca))
    error.rate.data.pca <- rbind(error.rate.data.pca,c(j,i,error.rate.total.pca))
  }
}


boxplot(V2 ~ V1, data = good.bad,
        xlab = "Iteration Count", ylab = "Number of Clusters",
        main = "whisker plot for ringnorm"
)

error.rate.data <- na.omit(error.rate.data)
error.rate.data <- as.data.frame(error.rate.data[,1:ncol(error.rate.data)-1])

ggplot(error.rate.data, aes(y=V3, x=V2, color=V2, fill=V2)) + 
  geom_point( stat="identity") + geom_line()+
  facet_wrap(~V1)+
  ggtitle("k-means error rate plot for 1-5 nodes [20 Iterations]") +
  xlab("Clusters - k") +
  ylab("Error Rate") 
  



