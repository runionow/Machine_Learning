# @Author : Arun Nekkalapudi.


# ==============================================================
#Environment Preperation.
#-----------------------

# 1. Set the working directory to the path where all data files are available.
# setwd("C:\\Users\\arunt\\Desktop\\AML\\Assignment 1\\data")

# 2. Install below mentioned packages.
#install.packages("pdist")
#install.packages("dplyr")
#install.packages("ggplot2")
# ==============================================================

library(dplyr)
library(pdist)
library(ggplot2)

# WARNING : Only for 2 dimensional data of format [x,y,k].
# ==============================================================
# NAME: random.centroids(k,data.set)
# DESCRIPTION: Function to get k number of random centroids.
# ARGUMENTS: 
# 1. K [integer] - Number of Clusters.
# 2. data.set [data.frame] - Input dataset.
# RETURNS: returns a dataset consisting of information about clusters.
# ==============================================================
final.clusters <- function (small.dataset,k){
  
  #Empty Matrix
  centroids <- matrix(,ncol = ncol(small.dataset))
  
  #Keeping a copy of old k data.
  old.k <- as.vector(small.dataset$k)
  
  # b. Computing centroid for each cluster.
  for(i in 1:k){
    temp <- filter(small.dataset,k==get("i"))
    mean.centroid <- colMeans(temp)
    centroids <- rbind(centroids,mean.centroid)
  }
  
  #typecasting to a data frame.
  centroids <- as.data.frame(centroids)
  
  #Removing empty rows in a dataframe.
  centroids.k <- centroids[!apply(is.na(centroids) | centroids == "", 1, all),]
  
  #Assigning colNames
  colnames(centroids.k) <- c("x","y","k")
  centroids.only <- select(centroids.k,x,y)
  print(centroids.only)
  
  # d. Assign each observation to the nearest centroid. 
  dists <- as.matrix(pdist(small.dataset[,1:ncol(small.dataset)-1],centroids.only)) 
  print(dists)
  new.k <- as.vector(cbind(apply(dists,1,which.min))) 
  
  #Updating with new clusters
  small.dataset$k = new.k
  
  #e. Repeat (c) and (d) until it stops changing.
  if(all(new.k == old.k)){
    #return Result
    small.dataset
  }
  else{
    #recursion
    final.clusters(small.dataset,k)
  }
}

# 
# e. color the observations based on the clusters obtained.
# ==============================================================
# NAME: plot.graph (final.dataset)
# DESCRIPTION: For plotting the graph.
# ARGUMENTS: 
# 1. final.dataset [data.frame] - Input dataset.
# RETURNS: returns a beautiful graph.
# ==============================================================
plot.graph <- function (final.dataset){
  #plotting the final dataset after iterations.
  p <- ggplot(data=final.dataset,aes(x,y,colour=factor(k))) + 
    geom_point(size=3,aes(shape = factor(k)))+
    ggtitle("Q1. K Means Clustering")+
    xlab("X")+
    ylab("Y")
  print(p)
}
#MyData <- read.csv(file="c:/TheDataIWantToReadIn.csv", header=TRUE, sep=",")
set.seed(100)

# Given dataset in Assignment [pg. 414 Exercise 3].
data.x <- c(1,1,0,5,6,4)
data.y <- c(4,3,4,1,2,0)

# a. Plot the observations.
# p <- ggplot(data=final.dataset,aes(x,y)) + 
#   geom_point(size=3)+
#   ggtitle("Q1. K Means Clustering")+
#   xlab("X")+
#   ylab("Y")
# print(p)

# Set the number of clusters in here.
k <- 3

small.dataset = data.frame(data.x,data.y)

# Assigning Random cluster values to each observation.
small.dataset <- cbind(small.dataset,sample(1:k,length(data.x),replace = TRUE))
colnames(small.dataset) <- c("x","y","k")

print("Initialization dataset")
print(small.dataset)

final.dataset <- final.clusters(small.dataset,k)
plot.graph(final.dataset)
