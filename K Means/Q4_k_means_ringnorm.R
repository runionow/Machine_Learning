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

ringnorm.data <- read.csv(file="ringnorm_data.csv", header=F)
head(ringnorm.data)
#set.seed(100)

#Summary of the ringnorm_data.csv
#summary(ringnorm.data)

# Removing 1st column.
# After Watching the data it looks like a label which has only 2 unique values 1 and -1
ringnorm.data.nolabels  <- as.data.frame(ringnorm.data[,2:ncol(ringnorm.data)])
summary(ringnorm.data.nolabels)

#Initializing master centroid data.
master.centroids <- matrix(,ncol = ncol(ringnorm.data.nolabels))

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
  
  # Calculating Distance from Centroid to all the points.
  dists <- as.matrix(pdist(data.sets,initialize.centroids))
  
  #Seperating the points and allocating them to the correspoding centroids.
  sep.matrix <- cbind(apply(dists,1,which.min))
  
  # Converting unique values to vector.
  unique.values <- sort(unique(sep.matrix))
  
  # the first matrix after randomly initilizing centroids.
  cluster.matrix <- cbind(sep.matrix,data.sets)
  
  #Empty Matrix
  new.centroids <- matrix(,ncol = (ncol(cluster.matrix)-1))
  
  # Renamin the first column to k1 
  colnames(cluster.matrix) <- c("kk",letters[1:(length(data.sets))])
  
  
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
    kmeans.convergence <- kmeans.convergence +  sqrt(t(sub.centroids[,i]) %*% as.matrix(sub.centroids[,i]))/2
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
k <- 5

# Initializing the centroids
initialize.centroids <- random.centroids(k,ringnorm.data.nolabels)

# Setting up the threshold.
kmeans.threshold <- 10

ringnorm.data.nolabels <- as.data.frame(ringnorm.data.nolabels)

# The Ultimate Showdown.
final.centroid <- k.means(k,ringnorm.data.nolabels,initialize.centroids,kmeans.threshold)

print(final.centroid)