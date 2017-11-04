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


# ==============================================================
# NAME: plot.elbow.graph(data.set,graph.heading)
# DESCRIPTION: Function to get k number of random centroids.
# ARGUMENTS: 
# 1. data.set [data.frame] - Input dataset.
# 2. graph.heading [string] - heading.
# RETURNS: returns k number of random rows from the dataset.
# ==============================================================
plot.elbow.graph <- function(data.set,graph.heading){
  p <- ggplot(data.set, aes(y=V2, x=V1, color="red")) + 
    geom_point() + geom_line()+
    ggtitle(print(paste0("K-Means Elbow Method - ",graph.heading))) +
    scale_x_continuous(breaks = c(1:28))+
    ylab("Total within Sum of Square") +
    xlab("Number of Clusters")
  print(p)
}

# Initializing ringnorm data.
ringnorm.data <- read.csv(file="ringnorm_data.csv", header=F)
ringnorm.data.nolabels  <- as.data.frame(ringnorm.data[,2:ncol(ringnorm.data)])

# Initializing Ionosphere data.
ionosphere.data <- read.table(file="ionosphere.data.txt", sep=",")
# Removing last column(classification) which tells whether data is good or not.
ionosphere.data.nolabels  <- as.data.frame(ionosphere.data[,1:ncol(ionosphere.data)-1])

plot.data.ringnorm <- matrix(,ncol=2)
plot.data.ionosphere <- matrix(,ncol=2)

# Calculating total within Sum of squares for Ringnorm data.
for(k in 1:20){
  SSE <- kmeans(ringnorm.data.nolabels,k, nstart = 25,iter.max = 100, algorithm = c("Lloyd"), trace=FALSE)
  plot.data.ringnorm <- rbind(plot.data.ringnorm,c(k,SSE$tot.withinss))
}


# Calculating total within Sum of squares for Ionosphere data.
for(k in 1:20){
  SSE <- kmeans(ionosphere.data.nolabels,k, nstart = 25,iter.max = 100, algorithm = c("Lloyd"), trace=FALSE)
  plot.data.ionosphere <- rbind(plot.data.ionosphere,c(k,SSE$tot.withinss))
}

#Ploting an elbow graph for ringnorm data.
plot.data.ringnorm <- as.data.frame(na.omit(plot.data.ringnorm))
plot.elbow.graph(plot.data.ringnorm,"Ringnorm")

#Ploting an elbow graph for Ionosphere data.
plot.data.ionosphere <- as.data.frame(na.omit(plot.data.ionosphere))
plot.elbow.graph(plot.data.ionosphere,"Ionosphere")


