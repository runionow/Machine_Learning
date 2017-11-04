# @Author : Arun Nekkalapudi.


# ==============================================================
# Environment Preperation.
# -----------------------

# 1. Set the working directory to the path where all data files are available.
# setwd("C:\\Users\\arunt\\Desktop\\AML\\Assignment 2\\data")

# 2. Install below mentioned packages.
#install.packages("pdist")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("mvtnorm")
# ==============================================================

library(dplyr)
library(pdist)
library(ggplot2)
library(mvtnorm)



# Number of Clusters.
k <- 3

#Initialize 
t <- 0 

# Set threshold
threshold <- 10

# Load CSV.
ringnorm.data <- read.csv(file="ringnorm_data.csv", header=F)
head(ringnorm.data)

ionosphere.data <- read.table(file="ionosphere.data.txt", sep=",")

# Removing 1st column.
# After Watching the data it looks like a label which has only 2 unique values 1 and -1
ringnorm.data.nolabels  <- ringnorm.data[,2:ncol(ringnorm.data)]



# Removing last column(classification) which tells whether data is good or not.
ionosphere.data.nolabels  <- as.matrix.data.frame(ionosphere.data[,1:ncol(ionosphere.data)-1])

pc.ringworm <- princomp(ringnorm.data.nolabels, scores = TRUE)
pc.ionosphere <- princomp(ionosphere.data.nolabels, scores = TRUE)

prcomp.ringworm <-prcomp(ringnorm.data.nolabels)
prcomp.ionosphere <-prcomp(ionosphere.data.nolabels)



summary(pc.ringworm)
summary(pc.ionosphere)

print(pc.ringworm$loadings)
print(pc.ionosphere$loadings)

biplot(pc.ringworm ,cex = 0.5, main ="Bi-plot for RingNorm data")
biplot(pc.ionosphere ,cex = 0.5,  main ="Bi-plot for Ionosphere data")

plot(pc.ringworm, main= "Principal Component Analysis on Ringworm Data" )
plot(pc.ionosphere, main= "Principal Component Analysis on Ionosphere Data" )

 



