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

ionosphere.data <- read.csv(file="I50.csv", header=F)
#ionosphere.data <- read.table(file="ionosphere.data.txt", sep=",")
#head(ionosphere.data)
#set.seed(1000)


# Removing last column(classification) which tells whether data is good or not.
ionosphere.data.nolabels  <- as.matrix.data.frame(ionosphere.data[,1:ncol(ionosphere.data)-1])


# ==============================================================
# NAME: calculate.good.bad(cluster.matrix,k)
# DESCRIPTION: Calculate good bad.
# ARGUMENTS: 
# 1. cluster.matrix [data.frame] - Data Set.
# 2. k [integer] - Number of clusters.
# RETURNS: returns [data.frame] good bad.
# ==============================================================
calculate.good.bad <- function (cluster.matrix,k){
  good.bad <- matrix(,ncol =4,nrow = k)
  
  colnames(cluster.matrix)[1] <- c("cluster")
  
  colnames(cluster.matrix)[ncol(cluster.matrix)] <- c("label")
  
  for(i in 1:k){
    good <- nrow(filter(cluster.matrix,cluster == get("i") & label == "g"))
    bad <- nrow(filter(cluster.matrix,cluster == get("i") & label == "b"))
    good.bad <- rbind(good.bad,c(k,i,good,bad))
  }
  good.bad <- na.omit(good.bad)
  
  colnames(good.bad) <- c("k","clusterNo","good","bad")
  #return
  good.bad
}

I50.Ionosphere.labels <- ionosphere.data
I50.Ionosphere.nolabels <- as.matrix.data.frame(I50.Ionosphere.labels[,1:ncol(I50.Ionosphere.labels)-1])

pca.I50 <- princomp(I50.Ionosphere.nolabels)
summary(pca.I50)

print(pca.I50)
summary(pca.I50)

# For upto 90 percent variance consider first 9 columns in the I50 data.
I50.Ionosphere.nolabels.pca <- pca.I50$scores[,1:13]

# Calculating disimmilarity matrix PCA 
I50.hclust.dissim.pca <- dist(I50.Ionosphere.nolabels.pca, method = "euclidean", diag = FALSE, upper = TRUE)

# Calculating disimmilarity matrix
I50.hclust.dissim <- dist(I50.Ionosphere.nolabels, method = "euclidean", diag = FALSE, upper = TRUE)

# Plotting the I50 data w/o pca.
hclust.complete <- hclust(I50.hclust.dissim, method = "complete", members = NULL)
plot(hclust.complete,main="Dendogram (Complete Linkage) for the Ionosphere I50",xlab="",hang = -1)
I50.Ionosphere.labels.clustering <- cbind(cutree(hclust.complete,h = 8),I50.Ionosphere.labels)

# Plotting the I50 data w pca.
hclust.complete.pca <- hclust(I50.hclust.dissim.pca, method = "complete", members = NULL)
plot(hclust.complete.pca,main="Dendogram (Complete Linkage) for the Ionosphere I50 with PCA",xlab="",hang = -1)

# We can cut with height
I50.Ionosphere.labels.clustering.pca <- cbind(cutree(hclust.complete.pca,h = 8),I50.Ionosphere.labels)


good.bad.no.pca <- calculate.good.bad(I50.Ionosphere.labels.clustering,2)
good.bad.with.pca <- calculate.good.bad(I50.Ionosphere.labels.clustering.pca,2)

error.rate.pca <- good.bad.with.pca[,4]/rowSums(good.bad.with.pca[,3:4])
error.rate <- good.bad.no.pca[,4]/rowSums(good.bad.no.pca[,3:4])

# Error rate with pca.
print(sum(error.rate.pca))

#Error rate without pca.
print(sum(error.rate))

# error.rates <- random [,3]/as.matrix(rowSums(random[,2:3]),col=1)
# error.rates.sum <- colSums(as.matrix(error.rate))
