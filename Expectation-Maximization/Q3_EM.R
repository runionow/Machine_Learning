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
#install.packages("mvtnorm")
#install.packages("stats")
#install.packages("pracma")
# ==============================================================

library(dplyr)
library(ggplot2)
library(mvtnorm)
library(stats)
library(pracma)
library(pdist)
set.seed(1001)

# Number of Clusters.
k <- 2

#Initialize 
t <- 0 

# Set threshold
threshold <- 0.005

EM_clustering.noupdate <- function(k,data.set,mean.onrun,sigma.onrun,threshold,prob.onrun.cluster){
  w  <- matrix(,nrow = nrow(data.set),ncol = k)
  repeat{
    t <- t+1
    
    #Step 1. Expectation
    mvn.denom <- 0
    for(ki in 1:k){
      mvn.denom <- mvn.denom + dmvnorm(x=as.matrix(data.set),mean = as.double(mean.onrun[ki,]),sigma = as.matrix(sigma.onrun[,,ki]))*prob.onrun.cluster[ki]
    }
    
    #w <- matrix(,nrow = nrow(data.set),ncol = k)
    for(i in 1:k){
      mvn.numer<- dmvnorm(x=as.matrix(data.set),mean = as.double(mean.onrun[i,]),sigma = as.matrix(sigma.onrun[,,i]))*prob.onrun.cluster[i]
      w[,i] <- mvn.numer/mvn.denom
    }
    
    
    # Step 2. Maximization. 
    mean.onrun.prev <- mean.onrun
    for(i in 1:k){
      mean.onrun[i,] <- ((as.matrix(t(w[,i])))%*%(as.matrix(data.set)))/(as.double(sum(w[,i])))
      #print(w)
      #sigma.onrun[,,i] <- cov.wt(x = data.set,wt=as.double(w[,i]),center = as.double(mean.onrun[i,]), method = "ML", cor = TRUE)$cov
      if(any(diag(sigma.onrun[,,i]) == 0)){
        diag(sigma.onrun[,,i]) <- diag(sigma.onrun[,,i])+0.000002
      }
    }
    
    #prob.onrun.cluster <- colMeans(w)
    
    # Test the output 
    # print(mean.onrun.prev)
    # print(mean.onrun)
    # print(prob.onrun.cluster)
    # print(sigma.onrun)
    
    # Convergence Condition [check the threshold if not rerun the K-Means function with new centroids]
    sub.means <- t(as.matrix(mean.onrun)) - t(as.matrix(mean.onrun.prev))
    EM.convergence <-c(0)
    for(i in 1:ncol(sub.means)){
      EM.convergence <- EM.convergence +  sqrt(t(sub.means[,i]) %*% as.matrix(sub.means[,i]))
    }
    
    # Squaring as mentioned in the Algorithm.
    EM.convergence <- EM.convergence^2
    
    if(EM.convergence <= threshold){
      #print(paste0("Convergence Rate broken at : ",t))
      #print(paste0("Convergence Rate : ",EM.convergence))
      break
    }
  }
  weight <- w
  iteration <- t
  list("w" = weight, "t" = iteration)
}
#== RINGWORM DATA =====================================================================
# Load CSV.
ringnorm.data <- read.csv(file="ringnorm_data.csv", header=F)
head(ringnorm.data)

# Removing 1st column.
# After Watching the data it looks like a label which has only 2 unique values 1 and -1
ringnorm.data.nolabels  <- ringnorm.data[,2:ncol(ringnorm.data)]
summary(ringnorm.data.nolabels)

pc.ringworm <- princomp(ringnorm.data.nolabels, scores = TRUE)
summary(pc.ringworm)


# Keeping 90% variance
ringnorm.data.nolabels.pca  <- as.matrix.data.frame(pc.ringworm$scores[,1:19])



#== IONOSPHERE DATA =====================================================================
# Load CSV.
ionosphere.data <- read.table(file="ionosphere.data.txt",sep=",")
head(ionosphere.data)

# Removing 1st column.
# After Watching the data it looks like a label which has only 2 unique values 1 and -1
ionosphere.data.nolabels  <- ionosphere.data[,1:ncol(ionosphere.data)-1]
summary(ionosphere.data.nolabels)


pc.ionosphere <- princomp(ionosphere.data.nolabels, scores = TRUE)
summary(pc.ionosphere)


# Keeping 90% variance
ionosphere.data.nolabels.pca  <- as.matrix.data.frame(pc.ionosphere$scores[,1:19])


# ======F:random.centroids========================================
# NAME: random.centroids(k,data.set)
# DESCRIPTION: Function to get k number of random centroids.
# ARGUMENTS: 
# 1. K [integer] - Number of Clusters.
# 2. data.set [data.frame] - Input dataset.
# RETURNS: returns k number of random rows from the dataset.

random.means <- function(k,data.set){
  data.set[sample(nrow(data.set),k,replace = FALSE),]
}

# ======F:points.to.centroid======================================
# NAME: points.to.centroid(centroid.set,data.set)
# DESCRIPTION: points to cluster mapper.
# ARGUMENTS: 
# 1. centroid.set [data.frame] - Centroid data.
# 2. data.set [data.frame] - Input dataset.
# RETURNS: returns a [data.frame] points belong to a cluster.

points.to.centroid <- function(centroid.set,data.set){
  # Calculating Distance from Centroid to all the points.
  dists <- as.matrix(pdist(data.set,centroid.set))
  
  #Seperating the points and allocating them to the correspoding centroids.
  sep.matrix <- cbind(apply(dists,1,which.min))
  
  #the first matrix after randomly initilizing centroids.
  cluster.matrix <- cbind(sep.matrix,data.set)
  
  #return
  cluster.matrix
}

# ======F:EM_clustering===========================================
# NAME: EM_clustering(data.set,mean.onrun,sigma.onrun,threshold,prob.onrun.cluster)
# DESCRIPTION: Clustering based on Gausian mixture model.
# ARGUMENTS: 
# 1. mean.onrun [vector] - means.
# 2. data.set [data.frame] - Input dataset.
# 3. sigma.onrun [multi-dimnensional array] - co-variance matrix.
# 4. threshold [numeric] - threshold condition for stopping.
# 5. prob.onrun.clusters [vector] - initializing probabilities.
# 6. k [numeric] - Number of Gaussian Distributions.
# RETURNS: returns mostelikelihood of the points.

EM_clustering <- function(k,data.set,mean.onrun,sigma.onrun,threshold,prob.onrun.cluster){
  w  <- matrix(,nrow = nrow(data.set),ncol = k)
  repeat{
    t <- t+1
    
    #Step 1. Expectation
    mvn.denom <- 0
    for(ki in 1:k){
      mvn.denom <- mvn.denom + dmvnorm(x=as.matrix(data.set),mean = as.double(mean.onrun[ki,]),sigma = as.matrix(sigma.onrun[,,ki]))*prob.onrun.cluster[ki]
    }
    
    #w <- matrix(,nrow = nrow(data.set),ncol = k)
    for(i in 1:k){
        mvn.numer<- dmvnorm(x=as.matrix(data.set),mean = as.double(mean.onrun[i,]),sigma = as.matrix(sigma.onrun[,,i]))*prob.onrun.cluster[i]
        w[,i] <- mvn.numer/mvn.denom
    }

    
    # Step 2. Maximization. 
    mean.onrun.prev <- mean.onrun
    for(i in 1:k){
      mean.onrun[i,] <- ((as.matrix(t(w[,i])))%*%(as.matrix(data.set)))/(as.double(sum(w[,i])))
      #print(w)
      sigma.onrun[,,i] <- cov.wt(x = data.set,wt=as.double(w[,i]),center = as.double(mean.onrun[i,]), method = "ML", cor = TRUE)$cov
      if(any(diag(sigma.onrun[,,i]) == 0)){
        diag(sigma.onrun[,,i]) <- diag(sigma.onrun[,,i])+0.000002
      }
    }
    
    prob.onrun.cluster <- colMeans(w)
    
    # Test the output 
    # print(mean.onrun.prev)
    # print(mean.onrun)
    # print(prob.onrun.cluster)
    # print(sigma.onrun)
    
    # Convergence Condition [check the threshold if not rerun the K-Means function with new centroids]
    sub.means <- t(as.matrix(mean.onrun)) - t(as.matrix(mean.onrun.prev))
    EM.convergence <-c(0)
    for(i in 1:ncol(sub.means)){
      EM.convergence <- EM.convergence +  sqrt(t(sub.means[,i]) %*% as.matrix(sub.means[,i]))
    }
    
    # Squaring as mentioned in the Algorithm.
    EM.convergence <- EM.convergence^2
    
    if(EM.convergence <= threshold){
      #print(paste0("Convergence Rate broken at : ",t))
      #print(paste0("Convergence Rate : ",EM.convergence))
      break
    }
  }
  weight <- w
  iteration <- t
  list("w" = weight, "t" = iteration)
}

# ======F:k.mean=================================================
# NAME: k.means(k,data.sets,initialize.centroids,threshold)
# DESCRIPTION: Lloyd K Means Algorithm.
# ARGUMENTS: 
# 1. K[integer] - Number of Clusters.
# 2. data.sets[data.frame] - Input dataset.
# 3. initialize.centroids[data.frame] - Initial Centroids to begin K Means.
# 4. threshold[integer] - Set the threshold.
# RETURNS: returns k number of random rows from the dataset.

k.means <- function(k,data.sets,initialize.centroids,threshold,iteration=0){
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
    list("c"=new.centroids,"i"=iteration)
  }
  else{
    iteration <- iteration + 1
    k.means(k,data.sets,new.centroids,threshold,iteration)
  }
  
}

# ======F:calculate.good.bad.ionosphere==========================
# NAME: calculate.good.bad.ionosphere(cluster.matrix,k,j)
# DESCRIPTION: Calculate good bad.
# ARGUMENTS: 
# 1. cluster.matrix [data.frame] - Data Set.
# 2. k [integer] - Number of clusters.
# 3. j [integer] - Iteration.
# RETURNS: returns [data.frame] good bad.

calculate.good.bad.ionosphere <- function (cluster.matrix,k,j,t){
  good.bad <- matrix(,ncol =5,nrow = k)
  
  colnames(cluster.matrix)[1] <- c("cluster")
  
  colnames(cluster.matrix)[ncol(cluster.matrix)] <- c("label")
  
  for(i in 1:k){
    good <- nrow(filter(cluster.matrix,cluster == get("i") & label == "g"))
    bad <- nrow(filter(cluster.matrix,cluster == get("i") & label == "b"))
    good.bad <- rbind(good.bad,c(j,k,good,bad,t))
  }
  good.bad <- na.omit(good.bad)
  
  #return
  good.bad
}

# ======F:calculate.good.bad.ringnorm=============================
# NAME: calculate.good.bad.ringnorm(cluster.matrix,k,j)
# DESCRIPTION: Calculate good bad.
# ARGUMENTS: 
# 1. cluster.matrix [data.frame] - Data Set.
# 2. k [integer] - Number of clusters.
# 3. j [integer] - Iteration.
# RETURNS: returns [data.frame] good bad.

calculate.good.bad.ringnorm <- function (cluster.matrix,k,j,iteration){
  good.bad <- matrix(,ncol =5,nrow = k)
  
  colnames(cluster.matrix)[1] <- c("cluster")
  
  colnames(cluster.matrix)[2] <- c("label")
  
  for(i in 1:k){
    good <- nrow(filter(cluster.matrix,cluster == get("i") & label == 1))
    bad <- nrow(filter(cluster.matrix,cluster == get("i") & label == -1))
    good.bad <- rbind(good.bad,c(j,k,good,bad,t))
  }
  good.bad <- na.omit(good.bad)
  
  #return
  good.bad
}

calculate.good.bad.ringnorm.kmeans <- function (cluster.matrix,k,j,iteration){
  good.bad <- matrix(,ncol =5,nrow = k)
  
  colnames(cluster.matrix)[1] <- c("label")
  
  colnames(cluster.matrix)[2] <- c("cluster")
  
  for(i in 1:k){
    good <- nrow(filter(cluster.matrix,cluster == get("i") & label == 1))
    bad <- nrow(filter(cluster.matrix,cluster == get("i") & label == -1))
    good.bad <- rbind(good.bad,c(j,k,good,bad,t))
  }
  good.bad <- na.omit(good.bad)
  
  #return
  good.bad
}


# ======MAIN:Calling Code

# Assigning probabilities for each cluster.
prob.init.cluster <- rep(1/k,k) 

# likelihood.matrix.ionosphere <- EM_clustering(k,ionosphere.data.nolabels,mean.onrun.ionosphere,sigma.onrun.ionosphere,threshold,prob.onrun.cluster)
# likelihood.matrix.ionosphere.pca <- EM_clustering(k,ionosphere.data.nolabels.pca,mean.onrun.ionosphere.pca,sigma.onrun.ionosphere.pca,threshold,prob.onrun.cluster)
good.bad <- matrix(,ncol =4)
good.bad.pca <- matrix(,ncol =4)

# Everything

kmeans.good.bad.ionosphere <- matrix(,ncol=5)
kmeans.good.bad.ringnorm <- matrix(,ncol=5)
kmeans.good.bad.ionosphere.pca <- matrix(,ncol=5)
kmeans.good.bad.ringnorm.pca <- matrix(,ncol=5)

EM.good.bad.ringnorm <- matrix(,ncol=5)
EM.good.bad.ionosphere <- matrix(,ncol=5)
EM.good.bad.ringnorm.pca <- matrix(,ncol=5)
EM.good.bad.ionosphere.pca <- matrix(,ncol=5)


for (j in 1:20){
  for(i in 2){
    
    # EM initialization probabilities
    posterior.prob.matrix <- matrix(,ncol = (i))
    prob.init.cluster <- rep(1/i,i) 
    prob.onrun.cluster <- prob.init.cluster

    # Initializing the centroids
    EM.init.means.ringnorm <- random.means(i,ringnorm.data.nolabels)
    EM.init.means.ionosphere <- random.means(i,ionosphere.data.nolabels)
    
    EM.init.means.ringnorm.pca <- random.means(i,ringnorm.data.nolabels.pca)
    EM.init.means.ionosphere.pca <- random.means(i,ionosphere.data.nolabels.pca)
    
    mean.onrun.ringnorm <- as.vector(EM.init.means.ringnorm)
    mean.onrun.ionosphere <- as.vector(EM.init.means.ionosphere)
    
    mean.onrun.ringnorm.pca <- as.vector(EM.init.means.ringnorm.pca)
    mean.onrun.ionosphere.pca <- as.vector(EM.init.means.ionosphere.pca)
    
    # Initializing Sigmoids
    sigma.init.ringnorm <- array(,c(ncol(ringnorm.data.nolabels),ncol(ringnorm.data.nolabels),i))
    sigma.init.ionosphere <- array(,c(ncol(ionosphere.data.nolabels),ncol(ionosphere.data.nolabels),i))
    
    sigma.init.ringnorm.pca <- array(,c(ncol(ringnorm.data.nolabels.pca),ncol(ringnorm.data.nolabels.pca),i))
    sigma.init.ionosphere.pca <- array(,c(ncol(ionosphere.data.nolabels.pca),ncol(ionosphere.data.nolabels.pca),i))
    
    for (k in 1:i){
      sigma.init.ringnorm[, , k] <- diag(x=1,ncol(ringnorm.data.nolabels))
      sigma.init.ionosphere[, , k] <- diag(x=1,ncol(ionosphere.data.nolabels))
      sigma.init.ringnorm.pca[, , k] <- diag(x=1,ncol(ringnorm.data.nolabels.pca))
      sigma.init.ionosphere.pca[, , k] <- diag(x=1,ncol(ionosphere.data.nolabels.pca))
    }
    
    sigma.onrun.ringnorm <- sigma.init.ringnorm
    sigma.onrun.ionosphere <- sigma.init.ionosphere
    
    sigma.onrun.ringnorm.pca <- sigma.init.ringnorm.pca
    sigma.onrun.ionosphere.pca <- sigma.init.ionosphere.pca

    # K MEANS - Ionosphere
    final.centroid.ionosphere <- k.means(i,ionosphere.data.nolabels,EM.init.means.ionosphere,threshold)
    final.centroid.ionosphere.pca <- k.means(i,ionosphere.data.nolabels.pca,EM.init.means.ionosphere.pca,threshold)
    
    # K MEANS - Ringnorm
    final.centroid.ringnorm <- k.means(i,ringnorm.data.nolabels,EM.init.means.ringnorm,threshold)
    final.centroid.ringnorm.pca <- k.means(i,ringnorm.data.nolabels.pca,EM.init.means.ringnorm.pca,threshold)
    
    # K MEANS - Analysis
    
    cluster.matrix.ionosphere <- as.data.frame(cbind(points.to.centroid(final.centroid.ionosphere$c,ionosphere.data.nolabels),ionosphere.data[,ncol(ionosphere.data)]))
    cluster.matrix.ionosphere.pca <- as.data.frame(cbind(points.to.centroid(final.centroid.ionosphere.pca$c,ionosphere.data.nolabels),ionosphere.data[,ncol(ionosphere.data)]))
    
    cluster.matrix.ringworm <- as.data.frame(cbind(ringnorm.data[,1],points.to.centroid(final.centroid.ringnorm$c,ringnorm.data.nolabels)))
    cluster.matrix.ringworm.pca <- as.data.frame(cbind(ringnorm.data[,1],points.to.centroid(final.centroid.ringnorm.pca$c,ringnorm.data.nolabels)))
    
    kmeans.good.bad.temp.ionosphere <- calculate.good.bad.ionosphere(cluster.matrix.ionosphere,i,j,final.centroid.ionosphere$i)
    kmeans.good.bad.ionosphere <- rbind(kmeans.good.bad.ionosphere,kmeans.good.bad.temp.ionosphere)
    
    kmeans.good.bad.temp.ringnorm <- calculate.good.bad.ringnorm.kmeans(cluster.matrix.ringworm,i,j,cluster.matrix.ringworm$i)
    kmeans.good.bad.ringnorm <- rbind(kmeans.good.bad.ringnorm,kmeans.good.bad.temp.ringnorm)
    
    kmeans.good.bad.temp.ionosphere.pca <- calculate.good.bad.ionosphere(cluster.matrix.ionosphere.pca,i,j,final.centroid.ionosphere.pca$i)
    kmeans.good.bad.ionosphere.pca <- rbind(kmeans.good.bad.ionosphere.pca,kmeans.good.bad.temp.ionosphere.pca)
    
    kmeans.good.bad.temp.ringnorm.pca <-   calculate.good.bad.ringnorm.kmeans(cluster.matrix.ringworm.pca,i,j,cluster.matrix.ringworm.pca$i)
    kmeans.good.bad.ringnorm.pca <- rbind(kmeans.good.bad.ringnorm.pca,kmeans.good.bad.temp.ringnorm.pca)
    

    # EM Clustering - Ionosphere
    likelihood.matrix.ionosphere <- EM_clustering(i,ionosphere.data.nolabels,EM.init.means.ionosphere,sigma.onrun.ionosphere,threshold,prob.onrun.cluster)
    likelihood.matrix.ionosphere.pca <- EM_clustering(i,ionosphere.data.nolabels.pca,EM.init.means.ionosphere.pca,sigma.onrun.ionosphere.pca,threshold,prob.onrun.cluster)
 
    # EM Clustering - Ringnorm
    likelihood.matrix.ringnorm <- EM_clustering(i,ringnorm.data.nolabels,EM.init.means.ringnorm,sigma.onrun.ringnorm,threshold,prob.onrun.cluster)
    likelihood.matrix.ringnorm.pca <- EM_clustering(i,ringnorm.data.nolabels.pca,EM.init.means.ringnorm.pca,sigma.onrun.ringnorm.pca,threshold,prob.onrun.cluster)
    
    # EM analysis
    sep.matrix.ringnorm.EM <- cbind(apply(likelihood.matrix.ringnorm$w,1,which.min))
    sep.matrix.ringnorm.EM.pca <- cbind(apply(likelihood.matrix.ringnorm.pca$w,1,which.min))
    
    sep.matrix.ionosphere.EM <- cbind(apply(likelihood.matrix.ionosphere$w,1,which.min))
    sep.matrix.ionosphere.EM.pca <- cbind(apply(likelihood.matrix.ionosphere.pca$w,1,which.min))
    
    EM.cluster.matrix.ringnorm <- cbind(sep.matrix.ringnorm.EM,ringnorm.data)
    EM.cluster.matrix.ringnorm.pca <- cbind(sep.matrix.ringnorm.EM.pca,ringnorm.data)
    
    EM.cluster.matrix.ionosphere <- cbind(sep.matrix.ionosphere.EM,ionosphere.data)
    EM.cluster.matrix.ionosphere.pca <- cbind(sep.matrix.ionosphere.EM.pca,ionosphere.data)
    
    EM.good.bad.ringnorm.temp <- calculate.good.bad.ringnorm(EM.cluster.matrix.ringnorm,i,j,likelihood.matrix.ringnorm$t)
    EM.good.bad.ringnorm <- rbind(EM.good.bad.ringnorm,EM.good.bad.ringnorm.temp)
    
    EM.good.bad.ringnorm.pca.temp <- calculate.good.bad.ringnorm(EM.cluster.matrix.ringnorm.pca,i,j,likelihood.matrix.ringnorm.pca$t)
    EM.good.bad.ringnorm.pca <-rbind(EM.good.bad.ringnorm.pca,EM.good.bad.ringnorm.pca.temp)
    
    EM.good.bad.ionosphere.temp <- calculate.good.bad.ionosphere(EM.cluster.matrix.ionosphere,i,j,likelihood.matrix.ionosphere$t)
    EM.good.bad.ionosphere <- rbind(EM.good.bad.ionosphere,EM.good.bad.ionosphere.temp)
    
    EM.good.bad.ionosphere.pca.temp <- calculate.good.bad.ionosphere(EM.cluster.matrix.ionosphere.pca,i,j,likelihood.matrix.ionosphere.pca$t)
    EM.good.bad.ionosphere.pca <- rbind(EM.good.bad.ionosphere.pca,EM.good.bad.ionosphere.pca.temp)
  }
}

#likelihood.matrix.ionosphere.noupdate <- EM_clustering.noupdate(i,ionosphere.data.nolabels,EM.init.means.ionosphere,sigma.onrun.ionosphere,threshold,prob.onrun.cluster)

EM.good.bad.ionosphere.pca <- data.frame(na.omit(EM.good.bad.ionosphere.pca))
EM.good.bad.ionosphere <- data.frame(na.omit(EM.good.bad.ionosphere))
EM.good.bad.ringnorm <- data.frame(na.omit(EM.good.bad.ringnorm))
EM.good.bad.ringnorm.pca <- data.frame(na.omit(EM.good.bad.ringnorm.pca))

kmeans.good.bad.ionosphere <- data.frame(na.omit(kmeans.good.bad.ionosphere))
kmeans.good.bad.ringnorm <- data.frame(na.omit(kmeans.good.bad.ringnorm))
kmeans.good.bad.ionosphere.pca <- data.frame(na.omit(kmeans.good.bad.ionosphere.pca))
kmeans.good.bad.ringnorm.pca <- data.frame(na.omit(kmeans.good.bad.ringnorm.pca))



kmeans.ionosphere.pca.error.rate.data <- matrix(,ncol=4)
for(j in 1:20){
  temp <- filter(kmeans.good.bad.ionosphere.pca,X1 == get("j") & X2 == 2)
  error.rate <- temp[,4]/rowSums(temp[,3:4])
  iter.count <- mean(temp$X5)
  error.rate.total <- colSums(as.matrix(error.rate))
  kmeans.ionosphere.pca.error.rate.data <- rbind(kmeans.ionosphere.pca.error.rate.data,c(j,2,error.rate.total,iter.count))
}
kmeans.ionosphere.pca.error.rate.data <- na.omit(kmeans.ionosphere.pca.error.rate.data)


kmeans.ionosphere.error.rate.data <- matrix(,ncol=4)
for(j in 1:20){
  temp <- filter(kmeans.good.bad.ionosphere,X1 == get("j") & X2 == 2)
  error.rate <- temp[,4]/rowSums(temp[,3:4])
  iter.count <- mean(temp$X5)
  error.rate.total <- colSums(as.matrix(error.rate))
  kmeans.ionosphere.error.rate.data <- rbind(kmeans.ionosphere.error.rate.data,c(j,2,error.rate.total,iter.count))
}
kmeans.ionosphere.error.rate.data <- na.omit(kmeans.ionosphere.error.rate.data)

kmeans.ringnorm.error.rate.data <- matrix(,ncol=4)
for(j in 1:20){
  temp <- filter(kmeans.good.bad.ringnorm,X1 == get("j") & X2 == 2)
  error.rate <- temp[,4]/rowSums(temp[,3:4])
  iter.count <- mean(temp$X5)
  error.rate.total <- colSums(as.matrix(error.rate))
  kmeans.ringnorm.error.rate.data <- rbind(kmeans.ringnorm.error.rate.data,c(j,2,error.rate.total,iter.count))
}
kmeans.ringnorm.error.rate.data <- na.omit(kmeans.ringnorm.error.rate.data)

kmeans.ringnorm.pca.error.rate.data <- matrix(,ncol=4)
for(j in 1:20){
  temp <- filter(kmeans.good.bad.ringnorm.pca,X1 == get("j") & X2 == 2)
  error.rate <- temp[,4]/rowSums(temp[,3:4])
  iter.count <- mean(temp$X5)
  error.rate.total <- colSums(as.matrix(error.rate))
  kmeans.ringnorm.pca.error.rate.data <- rbind(kmeans.ringnorm.pca.error.rate.data,c(j,2,error.rate.total,iter.count))
}
kmeans.ringnorm.pca.error.rate.data <- na.omit(kmeans.ringnorm.pca.error.rate.data)


EM.ionosphere.pca.error.rate.data <- matrix(,ncol=4)
for(j in 1:20){
    temp <- filter(EM.good.bad.ionosphere.pca,X1 == get("j") & X2 == 2)
    error.rate <- temp[,4]/rowSums(temp[,3:4])
    iter.count <- mean(temp$X5)
    error.rate.total <- colSums(as.matrix(error.rate))
    EM.ionosphere.pca.error.rate.data <- rbind(EM.ionosphere.pca.error.rate.data,c(j,2,error.rate.total,iter.count))
}
EM.ionosphere.pca.error.rate.data <- na.omit(EM.ionosphere.pca.error.rate.data)


EM.ionosphere.error.rate.data <- matrix(,ncol=4)
for(j in 1:20){
  temp <- filter(EM.good.bad.ionosphere,X1 == get("j") & X2 == 2)
  error.rate <- temp[,4]/rowSums(temp[,3:4])
  iter.count <- mean(temp$X5)
  error.rate.total <- colSums(as.matrix(error.rate))
  EM.ionosphere.error.rate.data <- rbind(EM.ionosphere.error.rate.data,c(j,2,error.rate.total,iter.count))
}
EM.ionosphere.error.rate.data <- na.omit(EM.ionosphere.error.rate.data)


EM.ringnorm.error.rate.data <- matrix(,ncol=4)
for(j in 1:20){
  temp <- filter(EM.good.bad.ringnorm,X1 == get("j") & X2 == 2)
  error.rate <- temp[,4]/rowSums(temp[,3:4])
  iter.count <- mean(temp$X5)
  error.rate.total <- colSums(as.matrix(error.rate))
  EM.ringnorm.error.rate.data <- rbind(EM.ringnorm.error.rate.data,c(j,2,error.rate.total,iter.count))
}
EM.ringnorm.error.rate.data <- na.omit(EM.ringnorm.error.rate.data)

EM.ringnorm.pca.error.rate.data <- matrix(,ncol=4)
for(j in 1:20){
  temp <- filter(EM.good.bad.ringnorm.pca,X1 == get("j") & X2 == 2)
  error.rate <- temp[,4]/rowSums(temp[,3:4])
  iter.count <- mean(temp$X5)
  error.rate.total <- colSums(as.matrix(error.rate))
  EM.ringnorm.pca.error.rate.data <- rbind(EM.ringnorm.pca.error.rate.data,c(j,2,error.rate.total,iter.count))
}
EM.ringnorm.pca.error.rate.data <- na.omit(EM.ringnorm.pca.error.rate.data)





ggplot(as.data.frame(EM.ringnorm.pca.error.rate.data), aes(x = V1, y = V3)) + 
  geom_point() + 
  geom_line(data = as.data.frame(EM.ringnorm.error.rate.data), aes(x = V1, y = V3))


draw.plot <- function(dataset1.pca,dataset2,heading){
  p <-plot(x=dataset1.pca[,1],y=dataset1.pca[,3],type="l",col="red",xlab="Iteration",ylab="Error Rate",main=heading)
  lines(x=dataset2[,1],y=dataset2[,3], col ="blue")
  legend("topright", col=c("red", "blue") ,legend=c("pca","no pca"),lty=1, title="Type")
  p
}

draw.plot.compare <- function(dataset1.pca,dataset2,heading){
  p <-plot(x=dataset1.pca[,1],y=dataset1.pca[,3],type="l",col="red",xlab="Iteration",ylab="Error Rate",main=heading)
  lines(x=dataset2[,1],y=dataset2[,3], col ="blue")
  legend("topright", col=c("red", "blue") ,legend=c("EM","k means"),lty=1, title="Type")
  p
}

draw.plot(EM.ringnorm.pca.error.rate.data,EM.ringnorm.error.rate.data,"Em.ringnorm.pca vs Em.ringnorm")
draw.plot(EM.ionosphere.pca.error.rate.data,EM.ionosphere.error.rate.data,"Em.ionosphere.pca vs Em.ionosphere")

draw.plot.compare(EM.ionosphere.error.rate.data,kmeans.ionosphere.error.rate.data,"Em.ionosphere vs kmeans.ionosphere")
draw.plot.compare(EM.ringnorm.error.rate.data,kmeans.ringnorm.error.rate.data,"Em.ringworm vs kmeans.ringworm")



