# kMeans algorithm by Guilherme Passos
# Federal University of Minas Gerais

###############################



###############################

# Algorithm's main function. It receives a matrix cointaing N samples and M features, the number of centroids 
# and the threshold condition for the clustering process. This function returns a list which has the labels of
# data, the centroid's positions and the number of iterations it takes to reach the threshold condition.
MyKmeans <- function (X,k, threshold){
  
  thr <- threshold*10
  itr <- 0
  means <- matrix(nrow=k,ncol=dim(X)[2])
  
  while (thr>threshold){
    if(itr==0) {
      # starting off centroids with the first Ks sample of data set
      for (i in 1:k) means[i,] <- X[sample(1:nrow(X),1),]
    }
    else {
      oldMeans <- means
      # calculting new centroids
      means <- UpdateMeans (X,k,partitionMatrix)
      # checking with the threshold was reached
      thr<-Delta(oldMeans,means)
    } 
    # calculting the distances from each centroids to each sample in the data set
    distances <- ManhattanDist(X,means)
    # assigning data points to its nearest centroid
    partitionMatrix <- GetClusters(distances)
    #iterating 
    itr <- itr +1
  }
  # clustering result
  cluster <- list(labels=partitionMatrix,ks=means,iterations=itr)
  
  return(cluster)
}
# This function computes the Manhattan distance between each feature of a sample and the correspond centroid's 
# feature
ManhattanDist <- function(X,Kmeans){
  d <- matrix(nrow=nrow(X),ncol=nrow(Kmeans))
  for (i in 1:nrow(d)){
    for (j in 1:ncol(d)){
      d[i,j] <- 0
      for (k in 1:ncol(X))
        d[i,j]<-abs(X[i,k]-Kmeans[j,k]) + d[i,j]
    }
  }
  return(d)
}

# This functions assigns each data point to its nearest centroid
GetClusters <- function(d){
  for (i in 1:nrow(d)){
    minDist <- min(d[i,])
    columnIndex_minDist <- which(d[i,]==minDist)
    d[i,which(d[i,]!=minDist)]<- 0
    d[i,columnIndex_minDist] <- 1
  }
  return (d)
}

# This function computes new centroids by taking the mean of all data points assigned to that centroid's cluster
UpdateMeans <- function(X,k,u){
  
  newMeans <- matrix(nrow=k,ncol=dim(X)[2])
  
  for (i in 1:nrow(newMeans )){
    index<-which(u[,i]==1)
    for (j in 1:ncol(newMeans)){
      newMeans[i,j] <- sum(X[index,j])/length(index)
    }
  }
  return (newMeans)
}
# This function calculates the distances from the old centroids to the new ones. This distance is used to
# check if the threshold condition was reached. 
Delta <- function(oldMeans,newMeans){

  d<-vector()
  
  for (i in 1:nrow(oldMeans)){
    d[i]<-0
    for (j in 1:ncol(oldMeans)){
      d[i]<-abs(oldMeans[i,j]-newMeans[i,j])+d[i]
    }
  }
  return (max(d))
}


    
    
    
    
    
    
    
    
    
