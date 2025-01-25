# Experimental Studies. 
# 1. Latent space simulation 
# Generate latent positions according to a multivariate Gaussian of dimension d = 1, 2, 3. MVT(0, I). 
# Generate a network according to the positions. 
# log(P(X_ij = 1) / P(X_ij = 0)) = theta - ||z_i - z_j||_2 
# Fit latent positions models for dimension d = 1, 2, 3, 4. 
# Run procedure and examine whether we can pick of the right dimension. 
library(intergraph)
library(igraph)
library(mvtnorm)

#size<-5
#M<-1000
#dim <- 2

latent_sim<-function(size,dim,space="euclidean",scale=1,M){

d<- -2.5

latent_nets<-vector(mode="list",length=M)
for(m in 1:M){
  
  positions<-rmvnorm(size, mean = rep(0,dim), sigma = scale*diag(dim))
  ifelse(space=="euclidean",
	 latent.covar<- -1*as.matrix(dist(positions,upper=TRUE,diag=TRUE)),
	 latent.covar <- positions%*%t(positions))
  
  X <- matrix(0,nrow=size,ncol=size);
  
  for(i in 1:(size-1)){
    for(j in (i+1):size){
      pij<-exp(d+latent.covar[i,j])/(1+exp(d+latent.covar[i,j]))
      ifelse(runif(1)<pij,X[i,j]<-1,X[i,j]<-0)
    }
  }
  
  X<- X+t(X)
  
  latent_nets[[m]]<-asNetwork(graph.adjacency(X, mode="undirected"))

}

return(latent_nets)
}


