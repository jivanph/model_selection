library(ergm)
library(igraph)
library(intergraph)
library(randomForest)
library(nnet)
library(naivebayes)
library(xgboost)

source("../spectral_ex.R")
source("../classification.R")

ergm_sims <-function(M,size,t){

d<- -2.5

coef1<-c(d,c(t,1))
coef0 <-c(d)

nets_gwesp<- simulate(network(size,directed=FALSE) ~ edges+gwesp, coef=coef1,nsim=M,
			control=control.simulate.formula(parallel=0,
							  MCMC.burnin=10*choose(size,2),
							  MCMC.interval=choose(size,2)))
nets_gwesp <-lapply(lapply(nets_gwesp,asIgraph),as.undirected)
eigen_gwesp <- t(matrix(unlist(lapply(nets_gwesp,gspectrum)),ncol = M, nrow = size+1))

nets_base <- simulate(network(size,directed=FALSE) ~ edges, coef=coef0,nsim=M,
		control=control.simulate.formula(parallel=0,
						 MCMC.burnin=10*choose(size,2),
						 MCMC.interval=choose(size,2)))
nets_base <-lapply(lapply(nets_base,asIgraph),as.undirected)
eigen_base <- t(matrix(unlist(lapply(nets_base,gspectrum)),ncol = M, nrow = size+1))

#######################################
# Model selection

data<-unlist(eigen_base)
data<-rbind(data,unlist(eigen_gwesp))

data<-cbind(data,c(rep(0,M),rep(1,M)))
colnames(data)<-c(c(1:(size+1)),"label")
rownames(data)<-c(rep(c("density"),M),rep(c("gwesp"),M))

obs_net <- simulate(network(size,directed=FALSE) ~ edges+gwesp, coef=coef1,
			control=control.simulate.formula(parallel=0,
							 MCMC.burnin=10*choose(size,2),
							 MCMC.interval=choose(size,2)))
obs_net <-as.undirected(asIgraph(obs_net));
obs_eigen <- gspectrum(obs_net)

return(predictions(obs_eigen,data)) # class predicted
}



