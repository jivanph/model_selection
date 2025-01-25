source('../spectral_ex.R')
source('../xgboost.R')
source('latent_sim.R')
library(mvtnorm)
library(intergraph)
library(igraph)
###############################################
Sizes<-c(50,100,150,200,500)
size<-Sizes[1]
type <-1 
M<-100
###############################################
latent<-function(size,space="euclidean",M,type){

nets_l1<- latent_sim(size=size,dim=1,space=space,M)
nets_l2<- latent_sim(size=size,dim=2,space=space,M)
nets_l3<- latent_sim(size=size,dim=3,space=space,M)
nets_l4<- latent_sim(size=size,dim=4,space=space,M)
nets_l5<- latent_sim(size=size,dim=5,space=space,M)

eigen_l1 <- lapply(lapply(nets_l1,asIgraph),gspectrum,dir=FALSE)
eigen_l2 <- lapply(lapply(nets_l2,asIgraph),gspectrum,dir=FALSE)
eigen_l3 <- lapply(lapply(nets_l3,asIgraph),gspectrum,dir=FALSE)
eigen_l4 <- lapply(lapply(nets_l4,asIgraph),gspectrum,dir=FALSE)
eigen_l5 <- lapply(lapply(nets_l5,asIgraph),gspectrum,dir=FALSE)

###################################################################
# Model Selection
data<-c()
data<-cbind(data,t(matrix(unlist(eigen_l1), ncol = M, nrow = size+1)))
data<-rbind(data,t(matrix(unlist(eigen_l2), ncol = M, nrow = size+1)))
data<-rbind(data,t(matrix(unlist(eigen_l3), ncol = M, nrow = size+1)))
data<-rbind(data,t(matrix(unlist(eigen_l4), ncol = M, nrow = size+1)))
data<-rbind(data,t(matrix(unlist(eigen_l5), ncol = M, nrow = size+1)))

data<-cbind(data,c(rep(0,M),rep(1,M),rep(2,M),rep(3,M),rep(4,M)))
colnames(data)<-c(c(1:(size+1)),"label")
rownames(data)<-c(rep(c("L1"),M),rep(c("L2"),M),
		  rep(c("L3"),M),rep(c("L4"),M),rep(c("L5"),M))

obs_net <- latent_sim(size,dim=type,space=space,M=1)
obs_net <- obs_net[[1]]
obs_eigen <-gspectrum(asIgraph(obs_net),dir=FALSE) # observed eigenvalue

pred_table <-xg_pred(obs_eigen,data) # class predicted

return(pred_table)
}

