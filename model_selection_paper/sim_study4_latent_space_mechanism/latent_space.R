source('../spectral_ex.R')
source('../xgboost.R')
source('latent_sim.R')
library(mvtnorm)
library(intergraph)
library(igraph)
###############################################
#Sizes<-c(50,100,150,200,500)
#Dims<-c(1:5)
#Scales<-seq(1:100)/100
#dim<-Dims[1]
#size<-Sizes[1]
#type <-"euclidean"  
#M<-100
###############################################
latent_space<-function(size,type,M,dim,scale){

nets_euc<- latent_sim(size=size,dim,space="euclidean",scale,M)
nets_bil<- latent_sim(size=size,dim,space="bilinear",scale,M)

eigen_euc <- lapply(lapply(nets_euc,asIgraph),gspectrum,dir=FALSE)
eigen_bil <- lapply(lapply(nets_bil,asIgraph),gspectrum,dir=FALSE)

###################################################################
# Model Selection
data<-c()
data<-cbind(data,t(matrix(unlist(eigen_euc), ncol = M, nrow = size+1)))
data<-rbind(data,t(matrix(unlist(eigen_bil), ncol = M, nrow = size+1)))

data<-cbind(data,c(rep(0,M),rep(1,M)))
colnames(data)<-c(c(1:(size+1)),"label")
rownames(data)<-c(rep(c("Euclidean"),M),rep(c("Bilinear"),M))

obs_net <- latent_sim(size,dim,space=type,scale,M=1)
obs_net <- obs_net[[1]]
obs_eigen <-gspectrum(asIgraph(obs_net),dir=FALSE) # observed eigenvalue

pred_table <-xg_pred(obs_eigen,data) # class predicted

return(pred_table)
}

