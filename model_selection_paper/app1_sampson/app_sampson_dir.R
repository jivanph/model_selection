set.seed(333)

library(ergm)
library(hergm)
library(mlergm)
library(latentnet)
library(blockmodels)
library(intergraph)
library(igraph)
library(ggplot2)
source('spectral_ex.R')
source('../xgboost.R')

# Load network 
data(sampson)
net <- samplike 
delete.edge.attribute(net, "nominations")
obs_net <- asIgraph(net)
size  <-vcount(obs_net)
M<-1000 # Networks to be simulated

# Estimate SBM: ###################################################### 
m.SBM <- BM_bernoulli("SBM",
                      as.matrix(as_adjacency_matrix(obs_net)))
#m.SBM$explore_min=2;
m.SBM$explore_max=5;
m.SBM$estimate()


nets_SBM<-vector(mode='list', length=4)
eigen_SBM<-vector(mode='list', length=4)

for(K in c(1:4)){
#K<-3 #We force 3 blocks

node_memb <- apply(m.SBM$memberships[[K]]$Z,MARGIN=1,FUN=which.max)
group_sizes<-as.numeric(table(node_memb))
block_transition<-m.SBM$model_parameters[[K]]$pi
#Symmetrization done to round values # error below 5.551115e-15

# Simulation of SBM ###################################################

sbm_net<-function(size,group_sizes,block_transition,M){
  if(M==1){sim_sbm_nets<-sample_sbm(size,pref.matrix=block_transition,
                                    block.sizes=group_sizes)}
  else{
    sim_sbm_nets <- vector(mode = "list", length = M)
    for(i in 1:M){
      sim_sbm_nets[[i]] <- sample_sbm(size,
                                      pref.matrix=block_transition,
                                      block.sizes=group_sizes,directed=TRUE)
    }}
  return(sim_sbm_nets)}

nets_SBM[[K]]<-sbm_net(size,group_sizes,block_transition,M) # Simulate SBM
eigen_SBM[[K]]<-lapply(nets_SBM[[K]],gspectrum,ex=0,dir=TRUE) # SBM Eigenvalues 

}

# Estimate Latent Space Model: ###################################################### 

nets_LSM <- vector(mode='list', length=4)
eigen_LSM <- vector(mode='list', length=4)

nets_LSM2 <- vector(mode='list', length=12)
eigen_LSM2 <- vector(mode='list', length=12)

step<-1
for(D in c(1:4)){
     lpm <- ergmm(net ~ edges +mutual+ euclidean(d = D), verbose = 2)
  for(g in c(2:4)){	
        lpm2 <- ergmm(net ~ edges +mutual+ euclidean(d = D,G=g), verbose = 2)
        sim2 <- simulate(lpm2, M)
	nets_LSM2[[step]] <- sim2[[2]]
	eigen_LSM2[[step]]<-lapply(lapply(nets_LSM2[[step]],asIgraph),gspectrum,ex=0,dir=TRUE)
	step <- step+1
  }
sim <- simulate(lpm, M) # Simulate LSM:
nets_LSM[[D]] <- sim[[2]]
eigen_LSM[[D]]<-lapply(lapply(nets_LSM[[D]],asIgraph),gspectrum,ex=0,dir=TRUE)

}

#### Estimate Local Dependence Block Model: ##########################
#loc_ergm<-hergm(net~edges+mutual+dgwesp(fixed = FALSE),
#                max_number = 3,verbose = 2,sample_size = 5000)
#sim_loc_ergm <- function(K, model){ 
#  sim_list <- rep(list(NULL), K)
#  N <- network.size(model$network)
#  for (k in 1:K) { 
#    net_ <- simulate_mlnet(model$network~edges+mutual+dgwesp(fixed = FALSE), 
#                           theta = model$results$parameters,
#                           node_memb = as.numeric(model$results$partition),
#                           between_form = ~ edges,  
#                           between_theta = model$results$between_parameter)
#    print(k*100/K)
#
#    for (i in 1:N) { 
#      for (j in 1:N) {
#        gi <- model$results$partition[i]
#        gj <- model$results$partition[j] 
#        if (gi == gj) next; 
#        net_[i,j] <- rbinom(1, 1, model$results$sbm.params[gi,gj])
#      }
#    }
#    sim_list[[k]] <- net_    
#  }
#  return(sim_list)
#}
# Ex: 
#nets_LDB <- sim_loc_ergm(M, loc_ergm)
#eigen_LDB<-vector(mode = "list", length = M)
#eigen_LDB<-lapply(lapply(nets_LDB,asIgraph),gspectrum,ex=0,dir=TRUE)
###################################################################

#save(nets_LDB,file="nets_LDB.RData")
save(nets_SBM,file="nets_SBM.RData")
save(nets_LSM,file="nets_LSM.RData")
save(nets_LSM2,file="nets_LSM2.RData")

###################################################################
# Model Selection
data<-c()
data<-cbind(data,t(matrix(unlist(eigen_SBM[[1]]), ncol = M, nrow = size+1)))
data<-rbind(data,t(matrix(unlist(eigen_SBM[[2]]), ncol = M, nrow = size+1)))
data<-rbind(data,t(matrix(unlist(eigen_SBM[[3]]), ncol = M, nrow = size+1)))
data<-rbind(data,t(matrix(unlist(eigen_SBM[[4]]), ncol = M, nrow = size+1)))

data<-rbind(data,t(matrix(unlist(eigen_LSM[[1]]), ncol = M, nrow = size+1)))
data<-rbind(data,t(matrix(unlist(eigen_LSM[[2]]), ncol = M, nrow = size+1)))
data<-rbind(data,t(matrix(unlist(eigen_LSM[[3]]), ncol = M, nrow = size+1)))
data<-rbind(data,t(matrix(unlist(eigen_LSM[[4]]), ncol = M, nrow = size+1)))

for(i in c(1:12)){
data<-rbind(data,t(matrix(unlist(eigen_LSM2[[i]]), ncol = M, nrow = size+1)))
}

column <-c()

for(i in c(0:19)){
column<-c(column,rep(i,M))
}

data <- cbind(data,column)

colnames(data)<-c(c(1:(size+1)),"label")

#rownames(data)<-c(rep(c("SBM"),M),rep(c("LSM"),M),rep(c("LDB"),M))

obs_eigen <-gspectrum(obs_net,dir=TRUE) # observed eigenvalue
pred_table <-xg_pred(obs_eigen,data) # class predicted

save(pred_table,file="pred_table.RData")









