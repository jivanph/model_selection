source('../spectral_ex.R')
source('../xgboost.R')

###############################################
#Sizes<-c(50,100,150,200,500)
#size<-Sizes[1]
#Tr<-seq(0,1,length=100)
#t <-Tr[2]
#type <- 0
#M<-100
##############################################
mutual_sim<-function(size,theta,type,nsim){
  
  edge<- -2.5
  C <- 1 + 2*exp(edge)+exp(2*edge+theta)
  p00 <- 1/C; p11<-exp(2*edge+theta)/C
  p01 <- exp(edge)/C; p10 <- exp(edge)/C;
  s1<-p00; s2<-s1+p01; s3<-s2+p10; s4<-s3+p11
  
  mutual_nets<-vector(mode="list",length=nsim)
  
  for(m in 1:nsim){
    
    X <- matrix(NA,nrow=size,ncol=size); diag(X)<-0
    for(i in 1:(size-1)){
      for(j in (i+1):size){
        u<-runif(1)
        if(u<s1){X[i,j]<-0; X[j,i]<-0}
        else if(u<s2){X[i,j]<-0;X[j,i]<-1}
        else if(u<s3){X[i,j]<-1;X[j,i]<-0}
        else {X[i,j]<-1;X[j,i]<-1}
      }
    }
    
    mutual_nets[[m]]<-asNetwork(graph.adjacency(X, mode="directed"))
  }
  
  return(mutual_nets)
  
}

###############################################
mutual<-function(size,theta,type,M){

nets_edge<- mutual_sim(size,theta=0,nsim=M)
nets_mutual<- mutual_sim(size,theta=theta,nsim=M)
eigen_edge <- lapply(lapply(nets_edge,asIgraph,directed=TRUE),gspectrum,dir=TRUE)
eigen_mutual <- lapply(lapply(nets_mutual,asIgraph,directed=TRUE),gspectrum,dir=TRUE)

###################################################################
# Model Selection
data<-c()
data<-cbind(data,t(matrix(unlist(eigen_edge), ncol = M, nrow = size+1)))
data<-rbind(data,t(matrix(unlist(eigen_mutual), ncol = M, nrow = size+1)))

data<-cbind(data,c(rep(0,M),rep(1,M)))
colnames(data)<-c(c(1:(size+1)),"label")
rownames(data)<-c(rep(c("Edge"),M),rep(c("Mutual"),M))

if(type==0){obs_net <- mutual_sim(size,theta=0,nsim=1); obs_net <- obs_net[[1]]
}else{obs_net <- mutual_sim(size,theta=theta,nsim=1); obs_net <- obs_net[[1]]}
  
obs_eigen <-gspectrum(asIgraph(obs_net),dir=TRUE) # observed eigenvalue
pred_table <-xg_pred(obs_eigen,data) # class predicted

return(pred_table)
}

