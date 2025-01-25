#library(igraph)

# we compute the laplacian matrix from a given network
laplacian<-function(mat,dir=FALSE){
  if(dir==FALSE){return(diag(rowSums(mat)) - mat)}
  if(dir==TRUE){return(mat%*%t(mat))}
}

gspectrum<-function(obs_net,val.only=TRUE,norm=FALSE,ex=0,dir=FALSE,netObj=FALSE){
  if(val.only!=TRUE){
    gspect <- vector(mode = "list", length = 2)
    
    if(dir==FALSE){mat<-as.matrix(as_adjacency_matrix(obs_net))}
    if(dir==TRUE & netObj==FALSE){mat<-as.matrix(asNetwork(obs_net),matrix.type="incidence")}
    if(dir==TRUE & netObj==TRUE){mat<-as.matrix(obs_net,matrix.type="incidence")}

    obs_eigen <- eigen(laplacian(mat,dir))
    gspect[[1]]<- obs_eigen$values/(sum(obs_eigen$values)*norm + 1*(1-norm))
    gspect[[2]]<- obs_eigen$vectors
    return(gspect) 
  }
  else{
    if(dir==FALSE){mat<-as.matrix(as_adjacency_matrix(obs_net))}
    if(dir==TRUE & netObj==FALSE){mat<-as.matrix(asNetwork(obs_net),matrix.type="incidence")}
    if(dir==TRUE & netObj==TRUE){mat<-as.matrix(obs_net,matrix.type="incidence")}
    obs_eigen<-c()
    for(i in 1:(ex+1)){
      lap<-laplacian(mat^i,dir)
      aux<-eigen(lap,only.values = TRUE)
      aux<- aux$values/(sum(aux$values)*norm + 1*(1-norm))
      obs_eigen <- c(obs_eigen,c(aux,sum(aux)))
    }
    
    return(obs_eigen)
  }
}


spectral_gof<-function(obs_net,fitted_nets,null_nets){
  
  # we compute the rescaled eigenvalues
  obs_eigen <- eigen(laplacian(obs_net),only.values=TRUE)
  obs_eigen <- obs_eigen$values/sum(obs_eigen$values)
  
  M1 <- length(fitted_nets)
  fitted_eigens <- vector(mode = "list", length = M1)
  fitted_ESD <- numeric(M1) # Euclidean Spectral Distance
  
  # We compute the Spectral Euclidean distance for the fitted (simulated) networks
  for(i in 1:M1){
    temp <- eigen(laplacian(fitted_nets[[i]]),only.values=TRUE)
    fitted_eigens[[i]] <- temp$values/sum(temp$values)
    fitted_ESD[i] <- sqrt(sum((obs_eigen - fitted_eigens[[i]])^2))
  }
  
  M2 <- length(null_nets)
  null_eigens <- vector(mode = "list", length = M2)
  null_ESD <- numeric(M2)
  
  # We compute the Spectral Euclidean distance for the null (simulated) networks
  for(i in 1:M2){
    temp <- eigen(laplacian(null_nets[[i]]),only.values=TRUE)
    null_eigens[[i]] <- temp$values/sum(temp$values)
    null_ESD[i] <- sqrt(sum((obs_eigen - null_eigens[[i]])^2))
  }
  
  # compute results and SGOF
  return(list(obs_eigen = obs_eigen,
              fitted_eigens = fitted_eigens,
              null_eigens = null_eigens,
              fitted_ESD = fitted_ESD,
              null_ESD = null_ESD,
              R2 = 1- mean(fitted_ESD)/mean(null_ESD)))
}

#x<-spectral_gof(obs_net_sbm,sim_nets_sbm)
#y<-spectral_gof(obs_net_sbm,sim_nets_ber)




