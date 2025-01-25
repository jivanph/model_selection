library(igraph)

#Bernoulli
# size<-100
# p<-0.05
# M<-1
#   
ber_net<-function(size,p,M){
  
  if(M==1){sim_ber_nets<-sample_gnp(size,p)}
  
  else{
  
    sim_ber_nets <- vector(mode = "list", length = M)
    
    for(i in 1:M){
      sim_ber_nets[[i]] <- sample_gnp(size,p)
    }
  }
  
  return(sim_ber_nets)
}

###########################
#library(igraph)

#SBM
# size<-99
# group_sizes<-c(size/3,size/3,size/3)
# block_transition<- cbind(c(.5,.25,0.1),c(.25,.5,0.25),c(0.1,0.25,0.5))
# M<-1

sbm_net<-function(size,group_sizes,block_transition,M){
  
  if(M==1){sim_sbm_nets<-sample_sbm(size,
                             pref.matrix=block_transition,
                             block.sizes=group_sizes)}
  
  else{
    sim_sbm_nets <- vector(mode = "list", length = M)
    
    for(i in 1:M){
      sim_sbm_nets[[i]] <- sample_sbm(size,
                                      pref.matrix=block_transition,
                                      block.sizes=group_sizes)
    }
  }
  
  return(sim_sbm_nets)
}

#########################
#library(mlergm)
#library(intergraph)# to turn final answer into igraph object
# Local dependency model
# size<-99
# node_memb <- c(rep(1, 30), rep(2, 30), rep(3, 30))
# theta = c(-3, 1, .5)
# M<-1

ldergm_net<-function(size,node_memb,theta,M){
  
  if(M==1){
    
    #burnin<-max(1e+6,10*choose(size,2))
    # sim_ldergm_nets<-asIgraph(simulate_mlnet(form = net ~ edges + gwesp, 
    #                          node_memb = node_memb,
    #                          theta = theta),
    #                           control = control.simulate(MCMC.burnin = burnin))
    sim_ldergm_nets<-asIgraph(simulate_mlnet(form = net ~ edges + gwesp, 
                                             node_memb = node_memb,
                                             theta = theta))
    
  }
  
  else{
    sim_ldergm_nets <- vector(mode = "list", length = M)
  
    for(i in 1:M){
      
      
      net <- network.initialize(size, directed = FALSE)
      # Simulate a network from the edge + gwesp model 
      
      sim_ldergm_nets[[i]] <- asIgraph(simulate_mlnet(form = net ~ edges + gwesp, 
                              node_memb = node_memb,
                              theta = theta),
                              control = control.simulate(MCMC.burnin = burnin))
    }
  }
  return(sim_ldergm_nets)
}

#######################

