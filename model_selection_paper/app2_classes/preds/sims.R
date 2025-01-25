library(ergm)
library(hergm)
library(mlergm)
library(intergraph)
library(igraph)
library(parallel)

set.seed(123)
wd<-getwd()

setwd('../')
setwd(paste0(getwd(),"/models"))

load('school_net.RData')
load('m1.RData')
load('m2.RData')
load('m3.RData')
load('m4.RData')

M <- 100
size<-length(m1$node_memb)
nthread <- 20

m1_nets <- vector(mode = "list", length = M)
m2_nets <- vector(mode = "list", length = M)
m3_nets <- vector(mode = "list", length = M)
m4_nets <- vector(mode = "list", length = M)


#test_m1_net <- simulate_mlnet(school_net ~ edges + mutual + 
#                                           nodeofactor("sex", levels = 1) +
#                                           nodeifactor("sex", levels = 1) +
#                                           nodematch("sex") + 
#                                           odegree(1:6),  
#                                           theta = as.numeric(m1$theta),
#                                           parameterization = "offset", 
#                                           node_memb = m1$node_memb) 

#image(test_m1_net[ , ])


for(i in 1:M){
  print(i)
  m1_nets[[i]] <- simulate_mlnet(school_net ~ edges + mutual +
                                nodeofactor("sex", levels = 1) + 
                                nodeifactor("sex", levels = 1) +
			       	nodematch("sex") +
			       	odegree(1:6),
                                node_memb = m1$node_memb,
				parameterization = "offset",
                                theta = m1$theta)

  m2_nets[[i]] <- simulate_mlnet(school_net ~ edges + mutual +
                                nodeofactor("sex", levels = 1) +
                                nodeifactor("sex", levels = 1) +
			       	nodematch("sex") +
			       	odegree(1:6) +
  			  	dgwesp(fixed = TRUE, decay = 0, type = "OTP"),
                                node_memb = m2$node_memb,
				parameterization = "offset",
                                theta = m2$theta)


  m3_nets[[i]] <- simulate_mlnet(school_net ~ edges + mutual +
                                nodeofactor("sex", levels = 1) +
                                nodeifactor("sex", levels = 1) +
				nodematch("sex") +
			       	odegree(1:6) +
				dgwesp(fixed = TRUE, decay = 0.25, type = "OTP"),
                                node_memb = m3$node_memb,
				parameterization = "offset",
                                theta = m3$theta)

  m4_nets[[i]] <- simulate_mlnet(school_net ~ edges + mutual +
                                nodeofactor("sex", levels = 1) +
                                nodeifactor("sex", levels = 1) +
			       	nodematch("sex") +
			       	odegree(1:6) +
				dgwesp(fixed = FALSE, type = "OTP"),
                                node_memb = m4$node_memb,
				parameterization = "offset",
                                theta = m4$theta)
}


setwd('../')
setwd(paste0(getwd(),"/preds"))

save(m1_nets,file="m1_nets.RData")
save(m2_nets,file="m2_nets.RData")
save(m3_nets,file="m3_nets.RData")
save(m4_nets,file="m4_nets.RData")









