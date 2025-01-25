# PREAMBLE

library(ergm)
library(igraph) 
library(intergraph) 

source("ergm_sims.R")
source("../spectral_ex.R")
source("../xgboost.R")

library(parallel)

# PARAMETERS
M <-100
Sizes <-c(100,200,300)
Tr<- seq(1,50)/500 #gwesp parameters

Replicates <-5000
n_cores <- 50

# RUN METHODOLOGY SEVERAL TIMES
sim_fun <- function(sim_id,Sizes,M,Tr,seeds){

file_ <- paste0("out/sim_", ".out")
sink(file_)

cat(paste0("\nStarting simulation number: ", sim_id, "\n"))
set.seed(seeds[sim_id])

results<-vector(mode = "list", length = 2)
mar_res<-array(NA,dim=c(length(Sizes),length(Tr),2))

tic<- Sys.time()
for(i1 in 1:length(Sizes)){
for(i2 in 1:length(Tr)){

 pred <-  ergm_sims(M=M,size=Sizes[i1],t=Tr[i2])
 mar_res[i1,i2,]<-pred$pred
}
}

toc <- Sys.time()
time <-toc-tic
results$time<-time
results$mar<-mar_res

    cat(paste0("\n\nTime elapsed: ", time, " seconds."))
    file_ <- paste0("res/res_",sim_id,".rda")
    save(results, file = file_)
    cat(paste0("\n\nLast file created: ",file_))
     sink()
    return(results)
}

seeds<-sample(1:99999999,Replicates,replace=FALSE)
save(seeds, file="seeds.rda")

#try<-sim_fun(1,Sizes=c(50),Tr=c(0.1),M=50,seeds) 
#print(try)

ergm_large_res <- mclapply(1:Replicates, sim_fun,
			Sizes = Sizes,
			Tr = Tr,
			M = M,
			seeds = seeds,mc.cores = n_cores)

save(ergm_large_res, file = "ergm_large.rda") 


