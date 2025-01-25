# PREAMBLE

library(ergm)
library(igraph) 
library(intergraph) 
library(Rcpp)
library(xgboost)
library(randomForest)
library(nnet)
library(naivebayes)
library(MASS)

source("ergm_sims.R")
source("../spectral_ex.R")
source("../classification.R")

library(parallel)

# PARAMETERS
M <-100
Sizes <-c(25,50,75)
Tr<- seq(1,10)/10 #gwesp parameters

Replicates <-1000
n_cores <- 25

# RUN METHODOLOGY SEVERAL TIMES
sim_fun <- function(sim_id,Sizes,M,Tr,seeds){

file_ <- paste0("out/sim_", ".out")
sink(file_)

cat(paste0("\nStarting simulation number: ", sim_id, "\n"))
set.seed(seeds[sim_id])

results<-vector(mode = "list", length = 2)
mat_res<-array(NA,dim=c(length(Sizes),length(Tr),3,2))

tic<- Sys.time()
for(i1 in 1:length(Sizes)){
for(i2 in 1:length(Tr)){

 mat_res[i1,i2,,] <-  ergm_sims(M=M,size=Sizes[i1],t=Tr[i2])
}
}

toc <- Sys.time()
time <-toc-tic
results$time<-time
results$mat<-mat_res

    cat(paste0("\n\nTime elapsed: ", time, " seconds."))
    file_ <- paste0("res/res_",sim_id,".rda")
    save(results, file = file_)
    cat(paste0("\n\nLast file created: ",file_))
     sink()
    return(results)
}

seeds<-sample(1:99999999,Replicates,replace=FALSE)
save(seeds, file="seeds.rda")

#try<-sim_fun(1,Sizes=c(50),Tr=c(0.5),M=50,seeds) 
#print(try)

ergm_multi <- mclapply(1:Replicates, sim_fun,
			Sizes = Sizes,
			Tr = Tr,
			M = M,
			seeds = seeds,mc.cores = n_cores)

save(ergm_multi, file = "ergm_multi.rda") 


