# PREAMBLE

library(ergm)
library(igraph) 
library(intergraph) 
library(xgboost)  # the main algorithm
library(e1071)
library(caret)    # for the confusionmatrix() function (also needs e1071 package)
library(dplyr)    # for some data preperation
library(Rcpp)

source("ergm_sims.R")
source("xgboost.R")
source("../spectral_ex.R")

library(parallel)

# PARAMETERS
#M <-100

source("params.R")

#M<- c(5000,10000,15000,20000)
#Sizes <-c(25,50,75)
#Tr<- seq(1,20)/100 #gwesp parameters

Replicates <-100
n_cores <- 50

# RUN METHODOLOGY SEVERAL TIMES
sim_fun <- function(sim_id,Sizes,M,Tr,seeds){

file_ <- paste0("out/sim_", ".out")
sink(file_)

cat(paste0("\nStarting simulation number: ", sim_id, "\n"))
set.seed(seeds[sim_id])


results<-list()
mar_res<-array(NA,dim=c(length(Sizes),length(Tr),length(M),2))
test_confusions <-array(list(),dim=c(length(Sizes),length(Tr),length(M)))
train_confusions <-array(list(),dim=c(length(Sizes),length(Tr),length(M)))
validations <- array(list(),dim=c(length(Sizes),length(Tr),length(M)))

tic<- Sys.time()
for(i1 in 1:length(Sizes)){
for(i2 in 1:length(Tr)){
for(i3 in 1:length(M)){

 pred <-  ergm_sims(M=M[i3],size=Sizes[i1],t=Tr[i2])
 mar_res[i1,i2,i3,]<-pred$pred
 train_confusions[[i1,i2,i3]]<-pred$train_CM
 test_confusions[[i1,i2,i3]]<-pred$test_CM
 validations[[i1,i2,i3]] <- pred$validation

}
}
}

toc <- Sys.time()
time <-toc-tic
results$time<-time
results$mar<-mar_res
results$train_confusions<-train_confusions
results$test_confusions<-test_confusions
results$validations <- validations

    cat(paste0("\n\nTime elapsed: ", time, " seconds."))
    file_ <- paste0("res/res_",sim_id,".rda")
    save(results, file = file_)
    cat(paste0("\n\nLast file created: ",file_))
     sink()
    return(results)
}

seeds<-sample(1:99999999,Replicates,replace=FALSE)
save(seeds, file="seeds.rda")

#try<-sim_fun(1,Sizes=c(25,50),Tr=c(0.1,0.2),M=c(50,100),seeds) 
#print(try)

ergm_overfit <- mclapply(1:Replicates, sim_fun,
			Sizes = Sizes,
			Tr = Tr,
			M = M,
			seeds = seeds,mc.cores = n_cores)

save(ergm_overfit, file = "ergm_overfit.rda") 


