# PREAMBLE
library(intergraph)
library(igraph)
library(parallel)
library(mvtnorm)

source('../xgboost.R')
source('../spectral.R')
source('latent.R')

wd<-getwd()

# PARAMETERS
Sizes<-c(50,100,150,200,250) 
Type<-seq(1,5)

M<-100
Replicates <-1000
n_cores <-50

# RUN METHODOLOGY SEVERAL TIMES
sim_fun <- function(sim_id,Sizes,Type,M,seeds){

file_ <- paste0("out/sim_", ".out")
sink(file_)

cat(paste0("\nStarting simulation number: ", sim_id, "\n"))
set.seed(seeds[sim_id])

tic<- Sys.time()
results<-vector(mode = "list", length = 5)
mat_res<-array(NA,dim=c(length(Sizes),length(Type),5))

for(i1 in 1:length(Sizes)){
for(i3 in 1:length(Type)){

size<-Sizes[i1]
type<-Type[i3]

 run <- latent(size=size,type=type,space="euclidean",M=M)
 mat_res[i1,i3,]<-run$pred

}}

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

#try<-sim_fun(1,Sizes=c(50),Type=c(1),M=50,seeds)
#print(try)

latent_res <- mclapply(1:Replicates, sim_fun,
			Sizes = Sizes,
			Type=Type,
			M = M,
			seeds = seeds,mc.cores = n_cores)

save(latent_res, file = "latent.rda")
