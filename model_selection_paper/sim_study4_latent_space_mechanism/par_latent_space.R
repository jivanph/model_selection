# PREAMBLE
library(intergraph)
library(igraph)
library(parallel)
library(mvtnorm)

source('../xgboost.R')
source('../spectral.R')
source('latent_space.R')

wd<-getwd()

# PARAMETERS
Sizes<-c(50,100,150,200,250) 
Type<-c("euclidean","bilinear")
Dims<-c(1:5)
Scale<-seq(1:10)/100
M<-100
Replicates <-1000
n_cores <-25

# RUN METHODOLOGY SEVERAL TIMES
sim_fun <- function(sim_id,Sizes,Dims,Type,Scale,M,seeds){

file_ <- paste0("out/sim_", ".out")
sink(file_)

cat(paste0("\nStarting simulation number: ", sim_id, "\n"))
set.seed(seeds[sim_id])

tic<- Sys.time()
results<-vector(mode = "list", length = 2)
mat_res<-array(NA,dim=c(length(Sizes),length(Dims),length(Type),length(Scale),2))

for(i1 in 1:length(Sizes)){
for(i2 in 1:length(Dims)){
for(i3 in 1:length(Type)){
for(i4 in 1:length(Scale)){

size<-Sizes[i1]
dim<-Dims[i2]
type<-Type[i3]
scale<-Scale[i4]

 run <- latent_space(size=size,type=type,dim=dim,scale=scale,M=M)
 mat_res[i1,i2,i3,i4,]<-run$pred

}}}}

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

#try<-sim_fun(sim_id=1,Sizes=c(50),Type=c("euclidean"),M=50,Dims=c(2),Scale=c(0.5),seeds=seeds)
#print(try)

latent_space_res <- mclapply(1:Replicates, sim_fun,
			Sizes = Sizes,
			Type=Type,
			Dims=Dims,
			M = M,
			Scale = Scale,
			seeds = seeds,mc.cores = n_cores)

save(latent_space_res, file = "latent_space.rda")
