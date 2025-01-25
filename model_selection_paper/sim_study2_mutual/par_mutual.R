# PREAMBLE
source('../xgboost.R')
source('../spectral.R')
source('mutual.R')

wd<-getwd()

# PARAMETERS
Sizes<-c(50,100,150,200,250) 
Tr<-seq(0,1,length=10)
Type<-c(0)

M<-100
Replicates <-1000
n_cores <-40

# RUN METHODOLOGY SEVERAL TIMES
sim_fun <- function(sim_id,Sizes,Tr,Type,M,seeds){

file_ <- paste0("out/sim_", ".out")
sink(file_)

cat(paste0("\nStarting simulation number: ", sim_id, "\n"))
set.seed(seeds[sim_id])

tic<- Sys.time()
results<-vector(mode = "list", length = 2)
mat_res<-array(NA,dim=c(length(Sizes),length(Tr),length(Type),2))

for(i1 in 1:length(Sizes)){
for(i2 in 1:length(Tr)){
for(i3 in 1:length(Type)){

size<-Sizes[i1]
t <-Tr[i2]
type<-Type[i3]

 run <- mutual(size=size,theta=t,type=type,M=M)
 mat_res[i1,i2,i3,]<-run$pred

}}}

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

#try<-sim_fun(1,Sizes=c(50),Tr=c(0),Type=c(0),M=50,seeds)
#print(try)

mutual_res <- mclapply(1:Replicates, sim_fun,
			Sizes = Sizes,
			Tr = Tr,
			Type=Type,
			M = M,
			seeds = seeds,mc.cores = n_cores)

save(mutual_res, file = "mutual.rda")
