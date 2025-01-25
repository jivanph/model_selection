wd<-getwd()

setwd(paste0(wd,"/res"))

files_<-list.files()

n<-length(files_)

load(files_[1])  

dims<-dim(results$mar)

F.mat_res<-array(NA,dim=c(dims,n))

for(i in 1:n){
	load(files_[i])
	F.mat_res[ , , ,i]<-results$mar
}

setwd(paste0(wd,"/reports"))
save(F.mat_res,file="results.RData")
