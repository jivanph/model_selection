wd<-getwd()

setwd(paste0(wd,"/res"))

files_<-list.files()

n<-length(files_)

load(files_[1])  

dims<-dim(results$train_confusions)


F.mar_res<-  array(NA,dim=c(dims,n,2))
F.res_confusions<-array(NA,dim=c(dims,n,2))
#F.final_models <- array(list(),dim=c(dims,n))
F.validations <- array(list(),dim=c(dims,n))

for(ind in 1:n){
load(files_[ind])

for(i in 1:dims[1]){
for(j in 1:dims[2]){
for(k in 1:dims[3]){
 F.mar_res[i,j,k,ind,]<- results$mar[i,j,k,]
 F.res_confusions[i,j,k,ind,1]<-results$train_confusions[i,j,k][[1]]$overall[1]
 F.res_confusions[i,j,k,ind,2]<-results$test_confusions[i,j,k][[1]]$overall[1]
 #F.final_models[[i,j,k,ind]]<-results$final_models[[i,j,k]]
 F.validations[[i,j,k,ind]]<- results$validations[[i,j,k]]

}}}}

F.results<-list(confusions=F.res_confusions,
#		final_models=F.final_models,
		results =F.mar_res,
		validations = F.validations)

setwd(paste0(wd,"/reports"))
save(F.results,file="results.RData")



