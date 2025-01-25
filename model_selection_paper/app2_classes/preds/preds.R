library(ergm)
library(hergm)
library(mlergm)
library(intergraph)
library(igraph)
library(parallel)

set.seed(123)
wd<-getwd()
source('xgboost.R')
source('spectral_ex.R')
load('school_net.RData')

size<-school_net$gal$n
M <- 100
nthread <- 20

load("m1_nets.RData")
load("m2_nets.RData")
load("m3_nets.RData")
load("m4_nets.RData")

eigen_m1<-lapply(m1_nets,gspectrum,dir=TRUE,netObj=TRUE)
eigen_m2<-lapply(m2_nets,gspectrum,dir=TRUE,netObj=TRUE)
eigen_m3<-lapply(m3_nets,gspectrum,dir=TRUE,netObj=TRUE)
eigen_m4<-lapply(m4_nets,gspectrum,dir=TRUE,netObj=TRUE)


data<-c()
data<-cbind(data,t(matrix(unlist(eigen_m1), ncol = M, nrow = size+1)))
data<-rbind(data,t(matrix(unlist(eigen_m2), ncol = M, nrow = size+1)))
data<-rbind(data,t(matrix(unlist(eigen_m3), ncol = M, nrow = size+1)))
data<-rbind(data,t(matrix(unlist(eigen_m4), ncol = M, nrow = size+1)))

data<-cbind(data,c(rep(0,M),rep(1,M),rep(2,M),rep(3,M)))

colnames(data)<-c(c(1:(size+1)),"label")
rownames(data)<-c(rep(c("m1"),M),rep(c("m2"),M),rep(c("m3"),M),rep(c("m4"),M))

obs_eigen <-gspectrum(asIgraph(school_net),dir=TRUE) # observed eigenvalue
pred_table <-xg_pred(obs_eigen,data,nthread=nthread) # class predicted

save(pred_table,file="pred_table.RData")










