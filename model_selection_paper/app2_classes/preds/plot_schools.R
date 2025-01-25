###################################################################
### Plots
set.seed(123)
library(ggplot2)
library(intergraph)
library(gridExtra)
library(egg)
library(ergm)
library(grDevices)
library(GGally)
library(RColorBrewer)

load("m1_nets.RData")
load("m2_nets.RData")
load("m3_nets.RData")
load("m4_nets.RData")

text.size<-40
max.deg<-7
max.esp<-6
M<-length(m1_nets)

load("school_net.RData")

##########################################
names(y) = levels(x)

n <- 44
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'div',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

#sample(col_vector, n)
colors <- col_vector[c(1:44)*2+11]
cols = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
set.seed(5)
colors1=sample(cols, 44)
colors1[19]<-"darkgrey"
colors<-rep(colors1,as.vector(table(get.node.attr(school_net,'node_memb'))))
school_plot<-ggnet2(school_net, node.size = 2.5,edge.size = 0.02,
                    node.color = colors,
                    edge.color = "gray")
school_plot

cairo_pdf("school_net_plot.pdf",width=8, height=8)
school_plot
dev.off()

##########################################

df.ideg<-function(models,max.deg,M){
  deg <- sapply(models, function(x) {summary(x ~ idegree(1:max.deg)) }); deg <- t(deg)
  degrees<-sub(".*degree", "", colnames(deg))
  Degree<-c(); for(i in 1:length(degrees)){Degree<-c(Degree,rep(degrees[i],M))}
  
  DF<-data.frame(Degree=Degree,Count=as.vector(deg))
  DF$Degree <- factor(DF$Degree,levels=c(1:max.deg))
  return(DF)
}

df_m1.ideg <- df.ideg(m1_nets,max.deg,M); df_m1.ideg$Model<-rep("M1",max.deg*M)
df_m2.ideg <- df.ideg(m2_nets,max.deg,M); df_m2.ideg$Model<-rep("M2",max.deg*M)
df_m3.ideg <- df.ideg(m3_nets,max.deg,M); df_m3.ideg$Model<-rep("M3",max.deg*M)
df_m4.ideg <- df.ideg(m4_nets,max.deg,M); df_m4.ideg$Model<-rep("M4",max.deg*M)

df_ideg<-rbind(df_m1.ideg,df_m2.ideg,df_m3.ideg,df_m4.ideg)
df_ideg$Degree <- factor(df_ideg$Degree,levels=c(1:max.deg))
df_ideg$Model <- factor(df_ideg$Model)

DFobs_ideg<-data.frame(Degree=c(1:max.deg),Count=summary(school_net ~ idegree(1:max.deg)))
DFobs_ideg$Degree <- factor(DFobs_ideg$Degree)
DFobs_ideg$Model <- "Observed"

for(i in 1:max.deg){
  df_ideg$Count[df_ideg$Degree==i]<-df_ideg$Count[df_ideg$Degree==i]-DFobs_ideg$Count[DFobs_ideg$Degree==i]
}

##########################################
# 
# df.odeg<-function(models,max.deg,M){
#   deg <- sapply(models, function(x) {summary(x ~ odegree(1:max.deg)) }); deg <- t(deg)
#   degrees<-sub(".*degree", "", colnames(deg))
#   Degree<-c(); for(i in 1:length(degrees)){Degree<-c(Degree,rep(degrees[i],M))}
#   
#   DF<-data.frame(Degree=Degree,Count=as.vector(deg))
#   DF$Degree <- factor(DF$Degree,levels=c(1:max.deg))
#   return(DF)
# }
# 
# df_m1.odeg <- df.odeg(m1_nets,max.deg,M); df_m1.odeg$Model<-rep("M1",max.deg*M)
# df_m2.odeg <- df.odeg(m2_nets,max.deg,M); df_m2.odeg$Model<-rep("M2",max.deg*M)
# df_m3.odeg <- df.odeg(m3_nets,max.deg,M); df_m3.odeg$Model<-rep("M3",max.deg*M)
# df_m4.odeg <- df.odeg(m4_nets,max.deg,M); df_m4.odeg$Model<-rep("M4",max.deg*M)
# 
# df_odeg<-rbind(df_m1.odeg,df_m2.odeg,df_m3.odeg,df_m4.odeg)
# df_odeg$Degree <- factor(df_odeg$Degree,levels=c(1:max.deg))
# df_odeg$Model <- factor(df_odeg$Model)
# 
# DFobs_odeg<-data.frame(Degree=c(1:max.deg),Count=summary(school_net ~ odegree(1:max.deg)))
# DFobs_odeg$Degree <- factor(DFobs_odeg$Degree)
# DFobs_odeg$Model <- "Observed"
# 
# for(i in 1:max.deg){
#   df_odeg$Count[df_odeg$Degree==i]<-df_odeg$Count[df_odeg$Degree==i]-DFobs_odeg$Count[DFobs_odeg$Degree==i]
# }

##########################################

df.esp<-function(models,max.esp,M){
  esp <- sapply(models, function(x) {summary(x ~ esp(1:max.esp)) }); esp <- t(esp)
  esps<-sub(".*esp", "", colnames(esp))
  ESP<-c(); for(i in 1:length(esps)){ESP<-c(ESP,rep(esps[i],M))}
  
  DF<-data.frame(ESP=ESP,Count=as.vector(esp))
  DF$ESP <- factor(DF$ESP,levels=c(1:max.esp))
  return(DF)
}

df_m1.esp <- df.esp(m1_nets,max.esp,M); df_m1.esp$Model<-rep("M1",max.esp*M)
df_m2.esp <- df.esp(m2_nets,max.esp,M); df_m2.esp$Model<-rep("M2",max.esp*M)
df_m3.esp <- df.esp(m3_nets,max.esp,M); df_m3.esp$Model<-rep("M3",max.esp*M)
df_m4.esp <- df.esp(m4_nets,max.esp,M); df_m4.esp$Model<-rep("M4",max.esp*M)

df_esp<-rbind(df_m1.esp,df_m2.esp,df_m3.esp,df_m4.esp)
df_esp$ESP <- factor(df_esp$ESP,levels=c(1:max.esp))
df_esp$Model <- factor(df_esp$Model)

DFobs_esp<-data.frame(ESP=c(1:max.esp),Count=summary(school_net ~ esp(1:max.esp)))
DFobs_esp$ESP <- factor(DFobs_esp$ESP)
DFobs_esp$Model <- "Observed"

for(i in 1:max.esp){
  df_esp$Count[df_esp$ESP==i]<-df_esp$Count[df_esp$ESP==i]-DFobs_esp$Count[DFobs_esp$ESP==i]
}

#############################################################################################


# cairo_pdf("ideg_plot.pdf",width=8, height=8)
# ideg_plot
# dev.off()
# 
# odeg_plot<- ggplot(df_odeg, aes(x=Degree, y=Count,fill=Model)) + geom_boxplot(outlier.shape = NA) + 
#   theme_classic(base_size = text.size) + scale_fill_brewer(palette="BuPu") + 
#   geom_hline(yintercept=0, linetype="dashed", color = "red",linewidth=1.5) +
#   xlab("Out-degree") + ylab("Difference to Observed Network") +
#   theme(legend.position="none")
# 
# cairo_pdf("odeg_plot.pdf",width=8, height=8)
# odeg_plot
# dev.off()

ideg_plot<- ggplot(df_ideg, aes(x=Degree, y=Count,fill=Model)) + geom_boxplot(outlier.shape = NA) + 
  theme_classic(base_size = text.size) + scale_fill_brewer(palette="BuPu") + 
  geom_hline(yintercept=0, linetype="dashed", color = "red",linewidth=1.5) +
  ylab("Difference to Observed Network") + xlab("In-degree") + 
  theme(legend.position="none")

esp_plot<- ggplot(df_esp, aes(x=ESP, y=Count,fill=Model)) + geom_boxplot(outlier.shape = NA) + 
  theme_classic(base_size = text.size) + scale_fill_brewer(palette="BuPu") + 
  geom_hline(yintercept=0, linetype="dashed", color = "red",linewidth=1.5) +
  xlab("Edgewise Shared Partners") + ylab("Difference to Observed Network") + 
  theme(legend.position="right")

# cairo_pdf("esp_plot.pdf",width=12.1, height=12.1)
# esp_plot
# dev.off()

mplot_schools<-ggarrange(ideg_plot,esp_plot, nrow=1, ncol = 2)
cairo_pdf("mplotschools.pdf",width=20, height=10)
mplot_schools
dev.off()


