load('results.RData')

validations<-F.results$validations
#Sizes, Tr, M, n
K<- c(100,1000,10000)
Sizes <-c(25,50,75)
Tr<- c(5,10,15)/100 #gwesp parameters
n<- dim(F.results$confusions)[4]

train_data<-array(NA,dim=c(length(Sizes),length(Tr),length(K),n,50))
test_data<-array(NA,dim=c(length(Sizes),length(Tr),length(K),n,50))

train_data_agg<-array(NA,dim=c(length(Sizes),length(Tr),length(K),50))
test_data_agg<-array(NA,dim=c(length(Sizes),length(Tr),length(K),50))

for(i1 in 1:length(Sizes)){
  for(i2 in 1:length(Tr)){
    for(i3 in 1:length(K)){
      for(i4 in 1:n){
  train_data[i1,i2,i3,i4,] <- validations[[i1,i2,i3,i4]]$evaluation_log$train_mlogloss[1:50]
  test_data[i1,i2,i3,i4,] <- validations[[i1,i2,i3,i4]]$evaluation_log$eval_mlogloss[1:50]
      }
      for(i in 1:50){
      train_data_agg[i1,i2,i3,i] <- mean(train_data[i1,i2,i3,,i])
      test_data_agg[i1,i2,i3,i] <- mean(test_data[i1,i2,i3,,i])
      }
    }
  }
}

df = data.frame(Sizes=c(0),Tr=c(0),K=c(0),Iteration=c(0),Train=c(0),Test=c(0))

for(i1 in 1:length(Sizes)){
  for(i2 in 1:length(Tr)){
    for(i3 in 1:length(K)){
      for(i in 1:50){
          df = rbind(df,c(Sizes[i1],Tr[i2],K[i3],i,
                          train_data_agg[i1,i2,i3,i],
                          test_data_agg[i1,i2,i3,i]))
      }
    }
  }
}

df = df[-1,]

save(df,file ="df.RData")
load("df.RData")

library(ggplot2)
library(ggalt)
library(egg)
df$Sizes = factor(df$Sizes,levels=c("25","50","75"))
df$K = factor(df$K,levels=c("100","1000","10000"))

df2=data.frame(Sizes=c(df$Sizes,df$Sizes),Tr=c(df$Tr,df$Tr),K=c(df$K,df$K),
               Iteration=c(df$Iteration,df$Iteration),Logloss=c(df$Train,df$Test),
               Set=c(rep("Train set, ",length(df$Sizes)),rep("Test set, ",length(df$Sizes))))

df2$Case <- paste0(df2$Set,paste0("K = ", df2$K))
df2$Case <-factor(df2$Case,levels=c("Train set, K = 100",
                                        "Train set, K = 1000",
                                        "Train set, K = 10000",
                                        "Test set, K = 100",
                                        "Test set, K = 1000",
                                        "Test set, K = 10000"))

theme_set(theme_classic())

colfunc1 <- colorRampPalette(c("deepskyblue1", "Purple4"))
cols1<-colfunc1(3)

colfunc2 <- colorRampPalette(c("firebrick1", "red4"))
cols2<-colfunc2(3)

cols <-c(cols1,cols2)

text.size<-36
          
     plot_easy <-  ggplot(data = df2[df2$Sizes=="75" & df$Tr==0.15,]) +
        geom_line(aes(x=Iteration,y = Logloss,color = Case),linewidth = 1.2)+
        xlab("Iteration")  + ylab("Log loss") +
        scale_color_manual(values = cols) +
       theme_classic(base_size = text.size) +
       theme(legend.position="bottom") +
      guides(colour=guide_legend(nrow=3))
        

     plot_hard <- ggplot(data = df2[df2$Sizes=="25" & df$Tr==0.05,]) +
       geom_line(aes(x=Iteration,y = Logloss,color = Case),linewidth = 1.2)+
       xlab("Iteration")  + ylab("Log loss") +
       scale_color_manual(values = cols) +
       theme_classic(base_size = text.size)+
       theme(legend.position="none")
      
       
mplot_overfit<-ggarrange(plot_easy,plot_hard, nrow=1, ncol = 2)
 
 dev.off()
 cairo_pdf("overfit.pdf",width=20, height=12)
 mplot_overfit
 dev.off()

