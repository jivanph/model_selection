set.seed(128452)

library(ggplot2)
library(grDevices)
load('results.RData')

Sizes<-c(50,100,150,200,250)
Dims<-c(1:5)
Type<-c("euclidean","bilinear")
Scale<-seq(1:10)/10
n<-dim(F.mat_res)[6]

dim(F.mat_res)

p<-array(NA,dim=c(5,5,2,10))

for(s in 1:length(Sizes)){
  for(d in 1:length(Dims)){
    for(t in 1:length(Type)){
      for(c in 1:length(Scale)){
        p[s,d,t,c]<-mean(F.mat_res[s,d,t,c,1,]>0.5)
    }}}}

colfunc <- colorRampPalette(c("deepskyblue", "Purple4"))
colors<-colfunc(4)
text.size<-14

# Grouped results
df1<-data.frame(x=Scale,
                proportion = c(p[1,1,1,],p[2,1,1,],p[3,1,1,],p[4,1,1,]),
                N = c(rep("50",length(Scale)),rep("100",length(Scale)),
                           rep("150",length(Scale)),rep("200",length(Scale))))
df1$ub <- pmin(1,df1$proportion+qnorm(0.975)*sqrt(df1$proportion*(1-df1$proportion)/n))

df1$lb <- pmax(0,df1$proportion-qnorm(0.975)*sqrt(df1$proportion*(1-df1$proportion)/n))

df1$N <- factor(df1$N,levels=c(1:4)*50)


# Plot of proportions
plot_proportion_1<- ggplot() + theme_classic() +
  geom_line(data=df1, aes(x=x, y=proportion, col = N),size = 2, linewidth = 1) + 
  xlab(c("Variance",expression(sigma^2))) +  ylab("Correct Classification Rate") +
  geom_ribbon(data=df1,aes(x=x,ymin=lb,ymax = ub,
                          fill = N), alpha=.25) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_x_continuous(limits = c(0.1, 1))  +
  theme(axis.title.y.left = element_text(size = text.size),
        axis.title.x.bottom = element_text(size = text.size),
        axis.text = element_text(size = text.size),
        axis.text.x.bottom = element_text(size = text.size),
        strip.text.y = element_text(size = text.size),
        strip.text.x = element_text(size = text.size),
        legend.text = element_text(size = text.size),
        legend.title = element_text(size = text.size)); 

cairo_pdf("plot_proportion_1.pdf",width=8, height=8)
plot_proportion_1
dev.off()

##########################################################################
Sizes <-rep(c(rep(50,10),rep(100,10),rep(150,10),rep(200,10),rep(250,10)),5)
Scales<-rep(c(1:10),25)/10
Fixed.Dimension <-c(rep(1,50),rep(2,50),rep(3,50),rep(4,50),rep(5,50))
Probability.Eu <- c(p[1,1,1,],p[2,1,1,],p[3,1,1,],p[4,1,1,],p[5,1,1,],
                 p[1,2,1,],p[2,2,1,],p[3,2,1,],p[4,2,1,],p[5,2,1,],
                 p[1,3,1,],p[2,3,1,],p[3,3,1,],p[4,3,1,],p[5,3,1,],
                 p[1,4,1,],p[2,4,1,],p[3,4,1,],p[4,4,1,],p[5,4,1,],
                 p[1,5,1,],p[2,5,1,],p[3,5,1,],p[4,5,1,],p[5,5,1,])
Probability.Bi <- c(p[1,1,2,],p[2,1,2,],p[3,1,2,],p[4,1,2,],p[5,1,2,],
                    p[1,2,2,],p[2,2,2,],p[3,2,2,],p[4,2,2,],p[5,2,2,],
                    p[1,3,2,],p[2,3,2,],p[3,3,2,],p[4,3,2,],p[5,3,2,],
                    p[1,4,2,],p[2,4,2,],p[3,4,2,],p[4,4,2,],p[5,4,2,],
                    p[1,5,2,],p[2,5,2,],p[3,5,2,],p[4,5,2,],p[5,5,2,])
##########################################################################
DF.Eu<-data.frame(Sizes=Sizes,
               Scales = Scales,
               Fixed.Dimension=factor(Fixed.Dimension),
               Probability.Eu=Probability.Eu)

DF.Eu$LV<- pmin(1,DF.Eu$Probability.Eu-qnorm(0.975)*sqrt(DF.Eu$Probability.Eu*(1-DF.Eu$Probability.Eu)/n))
DF.Eu$UV<- pmin(1,DF.Eu$Probability.Eu+qnorm(0.975)*sqrt(DF.Eu$Probability.Eu*(1-DF.Eu$Probability.Eu)/n))

latent_space.Eu <- ggplot(DF.Eu,aes(x=Scales,ymin=LV,ymax = UV,
                              col = Fixed.Dimension, fill = Fixed.Dimension),alpha=.15) +
  geom_ribbon() + facet_grid(vars(Fixed.Dimension),vars(Sizes)) + theme_bw() + 
  ylab("Correct Classification Rate") + xlab(c("Variance",expression(sigma^2))) + 
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Fixed Dimension", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Size", breaks = NULL, labels = NULL)) +
  theme(axis.title.y = element_text(size = text.size),
        axis.title.x = element_text(size = text.size),
        axis.text = element_text(size = 7.5),
        legend.position = "none")

cairo_pdf("latent_space_Eu.pdf",width=8, height=5)
latent_space.Eu
dev.off()

################################################################
# DF.Bi<-data.frame(Sizes=Sizes,
#                   Scales = Scales,
#                   Fixed.Dimension=factor(Fixed.Dimension),
#                   Probability.Bi=1-Probability.Bi)
# 
# DF.Bi$LV<- pmin(1,DF.Bi$Probability.Bi-qnorm(0.975)*sqrt(DF.Bi$Probability.Bi*(1-DF.Bi$Probability.Bi)/n))
# DF.Bi$UV<- pmin(1,DF.Bi$Probability.Bi+qnorm(0.975)*sqrt(DF.Bi$Probability.Bi*(1-DF.Bi$Probability.Bi)/n))
# 
# latent_space.Bi <- ggplot(DF.Bi,aes(x=Scales,ymin=LV,ymax = UV,
#                                     col = Fixed.Dimension, fill = Fixed.Dimension),alpha=.15) +
#   geom_ribbon() + facet_grid(vars(Fixed.Dimension),vars(Sizes)) + theme_bw() + 
#   ylab("Correct Classification Rate") + xlab(c("Variance",expression(sigma^2))) + 
#   scale_y_continuous(sec.axis = sec_axis(~ . , name = "Fixed Dimension", breaks = NULL, labels = NULL)) +
#   scale_x_continuous(sec.axis = sec_axis(~ . , name = "Size", breaks = NULL, labels = NULL)) +
#   theme(axis.title.y = element_text(size = text.size),
#         axis.title.x = element_text(size = text.size),
#         axis.text = element_text(size = 7.5),
#         legend.position = "none")
# 
# cairo_pdf("latent_space_Bi.pdf",width=8, height=5)
# latent_space.Bi
# dev.off()


