library(ggplot2)
library(ggalt)

text.size<-30
alpha <- 0.05
load('results.Rdata')

sizes<-c(100,200,300)
n <- dim(F.mat_res)[4]
tr <- dim(F.mat_res)[2]

p<-array(NA,dim=c(3,tr))

for(s in 1:length(sizes)){
  for(t in 1:tr){
    p[s,t]<-mean(F.mat_res[s,t,2,]>0.5)
  }}

x<- seq(1,50)/500

colfunc <- colorRampPalette(c("deepskyblue", "Purple4"))
colors<-colfunc(3)

# Grouped results
large.df<-data.frame(x=x,
                     proportion = c(p[1,],p[2,],p[3,]),
                     N = c(rep("100",tr),rep("200",tr),rep("300",tr)))
large.df$ub <- pmin(1,
     large.df$proportion+
       qnorm(1 - alpha / 2)*sqrt(large.df$proportion*(1-large.df$proportion)/n))

large.df$lb <- pmax(0,
     large.df$proportion-
       qnorm(1 - alpha / 2)*sqrt(large.df$proportion*(1-large.df$proportion)/n))

# Plot of proportions
plot_proportion_large<- ggplot() + theme_classic() +
  geom_line(data = large.df, aes(x = x, y = proportion, col = N), linewidth = 1) + 
  #geom_xspline(data=large.df, aes(x=x,y = proportion,col=N),
  #             spline_shape=2, size=1)+
  xlab(expression(theta[2])) +  ylab("Correct Classification Rate") +
  geom_ribbon(data=large.df,aes(x=x,ymin=lb,ymax = ub,
                                col = N, fill = N),alpha=.15) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_x_continuous(limits = c(0, 0.1))  +
  theme(axis.title.y = element_text(size = text.size),
        axis.title.x = element_text(size = text.size + 2),
        axis.text = element_text(size = text.size - 4),
        strip.text.y = element_text(size = text.size),
        strip.text.x = element_text(size = text.size),
        legend.text = element_text(size = text.size - 4),
        legend.title = element_text(size = text.size)); 

cairo_pdf("gwesplargeproportion5000.pdf",width=8, height=8)
plot_proportion_large
dev.off()

#ggsave("gwesplargeproportion.pdf",plot_proportion_large, width=3, height=3, units="in", scale=3)

#plot_proportion_large

# Plot of propensities
#plot_propensity_large<- ggplot() +  theme_classic() +
#  geom_xspline(data=large.df, aes(x=x,y = propensity, col=size),
#               spline_shape=1, size=0.9) +  scale_color_manual(values=colors) +
#  xlab(expression(theta[2])) + ylab("Median propensity") + 
#  geom_ribbon(data=large.df,aes(x=x,ymin=lb,ymax=ub,col=size),alpha=.15) +
#  scale_x_continuous(limits = c(0, 0.1))  +
#  theme(axis.title.y.left = element_text(size = text.size),
#        axis.title.x.bottom = element_text(size = text.size),
#        axis.text = element_text(size = text.size),
#        axis.text.x.bottom = element_text(size = text.size),
#        strip.text.y = element_text(size = text.size),
#        strip.text.x = element_text(size = text.size),
#        legend.text = element_text(size = text.size),
#        legend.title = element_text(size = text.size)); 
#ggsave("gwesplargepropensity.pdf",plot_propensity_large, width=3, height=3, units="in", scale=3)

# Individual plots
# large.df2<-data.frame(x=x,
#                       proportion100 = p[,1,1], propensity100 = p[,2,1],
#                       lb100 = p[,3,1], ub100 = p[,4,1],
#                       lb100.2 = pmax(0,p[,1,1]-qnorm(0.975)*sqrt(p[,1,1]*(1-p[,1,1])/1000)),
#                       ub100.2 = pmin(1,p[,1,1]+qnorm(0.975)*sqrt(p[,1,1]*(1-p[,1,1])/1000)),
#                       proportion200 = p[,1,2], propensity200 = p[,2,2],
#                       lb200 = p[,3,2], ub200 = p[,4,2],
#                       lb200.2 = pmax(0,p[,1,2]-qnorm(0.975)*sqrt(p[,1,2]*(1-p[,1,2])/1000)),
#                       ub200.2 = pmin(1,p[,1,2]+qnorm(0.975)*sqrt(p[,1,2]*(1-p[,1,2])/1000)),    
#                       proportion300 = p[,1,3], propensity300 = p[,2,3],
#                       lb300 = p[,3,3], ub300 = p[,4,3],
#                       lb300.2 = pmax(0,p[,1,3]-qnorm(0.975)*sqrt(p[,1,3]*(1-p[,1,3])/1000)),
#                       ub300.2 = pmin(1,p[,1,3]+qnorm(0.975)*sqrt(p[,1,3]*(1-p[,1,3])/1000)))
# 
# plot_proportion.100<- ggplot() +  theme_classic() +
#   geom_xspline(data=large.df2, aes(x=x,y = proportion100),
#                spline_shape=1, size=0.9, col="blue")+
#   xlab(expression(theta[2])) + ylab("Correct classification rate") +
#   geom_ribbon(data=large.df2,aes(x=x,ymin=lb100.2,ymax=ub100.2),alpha=.15) +
#   scale_x_continuous(limits = c(0, 0.1)) + geom_hline(yintercept=1) +
#   theme(axis.title.y.left = element_text(size = 14),
#         axis.title.x.bottom = element_text(size = 14),
#         axis.text = element_text(size = 11),
#         axis.text.x.bottom = element_text(size = 11),
#         strip.text.y = element_text(size = 12),
#         strip.text.x = element_text(size = 12),
#         legend.text = element_text(size = 12)); plot_proportion.100
# 
# plot_proportion.200<- ggplot() +  theme_classic() +
#   geom_xspline(data=large.df2, aes(x=x,y = proportion200),
#                spline_shape=1, size=0.9, col="blue")+
#   xlab(expression(theta[2])) + ylab("Correct classification rate") +
#   geom_ribbon(data=large.df2,aes(x=x,ymin=lb200.2,ymax=ub200.2),alpha=.15) +
#   scale_x_continuous(limits = c(0, 0.1)) + geom_hline(yintercept=1) +
#   theme(axis.title.y.left = element_text(size = 14),
#         axis.title.x.bottom = element_text(size = 14),
#         axis.text = element_text(size = 11),
#         axis.text.x.bottom = element_text(size = 11),
#         strip.text.y = element_text(size = 12),
#         strip.text.x = element_text(size = 12),
#         legend.text = element_text(size = 12)); plot_proportion.200
# 
# plot_proportion.300<- ggplot() +  theme_classic() +
#   geom_xspline(data=large.df2, aes(x=x,y = proportion300),
#                spline_shape=1, size=0.9, col="blue")+
#   xlab(expression(theta[2])) + ylab("Correct classification rate") +
#   geom_ribbon(data=large.df2,aes(x=x,ymin=lb300.2,ymax=ub300.2),alpha=.15) +
#   scale_x_continuous(limits = c(0, 0.1)) + geom_hline(yintercept=1) +
#   theme(axis.title.y.left = element_text(size = 14),
#         axis.title.x.bottom = element_text(size = 14),
#         axis.text = element_text(size = 11),
#         axis.text.x.bottom = element_text(size = 11),
#         strip.text.y = element_text(size = 12),
#         strip.text.x = element_text(size = 12),
#         legend.text = element_text(size = 12)); plot_proportion.300
# 
# plot_propensity.100<- ggplot() +  theme_classic() +
#   geom_xspline(data=large.df2, aes(x=x,y = propensity100),
#                spline_shape=1, size=0.9, col="blue")+
#   xlab(expression(theta[2])) + ylab("Correct classification rate") +
#   geom_ribbon(data=large.df2,aes(x=x,ymin=lb100,ymax=ub100),alpha=.15) +
#   scale_x_continuous(limits = c(0, 0.1)) + geom_hline(yintercept=1) +
#   theme(axis.title.y.left = element_text(size = 14),
#         axis.title.x.bottom = element_text(size = 14),
#         axis.text = element_text(size = 11),
#         axis.text.x.bottom = element_text(size = 11),
#         strip.text.y = element_text(size = 12),
#         strip.text.x = element_text(size = 12),
#         legend.text = element_text(size = 12)); plot_propensity.100
# 
# plot_propensity_small.200<- ggplot() +  theme_classic() +
#   geom_xspline(data=large.df2, aes(x=x,y = propensity200),
#                spline_shape=1, size=0.9, col="blue")+
#   xlab(expression(theta[2])) + ylab("Correct classification rate") +
#   geom_ribbon(data=large.df2,aes(x=x,ymin=lb200,ymax=ub200),alpha=.15) +
#   scale_x_continuous(limits = c(0, 0.1)) + geom_hline(yintercept=1) +
#   theme(axis.title.y.left = element_text(size = 14),
#         axis.title.x.bottom = element_text(size = 14),
#         axis.text = element_text(size = 11),
#         axis.text.x.bottom = element_text(size = 11),
#         strip.text.y = element_text(size = 12),
#         strip.text.x = element_text(size = 12),
#         legend.text = element_text(size = 12)); plot_propensity_small.200
# 
# plot_propensity.300 <- ggplot() +  theme_classic() +
#   geom_xspline(data=large.df2, aes(x=x,y = propensity300),
#                spline_shape=1, size=0.9, col="blue")+
#   xlab(expression(theta[2])) + ylab("Correct classification rate") +
#   geom_ribbon(data=large.df2,aes(x=x,ymin=lb300,ymax=ub300),alpha=.15) +
#   scale_x_continuous(limits = c(0, 0.1)) + geom_hline(yintercept=1) +
#   theme(axis.title.y.left = element_text(size = 14),
#         axis.title.x.bottom = element_text(size = 14),
#         axis.text = element_text(size = 11),
#         axis.text.x.bottom = element_text(size = 11),
#         strip.text.y = element_text(size = 12),
#         strip.text.x = element_text(size = 12),
#         legend.text = element_text(size = 12)); plot_propensity.300
# 
