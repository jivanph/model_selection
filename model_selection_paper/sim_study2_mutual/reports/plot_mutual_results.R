set.seed(128452)

library(ggplot2)
library(ggalt)
library(grDevices)

tr <-10
p<-array(NA,dim=c(5,10,1))
text.size<-22
load('results.RData')
for(i in 1:5){
  for(t in 1:tr){
    p[i,t,1]<-mean(F.mat_res[i,t,1,1,]>0.5)
  }}

x<-seq(0,1,length=10)

colfunc <- colorRampPalette(c("deepskyblue", "Purple4"))
colors<-colfunc(5)
names(colors) <- c("50", "100", "150", "200", "250")

# Grouped results
df<-data.frame(x=x,proportion = c(p[1,,1],p[2,,1],p[3,,1],p[4,,1],p[5,,1]),
                     N = c(rep("50",tr),rep("100",tr),rep("150",tr),
                           rep("200",tr),rep("250",tr)))
df$N <- factor(df$N,levels=c(1:5)*50)


df$ub <- pmin(1,df$proportion+qnorm(0.975)*sqrt(df$proportion*(1-df$proportion)/1000))

df$lb <- pmax(0,df$proportion-qnorm(0.975)*sqrt(df$proportion*(1-df$proportion)/1000))


# Plot of proportions
plot_proportion_mutual<- ggplot() + theme_classic() +
  geom_line(data=df, aes(x=x, y=proportion, col = N), size = 2, linewidth = 1) + 
  xlab(expression(theta[2])) +  ylab("Correct Classification Rate") +
  geom_ribbon(data=df,aes(x=x,ymin=lb,ymax = ub,
                                fill = N), alpha=.25) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_x_continuous(limits = c(0, 1))  +
  theme(axis.title.y.left = element_text(size = text.size),
        axis.title.x.bottom = element_text(size = text.size),
        axis.text = element_text(size = text.size),
        axis.text.x.bottom = element_text(size = text.size),
        strip.text.y = element_text(size = text.size),
        strip.text.x = element_text(size = text.size),
        legend.text = element_text(size = text.size),
        legend.title = element_text(size = text.size)); 

cairo_pdf("mutualproportion.pdf",width=10, height=6)
plot_proportion_mutual
dev.off()

#ggsave("mutualproportion.pdf",plot_proportion_mutual, width=7, height=3.5, units="in", scale=3)




