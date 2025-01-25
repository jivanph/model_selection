library(ggplot2)
library(colorspace)
library(RColorBrewer)
library(gridExtra)
library(egg)
library(grDevices)

load('Results.RData')

Sizes <-c(25,50,75)
Tr<- seq(1,10)/10
Replicates <-1000
text.size<-30

proportions<-array(NA,dim = c(length(Sizes),length(Tr),3))

for(s in 1:length(Sizes)){
  for(t in 1:length(Tr)){
    proportions[s,t,] <- colMeans(t(F.mat_res[s,t,,2,]>0.5))
  }
}

########################################################
colfunc <- colorRampPalette(c("deepskyblue", "Purple4"))
colors<-colfunc(3)
########################################################
# Grouped results
df.25<-data.frame(x=Tr,
                     proportion = c(proportions[1,,1],proportions[1,,2],proportions[1,,3]),
                     Classifier = c(rep("Naive Bayes",length(Tr)),rep("Random Forest",length(Tr)),rep("XGBoost",length(Tr))))
df.25$ub <- pmin(1,
                 df.25$proportion+
                      qnorm(0.975)*sqrt(df.25$proportion*(1-df.25$proportion)/Replicates))
df.25$lb <- pmax(0,
                 df.25$proportion-
                      qnorm(0.975)*sqrt(df.25$proportion*(1-df.25$proportion)/Replicates))



########################################################
# Grouped results
df.50<-data.frame(x=Tr,
                  proportion = c(proportions[2,,1],proportions[2,,2],proportions[2,,3]),
                  Classifier = c(rep("Naive Bayes",length(Tr)),rep("Random Forest",length(Tr)),rep("XGBoost",length(Tr))))
df.50$ub <- pmin(1,
                 df.50$proportion+
                   qnorm(0.975)*sqrt(df.50$proportion*(1-df.50$proportion)/Replicates))

df.50$lb <- pmax(0,
                 df.50$proportion-
                   qnorm(0.975)*sqrt(df.50$proportion*(1-df.50$proportion)/Replicates))

########################################################
df.75<-data.frame(x=Tr,
                  proportion = c(proportions[3,,1],proportions[3,,2],proportions[3,,3]),
                  Classifier = c(rep("Naive Bayes",length(Tr)),rep("Random Forest",length(Tr)),rep("XGBoost",length(Tr))))
df.75$ub <- pmin(1,
                 df.75$proportion+
                   qnorm(0.975)*sqrt(df.75$proportion*(1-df.75$proportion)/Replicates))

df.75$lb <- pmax(0,
                 df.75$proportion-
                   qnorm(0.975)*sqrt(df.75$proportion*(1-df.75$proportion)/Replicates))
#########################################################
# Plots of proportions

plot_proportion_25<- ggplot() + theme_classic(base_size = text.size) +
  geom_line(data = df.25, aes(x = x, y = proportion, col = Classifier), linewidth = 1) + 
  xlab(expression(theta[2])) +  ylab("Correct Classification Rate") + ggtitle("Network Size 25") +
  geom_ribbon(data=df.25,aes(x=x,ymin=lb,ymax = ub,
                             col = Classifier, fill = Classifier),alpha=.15) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_x_continuous(limits = c(0.1, 1))  +
  theme(legend.position="none",plot.title = element_text(hjust = 0.5))

plot_proportion_50<- ggplot() + theme_classic(base_size = text.size) +
  geom_line(data = df.50, aes(x = x, y = proportion, col = Classifier), linewidth = 1) + 
  xlab(expression(theta[2])) +  ylab("") +  ggtitle("Network Size 50") +
  geom_ribbon(data=df.50,aes(x=x,ymin=lb,ymax = ub,
                             col = Classifier, fill = Classifier),alpha=.15) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_x_continuous(limits = c(0.1, 1))  +
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))

plot_proportion_75<- ggplot() + theme_classic(base_size = text.size) +
  geom_line(data = df.75, aes(x = x, y = proportion, col = Classifier), linewidth = 1) + 
  xlab(expression(theta[2])) +  ylab("") +  ggtitle("Network Size 75") +
  geom_ribbon(data=df.75,aes(x=x,ymin=lb,ymax = ub,
                             col = Classifier, fill = Classifier),alpha=.15) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_x_continuous(limits = c(0.1, 1)) +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))

cairo_pdf("plot_proportion_75.pdf",width=17, height=11)
plot_proportion_75
dev.off()

mplot_multi<-ggarrange(plot_proportion_25,plot_proportion_50,plot_proportion_75, nrow=1, ncol = 3)
cairo_pdf("mplot_multi.pdf",width=24, height=8)
mplot_multi
dev.off()


