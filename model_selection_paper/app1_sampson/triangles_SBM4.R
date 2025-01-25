set.seed(128452)
library(ggplot2)
library(intergraph)
library(latentnet)

text.size<-23

load('nets_LSM.RData')
data(sampson)
net <- samplike 
delete.edge.attribute(net, "nominations")

trig <- sapply(nets_LSM[[4]], function(x) {summary(x ~ triangles) })
df<-data.frame(Triangles=trig)

obs.trig <- summary(net ~ triangles) 
df2<-data.frame(ObsTrig=obs.trig)

plot_triangles <- ggplot(df, aes(x=Triangles)) + geom_histogram(position="identity",bins=25,color="black", fill="grey") +
  geom_vline(data=df2, aes(xintercept=ObsTrig),color="red",linewidth=1) +  theme_classic(base_size = text.size) +
  xlab("Number of Triangles") + ylab("Count") + scale_y_continuous(expand = c(0, 0))

cairo_pdf("plot_triangles.pdf",width=10, height=6)
plot_triangles
dev.off()




