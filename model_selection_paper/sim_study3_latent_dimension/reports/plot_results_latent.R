set.seed(128452)

library(ggplot2)
library(MultinomialCI)

sizes<-c(50,100,150,200,250)
dims<-seq(1,5)
M<-1000
p<-array(NA,dim=c(5,5,M))

load('results.RData')
# Column 1 is size
# Column 2 is true dimension
# Column 3 is propensity score
# Column 4 is an independent experiment

for(s in 1:length(sizes)){
  for(t in 1:length(dims)){
    for(m in 1:M){
      p[s,t,m]<-which.max(F.mat_res[s,t,,m])
    }}}

count<-matrix(0,nrow =length(sizes)*5,ncol=length(dims))

for(s in 1:length(sizes)){
  for(t in 1:length(dims)){
    count[(t-1)*5+s,as.numeric(rownames(table(p[s,t,])))] <-table(p[s,t,])
  }}

heat<-count/1000
rownames(heat)<-rep(sizes,5)
colnames(heat)<-rep(dims)

Sizes<-rep(c(rep(50,5),rep(100,5),rep(150,5),rep(200,5),rep(250,5)),5)
Dimensions<-rep(c(1:5),25)
True.Dimension<-c(rep(1,25),rep(2,25),rep(3,25),rep(4,25),rep(5,25))

data_heat<-data.frame(Sizes=Sizes,
                      Dimensions=factor(Dimensions),
                      True.Dimension=factor(True.Dimension),
                      Probability=as.numeric(t(heat)))
LV<-c()
UV<-c()

for(i in 1:nrow(heat)){
  
  x<-multinomialCI(count[i,],alpha=0.05)
  LV<-c(LV,x[,1])
  UV<-c(UV,x[,2])
}

data_heat$LV<-LV
data_heat$UV<-UV

text.size<-14

latent<-ggplot(data_heat, aes(x = Sizes, y = Probability, ymin = LV, ymax = UV,
                      col = True.Dimension, fill = True.Dimension),alpha=.15) +
 geom_errorbar(width = 10) +  geom_point(size = 1) +  geom_line() +
facet_grid(vars(True.Dimension),vars(Dimensions)) + theme_bw() + 
  ylab("Selection Rate") + 
  xlab("Network Size") + 
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "True Dimension of Latent Space", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Dimension Selected", breaks = NULL, labels = NULL)) +
  theme(axis.title.y = element_text(size = text.size),
        axis.title.x = element_text(size = text.size),
        axis.text = element_text(size = 7.5),
        legend.position = "none")

cairo_pdf("latent.pdf",width=8, height=5)
latent3
dev.off()




