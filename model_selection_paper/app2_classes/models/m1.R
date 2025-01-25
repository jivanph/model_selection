set.seed(128452)
library(mlergm)

load("school_net.RData")
size<-school_net$gal$n

m1 <- mlergm(school_net ~ edges + mutual +
               nodeofactor("sex", levels = 1) + nodeifactor("sex", levels = 1)+
	       nodematch("sex")+odegree(1:6), 
             verbose = 2, eval_loglik = FALSE, parameterization = "offset",
             options = set_options(sample_size = 1000, 
                                   burnin = 1000, 
                                   interval = 1000, 
                                   number_cores = 25));
save(m1,file="m1.RData")





