set.seed(128452)
library(mlergm)
load("school_net.RData")
size<-school_net$gal$n

m2 <- mlergm(school_net ~ edges + mutual +
                          nodeofactor("sex", levels = 1) +
                          nodeifactor("sex", levels = 1) +
                          nodematch("sex") +
                          odegree(1:6) +
                          dgwesp(fixed = TRUE, decay = 0, type = "OTP"),
             #theta_init = c(0,-1.5,0,0,1.5),
             verbose = 2, eval_loglik = FALSE, parameterization = "offset",
             options = set_options(sample_size = 5000,
                                   burnin = 1e+6,
                                   interval = 1000,
                                   adaptive_step_len = FALSE,
                                   step_len = 0.25,
                                   NR_max_iter = 500,
                                   MCMLE_max_iter = 25,
                                   number_cores = 100),
             )

save(m2,file="m2.RData")

