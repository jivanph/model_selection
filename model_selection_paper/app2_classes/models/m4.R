set.seed(128452)
library(mlergm)

load("school_net.RData")
size<-school_net$gal$n

## This is actually model 4 so you need to copy this into the model 4 files 
## and then delete the terms not needed for model 1 

#ini <- mlergm(school_net ~ edges + mutual +
#                          nodeofactor("sex", levels = 1) +
#                          nodeifactor("sex", levels = 1) +
#                          nodematch("sex") +
#                          odegree(1:6) +
#                          dgwesp(fixed = TRUE, decay = 0, type = "OTP"),
#             verbose = 2, eval_loglik = FALSE, parameterization = "offset",
#             options = set_options(sample_size = 5000,
#                                   burnin = 1e+6,
#                                   interval = 1000,
#                                   adaptive_step_len = FALSE,
#                                   step_len = 0.25,
#                                   NR_max_iter = 500,
#                                   MCMLE_max_iter = 25,
#                                   number_cores = 100),
#             )
#print('Estimating real model')
#save(ini,file="ini.RData")
#load('ini.RData') 

m4 <- mlergm(school_net ~ edges + mutual +
                        nodeofactor("sex", levels = 1) +
                        nodeifactor("sex", levels = 1) + 
                        nodematch("sex") + 
                        odegree(1:6) + 
                        dgwesp(fixed = FALSE, type = "OTP"),
#	           theta_init = ini$theta,
             verbose = 2, eval_loglik = FALSE, parameterization = "offset",
             options = set_options(sample_size = 5000, 
                                   burnin = 1e+6, 
                                   interval = 3000,
	                           adaptive_step_len = FALSE,
	                           step_len = 0.25,
	                           NR_max_iter = 1000,
                                   MCMLE_max_iter = 25, 
                                   number_cores = 20) 
             )

save(m4,file="m4.RData")





