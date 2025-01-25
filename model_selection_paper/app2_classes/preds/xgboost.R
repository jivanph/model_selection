library(xgboost)  # the main algorithm
#library("archdata") # for the sample dataset
#library("caret")    # for the confusionmatrix() function (also needs e1071 package)
#library("dplyr")    # for some data preperation
library(Rcpp)
#library("Ckmeans.1d.dp") # for xgb.ggplot.importance

xg_pred<-function(obs_eigen,data,nthread=1){
 
data_variables <- as.matrix(data[, colnames(data) != "label"])
data_label <- data[,"label"]
full_matrix <- xgb.DMatrix(data = data_variables, label = data_label)

numberOfClasses<-length(unique(data[,"label"]))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround <- 50;
full_model <- xgb.train(params = xgb_params,
                       data = full_matrix,
                       nrounds = nround,
		       nthread=1)

names <-  colnames(data)[colnames(data) != "label"]
importance_matrix <- xgb.importance(feature_names = names, model = full_model)
num_keep <- which.max(cumsum(importance_matrix$Gain)>0.95)
var_keep <- importance_matrix$Feature[1:num_keep]
data_variables_final <- as.matrix(data_variables[,var_keep])

design_matrix <- xgb.DMatrix(data = data_variables_final, label = data_label)
final_model <- xgb.train(params = xgb_params,
                        data = design_matrix,
                        nrounds = nround,
                        nthread=nthread)

obs_data <- t(as.matrix(obs_eigen[as.numeric(var_keep)]))
test_pred <- predict(final_model, newdata = obs_data,nthread=nthread)

 results<-list()
 results$pred<-test_pred
 results$num_keep<-num_keep
 results$var_keep <-var_keep
 return(results)

}
#new_data <- xgb.DMatrix(data = t(as.matrix(obs_eigen_sbm)))
#test_pred <- predict(bst_model, newdata = new_data)
