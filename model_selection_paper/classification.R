#library(xgboost)  # the main algorithm
#library(randomForest)
#library(Rcpp)
#library(nnet)
#library(naivebayes)


predictions<-function(obs_eigen,data){
 
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
                        nthread=1)

obs_data <- t(as.matrix(obs_eigen[as.numeric(var_keep)]))
xgb_pred <- predict(final_model, newdata = obs_data,nthread=1)

#################################################################

size<-ncol(data)

colnames(data)<-c(paste0("V",c(1:(size-1))),"label")

df<-as.data.frame(data)
df$label<-as.factor(df$label)
df$label <- relevel(df$label, ref = "0")

df.obs<- as.data.frame(matrix(obs_eigen,ncol=length(obs_eigen)))

#################################################################
#mnom <- multinom(label ~ ., data = df)
#mnom_pred <- predict(mnom, df.obs,type="probs")
#################################################################
bayes <- naive_bayes(label ~ ., data = df,usekernel = T) 
bayes_pred <- predict(bayes, df.obs,type="prob")
###############################################################

rf <- randomForest(label~., data=df)
rf_pred <- predict(rf, df.obs,type="prob")
###################
#mlda <- lda(label~., data=df)
#lda_pred <- predict(mlda, df.obs,type="prob")$posterior
###################################

results <- rbind(bayes_pred,rf_pred,xgb_pred)
rownames(results)<- c("bayes","r_forest","xgboost")

 return(results)

}
