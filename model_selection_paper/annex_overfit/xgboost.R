#library(xgboost)  # the main algorithm
#library(e1071)
#library(caret)    # for the confusionmatrix() function (also needs e1071 package)
#library(dplyr)    # for some data preperation
#library(Rcpp)

xg_pred<-function(obs_eigen,data,nthread=1){
 
data_variables <- as.matrix(data[, colnames(data) != "label"])
data_label <- data[,"label"]
full_matrix <- xgb.DMatrix(data = data_variables, label = data_label)

numberOfClasses<-length(unique(data[,"label"]))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
#"eta" =  0.05 took too long

#
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

# Make split index
train_index <- sample(1:nrow(data), floor(nrow(data)*0.75))
# split train data and make xgb.DMatrix
train_data   <- as.matrix(data_variables_final[train_index,])
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# split test data and make xgb.DMatrix
test_data  <- as.matrix(data_variables_final[-train_index,])
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)
#########################################################
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround,
		       watchlist = list(train=train_matrix, eval=test_matrix),
			nthread=1)

#########################################################
# Predict in-sample train set
train_pred <- predict(bst_model, newdata = train_matrix,
                        nthread=1)
train_prediction <- matrix(train_pred, nrow = numberOfClasses,
                          ncol=length(train_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = train_label+1,
         max_prob = max.col(., "last"))

# confusion matrix of test set
train_CM<-confusionMatrix(factor(train_prediction$max_prob),
                factor(train_prediction$label),
                mode = "everything")


# Predict hold-out test set
test_pred <- predict(bst_model, newdata = test_matrix,
			nthread=1)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label+1,
         max_prob = max.col(., "last"))

# confusion matrix of test and training set
test_CM<-confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")

############################################################

design_matrix <- xgb.DMatrix(data = data_variables_final, label = data_label)
final_model <- xgb.train(params = xgb_params,
                        data = design_matrix,
                        nrounds = nround,
                        nthread=nthread)

obs_data <- t(as.matrix(obs_eigen[as.numeric(var_keep)]))
test_pred <- predict(final_model, newdata = obs_data,nthread=1)

 results<-list()
 results$pred<-test_pred
 results$num_keep<-num_keep
 results$var_keep <-var_keep
 results$test_CM <- test_CM
 results$train_CM <- train_CM
 results$validation <- bst_model
 return(results)

}
#new_data <- xgb.DMatrix(data = t(as.matrix(obs_eigen_sbm)))
#test_pred <- predict(bst_model, newdata = new_data)
