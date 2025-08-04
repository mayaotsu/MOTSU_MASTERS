rm(list = ls())
library(gbm)

###################
#### taape full ###
###################
df <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full_07.21")
df <- df[df$species=="LUKA",]

taape_full_brt = load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/07.21/taape/taape_full_reduced_0.001_0.75_07.21.Rdata")

#identfy predictor variables used in BRT
predictors <- PA_Model_Reduced[[1]][[1]]$gbm.call$predictor.names

#filter dataset to matching predictors
df_predict <- df[, predictors]
df_predict <- na.omit(df_predict)  # make sure there are no missing values

#generate predictions from each of the 50 brt models
Model_Estimates <- matrix(NA, nrow = nrow(df_predict), ncol = 50)

for (k in 1:50) {
  Model_Estimates[,k] <- predict.gbm(
    PA_Model_Reduced[[1]][[k]],
    newdata = df_predict,
    n.trees = PA_Model_Reduced[[1]][[k]]$gbm.call$best.trees,
    type = "response"
  )
  print(paste("Completed", k, "of 50"))
}

#add predictions to df
df_predict$brt_mean_pred <- rowMeans(Model_Estimates)

#save df
saveRDS(df_predict, "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/predict/taape_full_brt_predictions.rds")
taape_full_predicted = readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/predict/taape_full_predicted.rds")
cor(df_predict$brt_mean_pred, taape_full_predicted$fit_prob) #0.9482562

rm(list = ls())
###################
#### taape mhi ###
###################
df <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi_07.21")
df <- df[df$species == "LUKA" & df$region == "MHI",]

taape_mhi_brt = load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/07.21/taape/taape_mhi_reduced_0.001_0.75_07.21.Rdata")

#identfy predictor variables used in BRT
predictors <- PA_Model_Reduced[[1]][[1]]$gbm.call$predictor.names

#filter dataset to matching predictors
df_predict <- df[, predictors]
df_predict <- na.omit(df_predict)  # make sure there are no missing values

#generate predictions from each of the 50 brt models
Model_Estimates <- matrix(NA, nrow = nrow(df_predict), ncol = 50)

for (k in 1:50) {
  Model_Estimates[,k] <- predict.gbm(
    PA_Model_Reduced[[1]][[k]],
    newdata = df_predict,
    n.trees = PA_Model_Reduced[[1]][[k]]$gbm.call$best.trees,
    type = "response"
  )
  print(paste("Completed", k, "of 50"))
}

#add predictions to df
df_predict$brt_mean_pred <- rowMeans(Model_Estimates)

#save df
saveRDS(df_predict, "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/predict/taape_mhi_brt_predictions.rds")
taape_MHI_predicted = readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/predict/taape_MHI_predicted.rds")
cor(df_predict$brt_mean_pred, taape_MHI_predicted$fit_prob) #0.9411649 --> 0.9426508

###################
#### toau full ###
###################
rm(list = ls())
df <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full_07.21")
df <- df[df$species=="LUFU",]

toau_full_brt = load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/07.21/toau/toau_full_reduced_0.001_0.75_07.21.Rdata")

#identfy predictor variables used in BRT
predictors <- PA_Model_Reduced[[1]][[1]]$gbm.call$predictor.names

#filter dataset to matching predictors
df_predict <- df[, predictors]
df_predict <- na.omit(df_predict)  # make sure there are no missing values

#generate predictions from each of the 50 brt models
Model_Estimates <- matrix(NA, nrow = nrow(df_predict), ncol = 50)

for (k in 1:50) {
  Model_Estimates[,k] <- predict.gbm(
    PA_Model_Reduced[[1]][[k]],
    newdata = df_predict,
    n.trees = PA_Model_Reduced[[1]][[k]]$gbm.call$best.trees,
    type = "response"
  )
  print(paste("Completed", k, "of 50"))
}

#add predictions to df
df_predict$brt_mean_pred <- rowMeans(Model_Estimates)

#save df
saveRDS(df_predict, "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/predict/toau_full_brt_predictions.rds")
toau_full_predicted = readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/predict/toau_full_predicted.rds")
cor(df_predict$brt_mean_pred, toau_full_predicted$fit_prob) #0.9434286 --> 0.9485918

rm(list = ls())
###################
#### toau mhi ###
###################
df <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi_07.21")
df <- df[df$species == "LUKA" & df$region == "MHI",]

toau_mhi_brt = load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/07.21/toau/toau_mhi_reduced_0.001_0.75_07.21.Rdata")

#identfy predictor variables used in BRT
predictors <- PA_Model_Reduced[[1]][[1]]$gbm.call$predictor.names

#filter dataset to matching predictors
df_predict <- df[, predictors]
df_predict <- na.omit(df_predict)  # make sure there are no missing values

#generate predictions from each of the 50 brt models
Model_Estimates <- matrix(NA, nrow = nrow(df_predict), ncol = 50)

for (k in 1:50) {
  Model_Estimates[,k] <- predict.gbm(
    PA_Model_Reduced[[1]][[k]],
    newdata = df_predict,
    n.trees = PA_Model_Reduced[[1]][[k]]$gbm.call$best.trees,
    type = "response"
  )
  print(paste("Completed", k, "of 50"))
}

#add predictions to df
df_predict$brt_mean_pred <- rowMeans(Model_Estimates)

#save df
saveRDS(df_predict, "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/predict/toau_mhi_brt_predictions.rds")
toau_MHI_predicted = readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/predict/toau_MHI_predicted.rds")
cor(df_predict$brt_mean_pred, toau_MHI_predicted$fit_prob) #0.9177597 --> 0.91922462

###################
#### roi full ###
###################
rm(list = ls())
df <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full_07.21")
df <- df[df$species=="CEAR",]

roi_full_brt = load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/07.21/roi/roi_full_reduced_0.001_0.75_07.21.Rdata")

#identfy predictor variables used in BRT
predictors <- PA_Model_Reduced[[1]][[1]]$gbm.call$predictor.names

#filter dataset to matching predictors
df_predict <- df[, predictors]
df_predict <- na.omit(df_predict)  # make sure there are no missing values

#generate predictions from each of the 50 brt models
Model_Estimates <- matrix(NA, nrow = nrow(df_predict), ncol = 50)

for (k in 1:50) {
  Model_Estimates[,k] <- predict.gbm(
    PA_Model_Reduced[[1]][[k]],
    newdata = df_predict,
    n.trees = PA_Model_Reduced[[1]][[k]]$gbm.call$best.trees,
    type = "response"
  )
  print(paste("Completed", k, "of 50"))
}

#add predictions to df
df_predict$brt_mean_pred <- rowMeans(Model_Estimates)

#save df
saveRDS(df_predict, "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/predict/roi_full_brt_predictions.rds")
roi_full_predicted = readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/predict/roi_full_predicted.rds")
cor(df_predict$brt_mean_pred, roi_full_predicted$fit_prob) #0.971458 --> 0.9806029

rm(list = ls())
###################
#### roi mhi ###
###################
df <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_mhi_07.21")
df <- df[df$species == "CEAR" & df$region == "MHI",]

roi_mhi_brt = load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/07.21/roi/roi_mhi_reduced_0.001_0.75_07.21.Rdata")

#identfy predictor variables used in BRT
predictors <- PA_Model_Reduced[[1]][[1]]$gbm.call$predictor.names

#filter dataset to matching predictors
df_predict <- df[, predictors]
df_predict <- na.omit(df_predict)  # make sure there are no missing values

#generate predictions from each of the 50 brt models
Model_Estimates <- matrix(NA, nrow = nrow(df_predict), ncol = 50)

for (k in 1:50) {
  Model_Estimates[,k] <- predict.gbm(
    PA_Model_Reduced[[1]][[k]],
    newdata = df_predict,
    n.trees = PA_Model_Reduced[[1]][[k]]$gbm.call$best.trees,
    type = "response"
  )
  print(paste("Completed", k, "of 50"))
}

#add predictions to df
df_predict$brt_mean_pred <- rowMeans(Model_Estimates)

#save df
saveRDS(df_predict, "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/predict/roi_mhi_brt_predictions.rds")
roi_MHI_predicted = readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/gams/predict/roi_MHI_predicted.rds")
cor(df_predict$brt_mean_pred, roi_MHI_predicted$fit_prob) #0.9141506 --> 0.9328619




