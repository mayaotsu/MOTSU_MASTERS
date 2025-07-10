rm(list= ls()) 
df<-readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full_07.07") 

library(dplyr)
df_spear <- df %>%
  mutate(
    `MHI_spear+10` = MHI_spear * 1.10,
    `MHI_spear-10` = MHI_spear * 0.90
  )

rm(df)
library(gbm)
#load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/taape_full_reduced_0.001_0.75_07.7.Rdata")
load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/toau_full_reduced_0.001_0.75_07.07.Rdata")

length(PA_Model_Reduced[[1]])  # should be 50

df_base <- df_spear %>%
  ungroup() %>%
  dplyr::select(-`MHI_spear+10`, -`MHI_spear-10`)

df_plus <- df_spear %>%
  ungroup() %>%
  dplyr::select(-MHI_spear) %>%
  dplyr::rename(MHI_spear = `MHI_spear+10`) %>%
  dplyr::select(names(df_base))

df_minus <- df_spear %>%
  ungroup() %>%
  dplyr::select(-MHI_spear) %>%
  dplyr::rename(MHI_spear = `MHI_spear-10`) %>%
  dplyr::select(names(df_base))

# Create empty matrices to store predictions
n <- nrow(df_base)
base_preds <- matrix(NA, nrow = n, ncol = 50)
plus_preds <- matrix(NA, nrow = n, ncol = 50)
minus_preds <- matrix(NA, nrow = n, ncol = 50)

for (k in 1:50) {
  model_k <- PA_Model_Reduced[[1]][[k]]
  base_preds[,k] <- predict.gbm(model_k, df_base,
                                n.trees = model_k$gbm.call$best.trees, type = "response")
  plus_preds[,k] <- predict.gbm(model_k, df_plus,
                                n.trees = model_k$gbm.call$best.trees, type = "response")
  minus_preds[,k] <- predict.gbm(model_k, df_minus,
                                 n.trees = model_k$gbm.call$best.trees, type = "response")
  print(paste("Completed", k, "of 50"))
}
#predicted probability of occurrence (presence) using original MHI_spear values,
#acts as baseline and what the model predicts under current or observed conditions
prob_base <- rowMeans(base_preds)  

#predicted probability of occurrence when MHI spear increased by 10%
prob_plus <- rowMeans(plus_preds)

#predicted probability when spear decreased by 10%
prob_minus <- rowMeans(minus_preds)

#diff_plus: prob+10-base change in predicted probability if effort increases by 10
#diff_minus: prob-10-base change in predicted probability of occurence if effort decreases by 10
#df of results

toau_results <- df_base %>%
  dplyr::select(lat, lon, island, region, MHI_spear) %>%
  mutate(
    prob_base = prob_base,
    prob_plus10 = prob_plus,
    prob_minus10 = prob_minus,
    diff_plus = prob_plus10 - prob_base,
    diff_minus = prob_minus10 - prob_base
  )

saveRDS(toau_results, 
        file = "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/toau_spear_shift_preds.rds")


#increase in probability
ggplot(toau_results, aes(x = lon, y = lat, color = diff_plus)) +
  geom_point(size = 2) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                        name = "Δ Probability") +
  labs(title = "Change in Predicted Occurrence (+10% Spearfishing)",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

#decrease in probability
ggplot(toau_results, aes(x = lon, y = lat, color = diff_minus)) +
  geom_point(size = 2) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                        name = "Δ Probability") +
  labs(title = "Change in Predicted Occurrence (–10% Spearfishing)",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

#boxplot by island
ggplot(toau_results, aes(x = island, y = diff_plus)) +
  geom_boxplot(fill = "red", alpha = 0.6) +
  labs(title = "Predicted Change (+10%) by Island", y = "Δ Probability") +
  theme_minimal()

ggplot(toau_results, aes(x = island, y = diff_minus)) +
  geom_boxplot(fill = "blue", alpha = 0.6) +
  labs(title = "Predicted Change (–10%) by Island", y = "Δ Probability") +
  theme_minimal()







#model estimates creating number of rows same as df, and 50 ensembel
# 4 loop takes each individual model and predicts over that dataset

for (i in 1:length(Relevant_Models)){
  Model_Estimates<-matrix(, nrow=nrow(Env_Vars_SSLL_Quarter_Deg), ncol=50)
  SSLL_PA_Models<-readRDS(paste0("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/taape_full_reduced_0.001_0.75_07.7.Rdata",Relevant_Models[i])) #read in full reduced model for each species 
  
  for (k in 1:50){
    Model_Estimates[,k]<-predict.gbm(SSLL_PA_Models[[1]][[k]], Env_Vars_SSLL_Quarter_Deg,
                                     n.trees=SSLL_PA_Models[[1]][[k]]$gbm.call$best.trees, type="response")
    print(paste("completed", k, "of 50"))}
  saveRDS(Model_Estimates, 
 # can stop here
 
  paste0("~/Documents/Eco_Cast_Plus_2025/Model_Predictions/PA/",Relevant_Species[i],"_SSLL_PA_Model_Est.rds"))
  print(paste("completed", Relevant_Species[i],i, "of", length(Relevant_Models)))
  rm(Model_Estimates)
  
  #how different predicted probability of occurence is if using individual data compared to the plus or minus spearfishing
}