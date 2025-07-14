# ===============================================
# 1. Load Data and Set Up Spearfishing Scenarios
# ===============================================
rm(list = ls())

# Load species–environment data
df <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full_07.07")

library(dplyr)

# Create columns with +10% and -10% changes in spearfishing effort
df_spear <- df %>%
  mutate(
    `MHI_spear+10` = MHI_spear * 1.10,
    `MHI_spear-10` = MHI_spear * 0.90
  )

rm(df)

# ===============================================
# 2. Load GBM Models for taape or toau
# ===============================================
library(gbm)

#load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/taape_full_reduced_0.001_0.75_07.7.Rdata")
load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/toau_full_reduced_0.001_0.75_07.07.Rdata")

#confirm ensemble length
length(PA_Model_Reduced[[1]])  # should be 50

# ===============================================
# 3. Prepare Datasets for Prediction: Base, +10%, -10%
# ===============================================
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

rm(df_spear)

# ===============================================
# 4. Optional: Skip Prediction for +10% / -10% (Already Done)
# ===============================================
# The below prediction code is commented out since you already generated predictions.
# If needed, uncomment and re-run.

# n <- nrow(df_base)
# base_preds <- matrix(NA, nrow = n, ncol = 50)
# plus_preds <- matrix(NA, nrow = n, ncol = 50)
# minus_preds <- matrix(NA, nrow = n, ncol = 50)
# 
# for (k in 1:50) {
#   model_k <- PA_Model_Reduced[[1]][[k]]
#   base_preds[,k] <- predict.gbm(model_k, df_base,
#                                 n.trees = model_k$gbm.call$best.trees, type = "response")
#   plus_preds[,k] <- predict.gbm(model_k, df_plus,
#                                 n.trees = model_k$gbm.call$best.trees, type = "response")
#   minus_preds[,k] <- predict.gbm(model_k, df_minus,
#                                  n.trees = model_k$gbm.call$best.trees, type = "response")
#   print(paste("Completed", k, "of 50"))
# }


#predicted probability of occurrence (presence) using original MHI_spear values,
#acts as baseline and what the model predicts under current or observed conditions
prob_base <- rowMeans(base_preds)  # we have 50 predicted probablities for each row of the data; we are using rowMeans to average them

#predicted probability of occurrence when MHI spear increased by 10%
prob_plus <- rowMeans(plus_preds)

#predicted probability when spear decreased by 10%
prob_minus <- rowMeans(minus_preds)

#diff_plus: prob+10- base change in predicted probability if effort increases by 10
#diff_minus: prob-10-base change in predicted probability of occurence if effort decreases by 10
#df of results

# toau_results <- df_base %>%
#   dplyr::select(lat, lon, island, region, MHI_spear) %>%
#   mutate(
#     prob_base = prob_base,
#     prob_plus10 = prob_plus,
#     prob_minus10 = prob_minus,
#     diff_plus = prob_plus10 - prob_base,
#     diff_minus = prob_minus10 - prob_base
#   )

#saveRDS(toau_results, file = "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/toau_spear_shift_preds.rds")

# ===============================================
# 5. Load Previously Saved Results (Optional)
# ===============================================
taape_spear_shift_preds <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/taape_spear_shift_preds.rds")
toau_spear_shift_preds  <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/toau_spear_shift_preds.rds")

# ===============================================
# 6. Run Spearfishing Effort Shift Analysis on Oʻahu
# ===============================================
# Filter to just Oʻahu
df_base_oahu <- subset(df_base, island == "Oahu")

# Initialize results dataframe
df_result <- df_base_oahu %>%
  dplyr::select(lat, lon, island)

# Predict baseline for Oʻahu
base_preds_oahu <- matrix(NA, nrow = nrow(df_base_oahu), ncol = 50)
for (k in 1:50) {
  model_k <- PA_Model_Reduced[[1]][[k]]
  base_preds_oahu[,k] <- predict.gbm(model_k, df_base_oahu,
                                     n.trees = model_k$gbm.call$best.trees, type = "response")
  print(paste("Completed base model", k, "of 50"))
}

#next, loop over 21 scenarios: -100 to 100% spearfishing
percent_multiplier <- seq(0, 2, 0.1)        # 0x to 2x spearfishing
col_names <- seq(-100, 100, 10)             # label columns as -100% to +100%

for (i in seq_along(percent_multiplier)) {
  pct <- percent_multiplier[i]
  col_name <- as.character(col_names[i])
  
  df_temp <- df_base_oahu %>%
    mutate(MHI_spear = MHI_spear * pct)
  
  temp_preds <- matrix(NA, nrow = nrow(df_temp), ncol = 50)
  
  for (k in 1:50) {
    model_k <- PA_Model_Reduced[[1]][[k]]
    temp_preds[, k] <- predict.gbm(model_k, df_temp,
                                   n.trees = model_k$gbm.call$best.trees,
                                   type = "response")
  }
  
  
  # Compute mean change from baseline
  mean_base <- rowMeans(base_preds_oahu)
  mean_temp <- rowMeans(temp_preds)
  delta <- mean_temp - mean_base
  
  df_result[[as.character(col_name)]] <- delta
  print(paste("Completed percent change:", pct))
}

(colMeans(df_result[,4:24])) #calculate the mean for each column
df_result_toau = df_result

rm(temp_preds, mean_base, mean_temp, delta, df_temp)  # clean up

# ===============================================
# 8. Finalize and Save Results
# ===============================================
df_result_toau <- df_result_toau %>%
  mutate(species = "LUFU")

save(df_result_toau, file ="/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/10spear/df_result_toau_mean.Rdata")
# repeat for other species
# make a dataframe that combines this stuff (colMeans(df_result[,4:24])) together 
# and make sure you know which species is what and which means belong to which 100,90,80 etc

## make new plot ##


library(ggplot2)
ggplot(oahu_summary, aes(x = percent_change, y = mean_diff)) +
  geom_point(size = 3, color = "blue") +
  geom_line(color = "blue", linewidth = 1) +
  geom_errorbar(aes(ymin = mean_diff - sd_diff, ymax = mean_diff + sd_diff),
                width = 2, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Oʻahu: Change in Predicted Occurrence vs. Spearfishing Effort",
       x = "Percent Change in Spearfishing Effort",
       y = "Mean Change in Probability of Occurrence (Δ)") +
  theme_minimal(base_size = 14)

### plot -+10 increase
rm(list= ls()) 
taape_spear_shift_preds <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/taape_spear_shift_preds.rds")
toau_spear_shift_preds <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/toau_spear_shift_preds.rds")

taape_spear_shift_preds$species <- "taape"
toau_spear_shift_preds$species <- "toau"
combined <- bind_rows(taape_spear_shift_preds, toau_spear_shift_preds)

# +10% summary
plus_summary <- combined %>%
  dplyr::group_by(island, species) %>%
  dplyr::summarize(
    mean_diff = mean(diff_plus, na.rm = TRUE),
    sd_diff = sd(diff_plus, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(scenario = "+10% Spearfishing")

minus_summary <- combined %>%
  dplyr::group_by(island, species) %>%
  dplyr::summarize(
    mean_diff = mean(diff_minus, na.rm = TRUE),
    sd_diff = sd(diff_minus, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(scenario = "–10% Spearfishing")

summary_all <- bind_rows(plus_summary, minus_summary)
colnames(summary_all)

summary_all$island <- factor(summary_all$island, levels = c(
  "Niihau", "Kauai", "Oahu", "Molokai", "Lanai", "Kahoolawe", "Maui", "Hawaii"
))

summary_all %>%  filter(island %in% c("Hawaii", "Kahoolawe", "Kauai", "Lanai", "Maui", 
                                      "Molokai", "Niihau", "Oahu")) %>% 
  ggplot( aes(x = island, y = mean_diff, color = species)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = mean_diff - sd_diff, ymax = mean_diff + sd_diff),
                width = 0.2, position = position_dodge(width = 0.5)) +
  facet_wrap(~scenario) +
  labs(title = "Change in Predicted Occurrence by Island and Species",
       x = "Island", y = "Mean Change in Probability (Δ)") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("taape" = "blue", "toau" = "red"))
ggsave("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/plots/mhi_10spear.png", width = 12, height = 5, units = "in", bg = "white")

#toau increase in probability
ggplot(toau_results, aes(x = lon, y = lat, color = diff_plus)) +
  geom_point(size = 2) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                        name = "Δ Probability") +
  labs(title = "Change in Predicted Occurrence (+10% Spearfishing)",
       x = "Longitude", y = "Latitude") +
  coord_cartesian(xlim = c(195, 207), ylim = c(19, 24)) +
  theme_minimal()

#toau decrease in probability
ggplot(toau_results, aes(x = lon, y = lat, color = diff_minus)) +
  geom_point(size = 2) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                        name = "Δ Probability") +
  labs(title = "Change in Predicted Occurrence (–10% Spearfishing)",
       x = "Longitude", y = "Latitude") +
  coord_cartesian(xlim = c(198, 206), ylim = c(19, 23)) +
  theme_classic() +
  theme(
    plot.background = element_rect(fill = "gray80", color = NA),
    panel.background = element_rect(fill = "gray80", color = NA)
  )

#taape decrease in spear
ggplot(taape_spear_shift_preds, aes(x = lon, y = lat, color = diff_minus)) +
  geom_point(size = 2) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                        name = "Δ Probability") +
  labs(title = "Change in Predicted Occurrence (–10% Spearfishing)",
       x = "Longitude", y = "Latitude") +
  coord_cartesian(xlim = c(195, 207), ylim = c(19, 24)) +
  theme_minimal()

#boxplot by island
ggplot(toau_results, aes(x = island, y = diff_plus)) +
  geom_boxplot(fill = "red", alpha = 0.6) +
  labs(title = "Predicted Change (+10%) by Island", y = "Δ Probability") +
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
  
  library(reshape2)
  library(ggplot2)
  
  # Melt the data to long format
  df_long <- melt(df_result_toau, 
                  id.vars = c("lat", "lon", "island", "species"),
                  variable.name = "percent_change", 
                  value.name = "delta_prob")
  
  # Convert percent_change to numeric (it's character from column names like "-100")
  df_long$percent_change <- as.numeric(as.character(df_long$percent_change))
  
  # Optional: summarize by mean change per percent effort
  df_summary <- df_long %>%
    group_by(percent_change) %>%
    summarise(mean_delta = mean(delta_prob, na.rm = TRUE),
              sd_delta = sd(delta_prob, na.rm = TRUE))
  
  # Plot: Mean change in probability vs. % change in spearfishing effort
  ggplot(df_summary, aes(x = percent_change, y = mean_delta)) +
    geom_line(color = "steelblue") +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = mean_delta - sd_delta, ymax = mean_delta + sd_delta), 
                alpha = 0.2, fill = "steelblue") +
    labs(
      title = "Effect of Spearfishing Effort on Toʻau Presence Probability",
      x = "% Change in Spearfishing Effort",
      y = "Mean Δ Predicted Probability of Presence"
    ) +
    theme_minimal()
}