# ===============================================
# 1. Load Data and Set Up Spearfishing Scenarios
# ===============================================
rm(list = ls())
library(dplyr)

# Load species–environment data
df <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_reduced_final_CEAR.RDS")

# ===============================================
# 2. Prepare Datasets for Prediction: Base, +10%, -10%
# ===============================================
# Create columns with +10% and -10% changes in spearfishing effort
df_spear <- df %>%
  mutate(
    `MHI_spear+10` = MHI_spear * 1.10,
    `MHI_spear-10` = MHI_spear * 0.90
  )

df_base <- df_spear %>%
  ungroup() %>%
  dplyr::select(-`MHI_spear+10`, -`MHI_spear-10`)

#+10 spearfishing scenario, drops original MHI_spear and replaced with +10
df_plus <- df_spear %>%
  ungroup() %>%
  dplyr::select(-MHI_spear) %>%
  dplyr::rename(MHI_spear = `MHI_spear+10`) %>%
  dplyr::select(names(df_base))

#drops original MHI_spear, replaces with -10
df_minus <- df_spear %>%
  ungroup() %>%
  dplyr::select(-MHI_spear) %>%
  dplyr::rename(MHI_spear = `MHI_spear-10`) %>%
  dplyr::select(names(df_base))

rm(df_spear)
# ===============================================
# 3. Load GBM Models for taape or toau
# ===============================================
library(gbm)
load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/07.21/taape/taape_mhi_reduced_0.001_0.75_07.21.Rdata")
load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/07.21/toau/toau_mhi_reduced_0.001_0.75_07.21.Rdata")
load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/07.21/roi/roi_mhi_reduced_0.001_0.75_07.21.Rdata")


#confirm ensemble length
length(PA_Model_Reduced[[1]])  # should be 50
# ===============================================
# 4. Optional: Skip Prediction for +10% / -10% (Already Done)
# ===============================================
# The below prediction code is commented out since you already generated predictions.
# If needed, uncomment and re-run.

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
prob_base <- rowMeans(base_preds)  # we have 50 predicted probabilities for each row of the data; we are using rowMeans to average them

#predicted probability of occurrence when MHI spear increased by 10%
prob_plus <- rowMeans(plus_preds)

#predicted probability when spear decreased by 10%
prob_minus <- rowMeans(minus_preds)

#diff_plus: prob+10- base change in predicted probability if effort increases by 10
#diff_minus: prob-10-base change in predicted probability of occurence if effort decreases by 10 --> df of results

roi_spear_predictions <- df_base %>%
  dplyr::select(lat, lon, island, region, MHI_spear) %>%
  mutate(
    prob_base = prob_base,
    prob_plus10 = prob_plus,
    prob_minus10 = prob_minus,
    diff_plus = prob_plus10 - prob_base,
    diff_minus = prob_minus10 - prob_base
  )

saveRDS(roi_spear_predictions, file = "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/roi_spear_preds.rds")

# ===============================================
# 5. Load Previously Saved Results (Optional)
# ===============================================
# taape_spear_preds <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/taape_spear_preds.rds")
# toau_spear_preds  <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/toau_spear_preds.rds")
# roi_spear_preds <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/roi_spear_preds.rds")

#! need to run this whole thing gthrough for each species
# ===============================================
# 6. Run Spearfishing Effort Shift Analysis on Oʻahu
# ===============================================
# Filter to just Oʻahu
df_base_oahu <- subset(df_base, island == "Oahu") #df_base

# Initialize results dataframe df_base_oahu
df_result <- df_base_oahu %>% 
  dplyr::select(lat, lon, island)

# Predict baseline for Oʻahu
base_preds_oahu <- matrix(NA, nrow = nrow(df_base_oahu), ncol = 50) #df_base_oahu
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
df_result_roi= df_result

rm(temp_preds, mean_base, mean_temp, delta, df_temp)  # clean up

# ===============================================
# 8. Finalize and Save Results
# ===============================================
df_result_roi <- df_result_roi %>%
  mutate(species = "CEAR")

save(df_result_roi, file ="/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/10spear/oahu_result_roi.Rdata")
# repeat for other species
# make a dataframe that combines this stuff (colMeans(df_result[,4:24])) together 
# and make sure you know which species is what and which means belong to which 100,90,80 etc



## make new plot ##
load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/oahu_result_taape.RData")
load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/oahu_result_toau.RData")
load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/oahu_result_roi.RData")

#plot
library(reshape2)
library(dplyr)
library(ggplot2)

# Melt both datasets
long_toau <- melt(df_result_toau,
                  id.vars = c("lat", "lon", "island", "species"),
                  variable.name = "percent_change",
                  value.name = "delta_prob")

long_taape <- melt(df_result_taape,
                   id.vars = c("lat", "lon", "island", "species"),
                   variable.name = "percent_change",
                   value.name = "delta_prob")

long_roi <- melt(df_result_roi,
                 id.vars = c("lat", "lon", "island", "species"),
                 variable.name = "percent_change",
                 value.name = "delta_prob")

# Combine
df_long <- bind_rows(long_toau, long_taape, long_roi)

# Convert percent_change to numeric
df_long$percent_change <- as.factor(df_long$percent_change)
df_long$percent_change <- as.numeric(as.character(df_long$percent_change))

df_summary <- df_long %>%
  group_by(species, percent_change) %>%
  reframe(mean_delta = mean(delta_prob, na.rm = TRUE),
            sd_delta = sd(delta_prob, na.rm = TRUE))

ggplot(df_summary, aes(x = percent_change, y = mean_delta, color = species, fill = species)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = mean_delta - sd_delta,
                  ymax = mean_delta + sd_delta),
              alpha = 0.2, color = NA) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of Spearfishing Effort on Predicted Observability on Oʻahu",
    x = "% Change in Spearfishing Effort",
    y = "Δ Probability of Presence",
    color = "Species",
    fill = "Species"
  ) +
  scale_x_continuous(breaks = seq(-100, 100, 10), limits = c(-100, 100)) +
  theme_minimal(base_size = 14)

ggsave("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/plots/predicted_spear_loop.png", width = 12, height = 5, units = "in", bg = "white")

#####################
### plot -+10 increase
rm(list= ls()) 
taape_spear_preds <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/taape_spear_preds.rds")
toau_spear_preds <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/toau_spear_preds.rds")
roi_spear_preds <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/10spear/roi_spear_preds.rds")

taape_spear_preds$species <- "taape"
toau_spear_preds$species <- "toau"
roi_spear_preds$species <- "roi"
combined <- bind_rows(taape_spear_preds, toau_spear_preds, roi_spear_preds)

ggplot(df_summary, aes(x = percent_change, 
                       y = mean_delta, 
                       color = species, 
                       fill = species)) +
  # confidence interval shading
  geom_ribbon(aes(ymin = mean_delta - sd_delta,
                  ymax = mean_delta + sd_delta,
                  fill = species),
              alpha = 0.2, color = NA) +
  # dotted mean lines
  geom_line(linewidth = 1, linetype = "dotted") +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Effect of Spearfishing Effort on Predicted Probability of Presence",
    x = "% Change in Spearfishing Effort",
    y = "Δ Probability of Presence",
    color = "Species",
    fill = "Species"
  ) +
  coord_cartesian(xlim = c(0, 20)) +  # force -100 to 100 on x-axis
  theme_minimal()
