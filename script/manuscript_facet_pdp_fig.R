rm(list = ls()) 

library(dplyr)
library(ggplot2)

#run all this code for each species and full or mhi until save df step then rm(ls) to run for the next species and scale
#once all rds are saved for each species and scale, run the second half of the script
#load brt model
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/08.12.26/taape/taape_full_reduced_no_island.Rdata")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/08.12.26/taape/taape_mhi_reduced_no_island.Rdata")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/08.12.26/toau/toau_full_reduced_no_island.Rdata")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/08.12.26/toau/toau_mhi_reduced_no_island.Rdata")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/08.12.26/roi/roi_full_reduced_no_island.Rdata")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/08.12.26/roi/roi_mhi_reduced_no_island.Rdata")

#load percent contribution and convert to df
All_percent_contribution = readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/08.12.26/taape/taape_full_reduced_percentcont_no_island.rds")
All_percent_contribution = readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/08.12.26/taape/taape_mhi_reduced_percentcont_no_island.rds")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/08.12.26/toau/toau_full_reduced_percentcont_no_island.Rdata")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/08.12.26/toau/toau_mhi_reduced_percentcont_no_island.Rdata")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/08.12.26/roi/roi_full_reduced_percentcont_no_island.Rdata")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/08.12.26/roi/roi_mhi_reduced_percentcont_no_island.Rdata")

percent_df <- data.frame(
  variable = All_percent_contribution[,1],
  percent = All_percent_contribution[,2]
)

#load dataset
df <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_reduced_final_CEAR.RDS")

# subset species and scale
taape <- df[df$species=="LUKA",]
taape <- df[df$species=="LUKA" & df$region=="mhi",]

toau <- df[df$species=="LUFU",]
toau <- df[df$species=="LUFU" & df$region=="mhi",]

roi <- df[df$species=="CEAR",]
roi <- df[df$species=="CEAR" & df$region=="mhi",]

#extract brt models
PA_Model <- PA_Model_Reduced[[1]]
iters <- length(PA_Model)

# get predictor variables used in the models
var_tested <- PA_Model[[1]]$var.names

# identify continuous predictors (can only plot these)
Cont_Preds <- var_tested[sapply(roi[,var_tested], is.numeric)]

#store the numeric predictor indices
Num_Preds <- which(var_tested %in% Cont_Preds)

# extracts PDP values for every model iteration
Num_Vars <- var_tested[sapply(roi[,var_tested], is.numeric)]

#extract PDP values for every model iteration, loop through every brt model
#generate pdp for each variable
pdp_df <- bind_rows(
  lapply(1:iters, function(i){
    mod <- PA_Model[[i]] #take the ith brt model
    
    bind_rows(
      lapply(Num_Vars, function(v){ #loop through variables
        pp <- gbm::plot.gbm(mod, i.var=v, return.grid=TRUE) #this returns x= predictor value, y=partial effect log scale
        data.frame(x = pp[,1], #convert to dataframe so every row contains predictor value, partial effect, varibale name, model iteration
                   y = pp[,2], 
                   variable=v, 
                   iter=i)
      })
    )
  })
)


#result: variable x.    y.   iter
#.       depth.   5.   0.30.  1
#.       depth.   10.  0.28.  2

#instead of loess smooth and loess SE ribbon, mean across models, SD across models
#ribbon will now reflect variation across ensemble brt models instead of uncertainty in loess smoother
pdp_summary <- pdp_df %>%
  group_by(variable, x) %>%
  summarise(
    mean = mean(y),
    lower = min(y),
    upper = max(y),
  )

#now have smooth pdp curves with uncertainty
#result: variable x.    mean   lower.    upper
#.       depth.   5.   0.30.    0.25.     0.35
#.       depth.   10.  0.28.    0.24.     0.33


#renaming variables FIRST
pdp_summary$variable <- recode(pdp_summary$variable,
                               depth = "Depth (m)",
                               rugosity = "Rugosity",
                               mean_1mo_chla_ESA = "Chlorophyll a",
                               q05_1yr_sst_jpl = "SST (Q05)",
                               q95_1yr_sst_jpl = "SST (Q95)",
                               coral_cover = "Coral Cover (%)",
                               MHI_spear = "Spearfishing Effort",
                               otp_nearshore_sediment = "Nearshore Sediment",
                               otp_all_effluent = "Effluent"
)

#recode percent_df the SAME WAY
percent_df$variable <- recode(percent_df$variable,
                              depth = "Depth (m)",
                              rugosity = "Rugosity",
                              mean_1mo_chla_ESA = "Chlorophyll a",
                              q05_1yr_sst_jpl = "SST (Q05)",
                              q95_1yr_sst_jpl = "SST (Q95)",
                              coral_cover = "Coral Cover (%)",
                              full_spear = "Spearfishing Effort",
                              otp_nearshore_sediment = "Nearshore Sediment",
                              otp_all_effluent = "Effluent"
)

#NOW join
pdp_summary <- left_join(pdp_summary, percent_df, by="variable")

#add speciees to combine later
pdp_summary$species <- "roi"
pdp_summary$region <- "mhi"

#ggplot figure
ggplot(pdp_summary, aes(x, mean)) +
  
  geom_smooth(aes(fill = region, x=x, y=upper)
             , span = 0.1, 
              # ymin = lower, ymax = upper),
              alpha = 0.2) +
  geom_smooth(aes(fill = region, x=x, y=lower)
             , span = 0.1, 
             # ymin = lower, ymax = upper),
             alpha = 0.2) +
              # fill = "#0072B2") +
  
  geom_smooth(aes(color = region),
    method = "loess",
    se = FALSE,
    # color = "#0072B2",
    linewidth = 1,
    span = 0.3
  ) +
  
  # geom_text(
  #   data = label_df,
  #   aes(x = x, y = y, label = paste0(percent, "%")),
  #   inherit.aes = FALSE,
  #   hjust = 0,
  #   vjust = 1,
  #   size = 3
  # ) +
  # 
  facet_wrap(~variable, scales = "free") +
  
  labs(
    x = NULL,
    y = "Partial effect on occurrence (logit scale)"
  ) +
  
  theme_bw(base_size = 13) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  )


#save df
saveRDS(
  pdp_summary,
  "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/8.12.26/roi_mhi_no_island.rds")


### load dataframes and build combined manuscript facet figures
rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

# ---- load & combine ------------------------------------------------------

taape_full <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/8.12.26/taape_full_no_island.rds")
taape_mhi  <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/8.12.26/taape_mhi_no_island.rds")
toau_full  <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/8.12.26/toau_full_no_island.rds")
toau_mhi   <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/8.12.26/toau_mhi_no_island.rds")
roi_full   <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/8.12.26/roi_full_no_island.rds")
roi_mhi    <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/8.12.26/roi_mhi_no_island.rds")

pdp_master <- bind_rows(
  taape_full, taape_mhi,
  toau_full,  toau_mhi,
  roi_full,   roi_mhi
)

unique(pdp_master$species)
unique(pdp_master$region)
unique(pdp_master$variable)   # sanity check exact label spelling before any filtering

pdp_master$species <- factor(tolower(pdp_master$species), levels = c("taape", "toau", "roi"))
pdp_master$region  <- factor(tolower(pdp_master$region),  levels = c("mhi", "full"))

saveRDS(
  pdp_master,
  "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/pdp_master_all_species.rds"
)

# ============================================================
# SST panel
# ============================================================

# fixed: labels are "SST (Q05)" / "SST (Q95)" (this is what script 1's
# recode block actually writes) -- NOT "SST (5th percentile)" etc, which
# was silently matching zero rows before.
pdp_master_sst <- pdp_master %>%
  filter(variable %in% c("SST (Q05)", "SST (Q95)")) %>%
  mutate(
    mean  = plogis(mean),
    lower = plogis(lower),
    upper = plogis(upper)
  )

nrow(pdp_master_sst)  # should be > 0; stop here and check the filter above if not

percent <- pdp_master_sst %>%
  group_by(region, variable, species) %>%
  summarise(percent_cont = unique(percent), .groups = "drop")


percent <- pdp_master_sst %>%
  group_by(region, variable, species) %>%
  summarise(percent_cont = unique(percent), .groups = "drop") %>%
  # stagger mhi above full so the two colored labels don't overlap in
  # the corner -- adjust these two numbers to nudge both up/down together
  mutate(label_vjust = ifelse(region == "mhi", 1.3, 2.8)) %>%
  # EDIT HERE: list which species/variable panels should have their
  # label in the upper RIGHT instead of the default upper left.
  mutate(
    label_corner = case_when(
      variable == "SST (Q95)" ~ "right",
      TRUE ~ "left"
    ),
    x_pos     = ifelse(label_corner == "right", Inf, -Inf),
    hjust_val = ifelse(label_corner == "right", 1.1, -0.1)
  )


sst_plot <- ggplot(pdp_master_sst, aes(x = x, y = mean, color = region, fill = region)) +
  geom_smooth(aes(y = upper),
              method = "loess", se = FALSE,
              span = 0.3, linewidth = 0.4, linetype = "dashed") +
  geom_smooth(aes(y = lower),
              method = "loess", se = FALSE,
              span = 0.3, linewidth = 0.4, linetype = "dashed") +
  geom_smooth(method = "loess", se = TRUE, span = 0.3, linewidth = 1) +
  geom_rug(sides = "b", alpha = 0.2) +
  # facet_grid instead of facet_wrap: variable labels appear once across
  # the top (shared per column), species labels appear once down the
  # right side (shared per row) -- no repeated/combined strip text.
  facet_grid(
    species ~ variable,
    scales = "free",
    labeller = labeller(species = c(taape = "Ta\u02bbape", toau = "To\u02bbau", roi = "Roi"))
  ) +
  labs(
    x = NULL,
    # fixed: this is plogis() / inverse-logit, i.e. a probability scale,
    # not "reverse logit"
    y = "Partial effect on occurrence (Logit scale)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(face = "bold", size = 14),  # variable titles, top
    strip.text.y = element_text(face = "bold", size = 14, angle = 0)  # species labels, right
  ) +
  # corner-anchored instead of data-coordinate positions: works
  # correctly under scales = "free" no matter each panel's data range,
  # and needs no per-panel lookup table to maintain.
  geom_text(
    data = percent,
    aes(x = x_pos, y = Inf, label = percent_cont, color = region,
        vjust = label_vjust, hjust = hjust_val),
    size = 5,
    inherit.aes = FALSE,
    show.legend = FALSE
  )

sst_plot

ggsave(
  "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/figures/SST_q05_q95_no_island.png",
  plot = sst_plot, width = 12, height = 10, dpi = 300
)

# ============================================================
#  ============================================================
# Benthic panel
# ============================================================

pdp_master_benthic <- pdp_master %>%
  filter(variable %in% c("Rugosity", "Depth (m)", "Coral Cover (%)")) %>%
  mutate(
    mean  = plogis(mean),
    lower = plogis(lower),
    upper = plogis(upper)
  )

nrow(pdp_master_benthic)

percent_benthic <- pdp_master_benthic %>%
  group_by(region, variable, species) %>%
  summarise(percent_cont = unique(percent), .groups = "drop") %>%
  mutate(label_vjust = ifelse(region == "mhi", 1.3, 2.8)) %>%
  # EDIT HERE: list which species/variable panels should go upper right.
  mutate(
    label_corner = case_when(
      species == "roi" & variable == "Coral Cover (%)" ~ "right",
      TRUE ~ "left"
    ),
    x_pos     = ifelse(label_corner == "right", Inf, -Inf),
    hjust_val = ifelse(label_corner == "right", 1.1, -0.1)
  )

percent_benthic %>% arrange(species, variable, region) %>% print(n = 20)

benthic_plot <- ggplot(pdp_master_benthic, aes(x = x, y = mean, color = region, fill = region)) +
  geom_smooth(method = "loess", se = TRUE, span = 0.3, linewidth = 1) +
  geom_smooth(aes(y = upper),
              method = "loess", se = FALSE,
              span = 0.3, linewidth = 0.4, linetype = "dashed") +
  geom_smooth(aes(y = lower),
              method = "loess", se = FALSE,
              span = 0.3, linewidth = 0.4, linetype = "dashed") +
  geom_rug(sides = "b") +
  facet_grid(
    species ~ variable,
    scales = "free",
    labeller = labeller(species = c(taape = "Ta\u02bbape", toau = "To\u02bbau", roi = "Roi"))
  ) +
  labs(
    x = NULL,
    y = "Partial effect on occurrence (Logit scale)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(face = "bold", size = 14),  # variable titles, top
    strip.text.y = element_text(face = "bold", size = 14, angle = 0),  # species labels, right
    strip.placement = "outside"
  ) +
  geom_text(
    data = percent_benthic,
    aes(x = x_pos, y = Inf, label = percent_cont, color = region,
        vjust = label_vjust, hjust = hjust_val),
    size = 5,
    inherit.aes = FALSE,
    show.legend = FALSE
  )

benthic_plot

ggsave(
  "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/figures/pdp_benthic_no_island.png",
  plot = benthic_plot, width = 18, height = 12, dpi = 300
)
