rm(list = ls()) 

library(dplyr)
library(ggplot2)

#load brt model
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/07.21/taape/taape_full_reduced_0.001_0.75_07.21.Rdata")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/07.21/taape/taape_mhi_reduced_0.001_0.75_07.21.Rdata")
#load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/07.21/toau/toau_full_reduced_0.001_0.75_07.21.Rdata")
# load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/07.21/toau/toau_mhi_reduced_0.001_0.75_07.21.Rdata")
# load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/07.21/roi/roi_full_reduced_0.001_0.75_07.21.Rdata")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/07.21/roi/roi_mhi_reduced_0.001_0.75_07.21.Rdata")

#load percent contribution and convert to df
All_percent_contribution = readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/07.21/taape/taape_full_reduced_0.001_0.75_precentcont07.7.rds")
All_percent_contribution = readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/output/brts/07.21/taape/taape_mhi_reduced_0.001_0.75_precentcont07.7.rds")
#load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/07.21/toau/toau_full_reduced_percentcont_07.21.Rdata")
# load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/07.21/toau/toau_mhi_reduced_percentcont_07.21.Rdata")
# load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/07.21/roi/roi_full_reduced_percentcont_07.21.Rdata")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/brts/07.21/roi/roi_mhi_reduced_percentcont_07.21.Rdata")

percent_df <- data.frame(
  variable = All_percent_contribution[,1],
  percent = All_percent_contribution[,2]
)

#load dataset
df <- readRDS("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_reduced_final_CEAR.RDS")

# subset species and scale
taape <- df[df$species=="LUKA" & df$region=="mhi",]
# toau <- df[df$species=="LUFU" & df$region=="mhi",]
roi <- df[df$species=="CEAR" & df$region=="mhi",]

#extract brt models
PA_Model <- PA_Model_Reduced[[1]]
iters <- length(PA_Model)

# get predictor variables used in the models
var_tested <- PA_Model[[1]]$var.names

# identify continuous predictors (can only plot these)
Cont_Preds <- var_tested[sapply(taape[,var_tested], is.numeric)]

#store the numeric predictor indices
Num_Preds <- which(var_tested %in% Cont_Preds)

# extracts PDP values for every model iteration
Num_Vars <- var_tested[sapply(taape[,var_tested], is.numeric)]

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


#LOESS smoothing across ensemble predictions
# pdp_summary <- pdp_df %>%
#   group_by(variable) %>%
#   do({
# 
#     df <- .
# 
#     lo <- loess(y ~ x, data=df, span=0.3) #fit loess smoothing
# 
#     pred <- predict(lo, se=TRUE) #predict mean and uncertainty
# 
#     data.frame(
#       x = df$x,
#       mean = pred$fit,
#       lower = pred$fit - 1.96 * pred$se.fit,
#       upper = pred$fit + 1.96 * pred$se.fit
#     )
# 
#   })

#instead of loess smooth and loess SE ribbon, mean across models, SD across models
#ribbon will now reflect variation across ensemble brt models instead of uncertainty in loess smoother
pdp_summary <- pdp_df %>%
  group_by(variable, x) %>%
  summarise(
    mean = mean(y),
    sd = sd(y),
    lower = mean - 1.96 * sd,
    upper = mean + 1.96 * sd,
    .groups = "drop"
  )
#now have smooth pdp curves with uncertainty
#result: variable x.    mean   lower.    upper
#.       depth.   5.   0.30.    0.25.     0.35
#.       depth.   10.  0.28.    0.24.     0.33


#renaming variables FIRST
pdp_summary$variable <- recode(pdp_summary$variable,
                               depth = "Depth (m)",
                               rugosity = "Rugosity",
                               mean_1mo_chla_ESA = "Chla (1 month mean)",
                               q05_1yr_sst_jpl = "SST (5th percentile)",
                               q95_1yr_sst_jpl = "SST (95th percentile)",
                               coral_cover = "Coral Cover (%)",
                               full_spear = "Spearfishing Effort"
)

#recode percent_df the SAME WAY
percent_df$variable <- recode(percent_df$variable,
                              depth = "Depth (m)",
                              rugosity = "Rugosity",
                              mean_1mo_chla_ESA = "Chla (1 month mean)",
                              q05_1yr_sst_jpl = "SST (5th percentile)",
                              q95_1yr_sst_jpl = "SST (95th percentile)",
                              coral_cover = "Coral Cover (%)",
                              full_spear = "Spearfishing Effort"
)

#NOW join
pdp_summary <- left_join(pdp_summary, percent_df, by="variable")

#add speciees to combine later
pdp_summary$species <- "taape"
pdp_summary$region <- "full"

#df for annotation text
label_df <- pdp_summary %>%
  group_by(variable) %>%
  summarise(
    percent = first(percent),
    x = min(x),        # left side of panel
    y = max(upper),    # top of panel
    .groups = "drop"
  )

#ggplot figure
ggplot(pdp_summary, aes(x, mean)) +
  
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2,
              fill = "#0072B2") +
  
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "#0072B2",
    linewidth = 1,
    span = 0.3
  ) +
  
  geom_text(
    data = label_df,
    aes(x = x, y = y, label = paste0(percent, "%")),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    size = 3
  ) +
  
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
  "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/taape_full_pdp_summary.rds")


### load dataframes 
rm(list = ls()) 
library(dplyr)

taape_full <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/taape_full_pdp_summary.rds")
taape_mhi  <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/taape_mhi_pdp_summary.rds")
# taape_mhi <- taape_mhi %>%
#   select(-percent.x, -percent.y)

toau_full  <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/toau_full_pdp_summary.rds")
toau_mhi   <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/toau_mhi_pdp_summary.rds")

roi_full   <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/roi_full_pdp_summary.rds")
roi_mhi    <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/roi_mhi_pdp_summary.rds")

#back convert from logit because it will be from 0-1 (prob of occurence space )

#bind rows
pdp_master <- bind_rows(
  taape_full,
  taape_mhi,
  toau_full,
  toau_mhi,
  roi_full,
  roi_mhi
)

unique(pdp_master$species)
unique(pdp_master$region)

pdp_master$species <- tolower(pdp_master$species)
pdp_master$region  <- tolower(pdp_master$region)

pdp_master$species <- factor(
  pdp_master$species,
  levels = c("taape","toau","roi")
)

pdp_master$region <- factor(
  pdp_master$region,
  levels = c("mhi","full")
)

#smoothing
pdp_master_smooth <- pdp_master %>%
  group_by(species, region, variable) %>%
  do({
    
    df <- .
    
    lo_mean  <- loess(mean  ~ x, data = df, span = 0.3)
    lo_lower <- loess(lower ~ x, data = df, span = 0.3)
    lo_upper <- loess(upper ~ x, data = df, span = 0.3)
    
    data.frame(
      x = df$x,
      mean  = predict(lo_mean,  newdata = data.frame(x = df$x)),
      lower = predict(lo_lower, newdata = data.frame(x = df$x)),
      upper = predict(lo_upper, newdata = data.frame(x = df$x)),
      species = df$species,
      region = df$region,
      variable = df$variable
    )
    
  }) %>%
  ungroup()

# filter for q05 and q95
pdp_master_sst <- pdp_master %>%
  filter(variable %in% c("SST (5th percentile)", "SST (95th percentile)"))

#clean species name
pdp_master_sst$species <- factor(
  pdp_master_sst$species,
  levels = c("taape","toau","roi")
)

#reverse logit transofrmation
pdp_master_sst <- pdp_master_sst %>%
  mutate(
    mean  = plogis(mean),
    lower = plogis(lower),
    upper = plogis(upper)
  )

#SST plots
#keep uppr and lower to turn to the ribbon
ggplot(pdp_master_sst, aes(x = x, y = mean, color = region, fill = region)) +

geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2,
              color = NA) +

 geom_line(size = 1) +
   geom_smooth(method = "loess",
              se = TRUE,
              span = 0.3,
              linewidth = 1) +

  facet_grid(variable ~ species, scales = "free") +

  labs(
    x = NULL,
    y = "Partial effect on occurrence (reverse logit scale)"
  ) +

  theme_bw(base_size = 13) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank()
  )


ggplot(pdp_master_sst, aes(x = x, y = mean, color = region, fill = region)) +

  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2,
    color = NA
  ) +

  geom_smooth(
    method = "loess",
    se = FALSE,
    span = 0.3,
    linewidth = 1
  ) +

  facet_grid(variable ~ species, scales = "free") +

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

#save
saveRDS(
  pdp_master,
  "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/manuscript_facet_figure/pdp_master_all_species.rds"
)


#benthic plots
pdp_master_benthic <- pdp_master %>%
  filter(variable %in% c("Rugosity", "Depth (m)", "Coral Cover (%)"))

pdp_master_benthic$species <- factor(
  pdp_master_benthic$species,
  levels = c("taape","toau","roi")
)

#benthic plots
ggplot(pdp_master_benthic, aes(x = x, y = mean, color = region, fill = region)) +
  
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2,
              color = NA) +
  
  geom_line(size = 1) +
  
  facet_grid(variable ~ species, scales = "free") +
  
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

ggsave(
  "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/figures/pdp_benthic_species.png",
  width = 14,
  height = 6,
  dpi = 300
)

