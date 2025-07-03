library(raster)
library(dplyr)
library(ggplot2)

#function
fill_missing_column <- function(main_df, patch_df, join_cols, var_to_fill) {
  patched <- main_df %>%
    left_join(
      patch_df %>% select(all_of(c(join_cols, var_to_fill))),
      by = join_cols,
      suffix = c("", ".new")
    ) %>%
    mutate(
      {{ var_to_fill }} := coalesce(.data[[var_to_fill]], .data[[paste0(var_to_fill, ".new")]])
    ) %>%
    select(-all_of(paste0(var_to_fill, ".new")))
  
  return(patched)
}

boat_raster <- raster("/Users/mayaotsu/Downloads/MHI_Boat_Spear_hr.tif")
shore_raster <- raster("/Users/mayaotsu/Downloads/MHI_Shore_Spear_hr.tif")
spear_raster2 <- resample(shore_raster, boat_raster, method = "bilinear")

combined_spear <- boat_raster + spear_raster2
rm(boat_raster, shore_raster, spear_raster2)

#create df with NA points from original spc_reduced
na_points_spear <- spc_reduced %>% filter(is.na(MHI_spear))
#na_points$lon <- ifelse(na_points$lon < 0, na_points$lon + 360, na_points$lon)
na_points_spear$lon <- ifelse(na_points_spear$lon > 0, na_points_spear$lon - 360, na_points_spear$lon)

#convert na points to spatial points
na_spatial_spear <- na_points_spear[, c("lon", "lat")]
#4points <- SpatialPoints(na_spatial, proj4string = CRS(proj4string(combined_spear)))
points_spear <- sp::SpatialPoints(na_spatial_spear, proj4string = raster::crs(combined_spear))

rm(na_spatial_spear)

#extract raster values
extracted_vals_spear <- raster::extract(combined_spear, points_spear)
#reattach extracted values to NA points
na_points_spear$MHI_spear <- extracted_vals_spear

##check
sum(!is.na(na_points_spear$MHI_spear))  # should be > 0, 1514
sum(is.na(na_points_spear$MHI_spear))   # should be < original 1742, 220

#unique pts to avoid duplicates
na_points_unique_spear <- na_points_spear %>%
  filter(!is.na(MHI_spear)) %>%
  distinct(lat, lon, year, month, day, species, .keep_all = TRUE)

na_points_unique_spear$lon <- ifelse(na_points_unique_spear$lon < 0, na_points_unique_spear$lon + 360, na_points_unique_spear$lon)

#fix in spc reduced
# spc_filled <- spc_reduced %>%
#   rows_update(na_points_unique_spear, by = c("lat", "lon", "year", "month", "day", "species"), unmatched = "ignore")

spc_filled <- fill_missing_column(
  main_df = spc_reduced,
  patch_df = na_points_unique_spear,
  join_cols = c("lat", "lon", "year", "month", "day", "species"),
  var_to_fill = "MHI_spear"
)

###############
### CHECKS ####
###############

#confirm only those rows were changed
unchanged <- all.equal(spc_reduced %>% select(-MHI_spear),
                       spc_filled %>% select(-MHI_spear))

# see if any other rows changed other than spear
sum(spc_reduced$MHI_spear[!is.na(spc_reduced$MHI_spear)] != 
      spc_filled$MHI_spear[!is.na(spc_reduced$MHI_spear)], na.rm = TRUE) #should be 0

all.equal(spc_filled, spc_reduced)
setequal(spc_reduced$MHI_spear, spc_filled$MHI_spear)

#check
sum(is.na(spc_reduced$MHI_spear) & !is.na(spc_filled$MHI_spear)) #1514
sum(is.na(spc_reduced$MHI_spear)) #should be 1742
sum(is.na(spc_filled$MHI_spear)) #should be 220

#create df to visually see where spear values were filled
filled_rows <- spc_reduced %>%
  mutate(original_na = is.na(MHI_spear)) %>%
  bind_cols(filled_val = spc_filled$MHI_spear) %>%
  filter(original_na & !is.na(filled_val))

library(ggplot2)

# Identify filled rows
filled_MHI <- which(is.na(spc_reduced$MHI_spear) & !is.na(spc_filled$MHI_spear))
filled_rows_MHI <- spc_filled[filled_MHI, ]

# Plot
ggplot(spc_reduced, aes(x = lon, y = lat)) +
  geom_point(color = "gray70", size = 1) +
  geom_point(data = filled_rows_MHI, aes(x = lon, y = lat), color = "blue", size = 2) +
  labs(title = "Filled Values in MHI_spear",
       subtitle = "Gray = all data, Blue = filled values",
       x = "Longitude", y = "Latitude") +
  coord_fixed() +
  theme_minimal()

sum(is.na(spc_reduced$MHI_spear)) #should be 1734
sum(is.na(spc_filled$MHI_spear)) #should be 220

rm(combined_spear, na_points_spear, na_points_unique_spear, points_spear, filled_rows_MHI)
rm(filled_rows, filled_rows_MHI, changed_rows, filled_count, filled_MHI, unchanged)
#saveRDS(spc_filled, "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_filled_spear_updated.rds")

#################
### EFFLUENT ###
#################
effluent_raster <- raster("/Users/mayaotsu/Downloads/hi_otp_all_osds_effluent.tif")
na_points_eff <- spc_reduced %>% filter(is.na(otp_all_effluent))
na_points_eff$lon <- ifelse(na_points_eff$lon > 0, na_points_eff$lon - 360, na_points_eff$lon)

#spc_reduced$lon <- ifelse(spc_reduced$lon > 180, spc_reduced$lon - 360, spc_reduced$lon)

na_spatial_eff <- na_points_eff[, c("lon", "lat")]
points_eff <- sp::SpatialPoints(na_spatial_eff, proj4string = raster::crs(effluent_raster))

rm(na_spatial_eff)

extracted_vals_eff <- raster::extract(effluent_raster, points_eff)
na_points_eff$otp_all_effluent <- extracted_vals_eff

##check
sum(!is.na(na_points_eff$otp_all_effluent))  # should be > 0 (1700)
sum(is.na(na_points_eff$otp_all_effluent))   # should be < original 1742 (18)

#unique pts to avoid duplicates
na_points_unique_eff <- na_points_eff %>%
  filter(!is.na(otp_all_effluent)) %>%
  distinct(lat, lon, year, month, day, species, .keep_all = TRUE)

na_points_unique_eff$lon <- ifelse(na_points_unique_eff$lon < 0, na_points_unique_eff$lon + 360, na_points_unique_eff$lon)

#fix in spc reduced
# spc_filled <- spc_filled %>%
#   rows_update(na_points_unique_eff, by = c("lat", "lon", "year", "month", "day", "species"), unmatched = "ignore")

spc_filled <- fill_missing_column(
  main_df = spc_filled,
  patch_df = na_points_unique_eff,
  join_cols = c("lat", "lon", "year", "month", "day", "species"),
  var_to_fill = "otp_all_effluent"
)
# How many NA values were filled (1700)
sum(is.na(spc_reduced$otp_all_effluent)) - sum(is.na(spc_filled$otp_all_effluent))

# Did we overwrite any existing values? Should be 0
sum(spc_reduced$otp_all_effluent[!is.na(spc_reduced$otp_all_effluent)] != 
      spc_filled$otp_all_effluent[!is.na(spc_reduced$otp_all_effluent)], na.rm = TRUE)

#confirm only those rows were changed
unchanged <- all.equal(spc_reduced %>% select(-otp_all_effluent),
                       spc_filled %>% select(-otp_all_effluent))

all.equal(spc_filled, spc_reduced)
setequal(spc_reduced$otp_all_effluent, spc_filled$otp_all_effluent)

rm(effluent_raster, na_points_eff, na_points_unique_eff, points_eff)
rm(unchanged)

sum(is.na(spc_reduced$otp_all_effluent)) #should be 1718
sum(is.na(spc_filled$otp_all_effluent)) #should be 18

filled_spear <- spc_reduced %>%
  mutate(original_na = is.na(MHI_spear),
         new_val = spc_filled$MHI_spear) %>%
  filter(original_na & !is.na(new_val)) %>%
  select(lat, lon, date_, species, MHI_spear_old = MHI_spear, MHI_spear_new = new_val)

filled_effluent <- spc_reduced %>%
  mutate(original_na = is.na(otp_all_effluent),
         new_val = spc_filled$otp_all_effluent) %>%
  filter(original_na & !is.na(new_val)) %>%
  select(lat, lon, date_, species, effluent_old = otp_all_effluent, effluent_new = new_val)

filled_both <- spc_reduced %>%
  mutate(mhi_old = MHI_spear,
         effluent_old = otp_all_effluent,
         mhi_new = spc_filled$MHI_spear,
         effluent_new = spc_filled$otp_all_effluent) %>%
  filter((is.na(mhi_old) & !is.na(mhi_new)) | 
           (is.na(effluent_old) & !is.na(effluent_new))) %>%
  select(lat, lon, date_, species, mhi_old, mhi_new, effluent_old, effluent_new)

saveRDS(spc_filled, "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_filled_spear_effluent.rds")
