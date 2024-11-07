#####################################################
# Benthic habitat data:                             #
# CCA-coral cover, macroalgae cover, sediment cover #
#####################################################

library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(mgcv)
library(raster)

# Clear the environment
rm(list = ls())

# Alias for dplyr::select to avoid conflicts
select = dplyr::select

# Load the prediction grid prepped using EDS
#defining different depth bins based on bathymetry data, adding columns for island(unit), year(string) and depth(bathymetry)
#keeps columns lon, lat, island, depth bin
grid <- readRDS("/Users/mayaotsu/Documents/MOTSU_MASTERS/coral_cover/gridded_bathymetry_date_fields.rds") %>%
  mutate(island = unit, 
         year = as.character(year),
         depth = bathymetry,
         depth_bin = case_when(
           depth <= 0 & depth >= -6 ~ "Shallow",
           depth < -6 & depth >= -18 ~ "Mid",
           depth < -18 ~ "Deep",
           TRUE ~ ""
         )) %>%
  select(lon, lat, island, depth_bin) %>%
  distinct()

# Load and preprocess benthic coral cover data
#keep only regions that are MHI/NWHI, converting percentages into proportion, data cleaning columns
df <- read.csv("/Users/mayaotsu/Documents/MOTSU_MASTERS/coral_cover/BenthicCover_2010-2023_Tier1_SITE.csv") %>%
  rename_with(tolower) %>%
  filter(region %in% c("MHI", "NWHI"), exclude_flag == 0) %>%
  mutate(coral_cover = cca_coral / 100, #make into percent
         macroalgae_cover = ma / 100,
         sediment_cover = sed / 100,
         date_ = mdy(date_),  #convery to mdy format
         year = as.character(year(date_)), #extract year, convert to character string (WHY???)   
         month = month(date_),  #extract month
         day = day(date_), #extract data
         lon = longitude, 
         lat = latitude) %>% 
  select(lon, lat, year, region, island, reef_zone, depth_bin, coral_cover, macroalgae_cover, sediment_cover) %>% #keep these columns
  filter(depth_bin %in% c("Deep", "Mid", "Shallow"))

# Replace spaces with underscores in island names
df$island <- gsub(" ", "_", df$island)

# Filter grid to match islands in df
#include only rows where island value matches island name in df, to only include islands relevant to cca in df
grid <- grid %>% filter(island %in% unique(df$island))

# Plot histogram of coral cover by region
df %>% 
  ggplot(aes(x = coral_cover)) +
  geom_histogram(aes(y = ..density.., fill = ..density..), show.legend = FALSE) +
  scale_fill_viridis_c() +
  scale_x_continuous(limits = c(0, 1)) +
  facet_wrap(~region, ncol = 1)

# Initialize empty objects to store results
gam_site <- NULL
gam_grid <- NULL

# Loop through each island to perform GAM and predictions
islands <- unique(df$island) #make list of island with cca data for analysis per island
for (i in seq_along(islands)) { #loop iterates over each island to length of island vector
  
  # Subset data for the current island, isolate data pertaining to current island for analysis,
  #models trained and predictions made only for relevant spatial context
  df_i <- df %>% filter #contains observation data (CCA measurements) for current island
  grid_i <- grid %>% filter(island == islands[i]) #gridded spatial data points (where predictions will be made for current island)

#generalized can use different forms of link functions (poisson, log, etc), and additive to add predictors to find combined effect  
  # Fit GAM model for coral cover, to model relationship between coral cover and location and depth 
  #output is quantified relationship between cca and lat, lon and depth for each island
  #(how cca varies spatially and with depth for each island)
  #response: cca, predictor: lon,lat, depth
  coral_cover_gam <- gam(macroalgae_cover ~ s(lon, lat) + depth_bin, 
                         data = df_i,
                         family = binomial(link = "cloglog")) #data between 0 and 1, cloglog suitable when CCA (respvar) may be skewed toward 0 or 1
  
  # Predict coral cover on the observed and gridded data
  #generate predicted cca values using fitted model for observed and spatial grid points
  df_i$coral_cover_pred <- predict(coral_cover_gam, df_i, type = "response") #predictions on observed data, provided fitted values to compare to actual observations
  grid_i$coral_cover_pred <- predict(coral_cover_gam, grid_i, type = "response") #predictions on gridded data, extends predictions to unobserved locations, creating predictive cca maps
  #additional column called coral_cover_pred for predicted cca values
  
  # Filter grid points to match the extent of the observed data, avoid extrapolations beyond areas with no observations
  grid_i <- grid_i %>% 
    filter(lon >= min(df_i$lon), lon <= max(df_i$lon), #keeps points between max amd min lon
           lat >= min(df_i$lat), lat <= max(df_i$lat)) #keeps points between max and min lat
  #grid i contains grid points within spatial bounds of observed data
  
  # Rescale predicted values to match observed value range
  obs_range <- range(df_i$coral_cover) #calculate min and max of observed coral cover
  pred_range <- range(df_i$coral_cover_pred) #compute the range of predicted cca vals
  
  df_i$coral_cover_pred <- ((df_i$coral_cover_pred - pred_range[1]) / diff(pred_range)) * diff(obs_range) + obs_range[1]
  pred_range <- range(grid_i$coral_cover_pred)
  grid_i$coral_cover_pred <- ((grid_i$coral_cover_pred - pred_range[1]) / diff(pred_range)) * diff(obs_range) + obs_range[1]
  #predicted cals (coral_cover_pred) for observed df_i and gridded data grid_i rescaled to match observed vals range
  
  # Combine results, aggregate predicted results from each island into single df
  gam_site <- rbind(gam_site, df_i) #appends results for current island df_i to existing gam_site df, will have all observed and predicted vals across islands
  gam_grid <- rbind(gam_grid, grid_i) #appends gridded predictiosn for current island df_i to gam grid df, will contain corresponding gridded predictions
}

# Visualize observed vs. predicted coral cover for a specific island
i <- 5

p1 <- gam_site %>% 
  filter(island == islands[i]) %>%
  ggplot(aes(lon, lat, fill = coral_cover)) + 
  geom_point(shape = 21, size = 5, alpha = 0.8, show.legend = F) + 
  scale_fill_viridis_c("") + 
  ggtitle("Observation") + 
  coord_fixed()

p2 <- gam_site %>% 
  filter(island == islands[i]) %>%
  ggplot(aes(lon, lat, fill = coral_cover_pred)) + 
  geom_point(shape = 21, size = 5, alpha = 0.8) + 
  scale_fill_viridis_c("")  + 
  ggtitle("Prediction") + 
  coord_fixed()

p1 + p2

p3 <- gam_grid %>% 
  filter(island == islands[i]) %>%
  ggplot(aes(lon, lat, fill = coral_cover_pred)) + 
  geom_point(shape = 21, size = 5, alpha = 0.8) + 
  scale_fill_viridis_c("")  + 
  ggtitle("Gridded Prediction")

p1 + p3

# Export gridded coral cover predictions as NetCDF files
for (i in seq_along(islands)) {
  
  gridded_coral_cover <- gam_grid %>% 
    filter(island == islands[i]) %>%
    select(lon, lat, coral_cover_pred) %>% 
    rasterFromXYZ()
  
  crs(gridded_coral_cover) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  writeRaster(x = gridded_coral_cover, 
              filename = paste0("/Users/mayaotsu/Documents/MOTSU_MASTERS/coral_cover/gridded_coral_cover_", islands[i], ".nc"), 
              format = "CDF", overwrite = TRUE)
}
