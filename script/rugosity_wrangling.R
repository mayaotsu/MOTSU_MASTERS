bathymetry_rugosity$lon <- (360+bathymetry_rugosity$lon)
bathymetry_rugosity$lon <- round(bathymetry_rugosity$lon, 3)
bathymetry_rugosity$lat <- round(bathymetry_rugosity$lat, 3)

#test replacing values on subset
bathy_subset <- bathymetry_rugosity[1400:1600, c(5, 6, 10, 11, 12, 13)]

#which rows have NA in 10 
na_in_col10 <- is.na(bathymetry_rugosity[, 10])
rows_with_na_col10 <- which(na_in_col10)
print(rows_with_na_col10)

#which rows have NA in 11
na_in_col12 <- is.na(bathymetry_rugosity[, 12])
rows_with_na_col12 <- which(na_in_col11)
print(rows_with_na_col12)

bathy_subset_test <- bathy_subset %>%
  mutate(Bathymetry_HMRG_MHI_50m = ifelse(is.na(Bathymetry_HMRG_MHI_50m),
          Bathymetry_HMRG_MHI_60m, Bathymetry_HMRG_MHI_50m))
#merged_test <- merge(df_subset, bathy_subset, by = c("lon", "lat"))

df_bathymetry_rugosity <- df %>% 
  left_join(bathymetry_rugosity, by = c("lon", "lat"))
