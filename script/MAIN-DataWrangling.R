# clean desktop
rm(list= ls()) 
library(dplyr)
library(ggplot2)
select=dplyr::select

#load spc
load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/calibr_LUKA_abund.RData"); df1 = df
load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/calibr_LUFU_abund.RData"); df2 = df
load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/SPC25_CEAR.RData"); df3 = df

spc = rbind(rbind(df1, df2, df3)) %>%
  filter(method == "nSPC") %>%
  filter(region %in% c("MHI", "NWHI")) %>%
  filter(year >= 2009 & year < 2024)
rm(df, df1,df2,df3)

# df = rbind(rbind(df1, df2, df3)) %>% 
#   filter(method == "nSPC") %>% 
#   filter(region %in% c("MHI", "NWHI")) %>% 
#   filter(year >= 2009) %>% 
#   select(longitude, latitude, day, month, year) %>% 
#   distinct()
# rm(df1,df2,df3)
# save(df, file ="/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/eds_rerun.RData")
# rm(df1, df2,df,df3)
# # rerun eds with eds_rerun df

colnames(spc)[5:6] = c("lat", "lon")

#load eds-static variables, do not forget to transform lon range to 0-360
#df = read.csv("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/EDS_Climatologies_2025-06-25.csv")
#save(df, file = "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/EDS_Climatologies_2025-06-25.RData")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/EDS_Climatologies_2025-06-25.RData") #eds_bathymetry_rugosity.Rdata
static <- df
static$lon = ifelse(df$lon < 0, df$lon + 360, df$lon)
static = static[,c(3,4,6,9,11,13)] #lat, lon, 

# make sure decimal places are same
#spc = spc %>% 
#  mutate(lon = round(lon, 4),
#         lat = round(lat, 5))
#spc$lat <- as.numeric(spc$lat)

#static = static %>% 
#  mutate(lon = round(lon, 4),
#         lat = round(lat, 5))
#static$lat <- as.numeric(static$lat)

#add eds static variables to spc
spc = left_join(spc, static)
rm(df, static)

#add rugosity
spc = spc %>% 
  mutate(rugosity = Bathymetry_HMRG_MHI_50m_all_units_rugosity.nc) %>% 
  mutate(rugosity = ifelse(is.na(rugosity), Bathymetry_CRM_vol10_3s_all_units_rugosity.nc, rugosity)) %>% #only mains
  mutate(rugosity = ifelse(is.na(rugosity), Bathymetry_HURL_NWHI_60m_all_units_rugosity.nc, rugosity)) %>% #nwhi
  mutate(rugosity = ifelse(is.na(rugosity), Bathymetry_ETOPO_2022_v1_15s_all_units_rugosity.nc, rugosity)) #both

#only keep what you need
spc = spc %>% 
  dplyr::select(colnames(spc)[1:13], "rugosity") %>% na.omit()

#distance is lat/lon cell size for distance search radius around point
#function goes through each row of the dataset, once finds any value larger than -30 then it searches 0.13 decimal degrees 
#around nearby values and takes the mean of nearby values and replaces it
replace_deep_values <- function(df, threshold = 30, distance_threshold = 0.24) {
  
  # Iterate through each row to identify and replace incorrect deep values
  df <- df %>%
    rowwise() %>%
    mutate(rugosity = ifelse(
      depth > threshold, {
        nearby_values <- df$rugosity[df$lon >= (lon - distance_threshold) & 
                                       df$lon <= (lon + distance_threshold) &
                                       df$lat >= (lat - distance_threshold) & 
                                       df$lat <= (lat + distance_threshold) &
                                       df$depth <= threshold]
      
        # Check if there are any valid nearby values to calculate the mean
        if(length(nearby_values) > 0) {
          mean(nearby_values, na.rm = TRUE)
        } else {
          rugosity  # Keep original value if no valid nearby values
        }
      },
      rugosity
    )) %>%
    ungroup()
  
  return(df)
}

# Apply the function to original df
spc <- replace_deep_values(spc)
spc = spc %>% as.data.frame()
summary(spc$rugosity)
 
# spc <- spc %>%
#   mutate(lat = round(lat, 3),
#          lon = round(lon, 3))

#load dynamic variables
# df = read.csv("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_time_06.25.csv")
# save(df, file = "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_time_06.25.RData")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_time_06.25.Rdata")
dynamic = df
dynamic = dynamic[,c(3:346)] 
dynamic$date = as.POSIXct(dynamic$date, origin = "1970-01-01", tz = "UTC")

#spc = spc %>% dplyr::select(-date)

#make a "date" column in spc, make sure it's same format
spc$date <- as.Date(spc$date_)
dynamic$date <- as.Date(dynamic$date)
class(dynamic$date)
class(spc$date)

#add eds dynamic variables to spc, make sure decimal places are same
 # spc = spc %>% 
 #   mutate(lon = round(lon, 3),
 #          lat = round(lat, 3))

spc <- left_join(spc, dynamic, by = c("lat", "lon", "date"))
#spc = left_join(spc, dynamic)
rm(df, dynamic)

#load TKE, less tke vals because starts in 2009
# TKE <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spcdata_tke.rds")
# TKE <- filter(TKE, method == "nSPC")
# TKE = TKE %>% 
#   mutate(lon = round(longitude, 3),
#          lat = round(latitude, 3))
# spc <- left_join(spc, TKE[, !names(TKE) %in% c("longitude", "latitude", "longwest")])
# rm(TKE)

#load otp
otp = load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/EDS_Climatologies_NonERDDAP_2025-06-25.RData")
otp = df
otp$lon = ifelse(otp$lon < 0, otp$lon + 360, otp$lon)
#otp = otp %>% select( date_r, -year, -month, -day, -date) 

 # otp = otp %>% 
 #   mutate(lon = round(lon, 3),
 #          lat = round(lat, 3))

spc <- left_join(spc, otp)

rm(otp,df)

#call in coral cover, left join by lat and lon
#cca <- read.csv("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_coral_cover_new.csv")
#save(cca, file = "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_coral_cover_new.RData")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_coral_cover_new.RData")
cca$lon = ifelse(cca$lon < 0, cca$lon + 360, cca$lon)
cca$date <- as.Date(cca$date)
spc$date <- as.Date(spc$date_)

#replace _ with spaces for pearl and hermes
cca$island <- gsub("_", " ", cca$island)

class(cca$date)
class(spc$date)
# cca = cca %>%
#   mutate(lon = round(lon, 3),
#          lat = round(lat, 3))

spc <- left_join(spc, cca, by = c("island", "lon", "lat", "region", "year", "month", "day", "date"))
#pc = left_join(spc, cca)
rm(cca)

colnames(spc)

spc_reduced <- spc %>% 
  dplyr::select(1:15, #island, depth, method, lat, lon, species, density, presence, region, y, m, d, rugosity, date
                #"depth",
                #"rugosity",
                #"bathymetry", 
                #"date",
                mean_1mo_chla_ESA = "mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01mo", 
          #      q95_1yr_chla_ESA = "q95_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01yr", #removed bc correlated with mean 1mo chla, 0.76
                # mean_1mo_kd490_ESA = "mean_Kd490_ESA_OC_CCI_monthly_01mo" , correlated with 1mochla 0.89
                # mean_1day_sst_CRW = "mean_Sea_Surface_Temperature_CRW_daily_01dy", taken out bc correlated with monthly sst 0.94
                #  mean_1mo_sst_CRW = "mean_Sea_Surface_Temperature_CRW_daily_01mo" , 
                q05_1yr_sst_jpl = "q05_Sea_Surface_Temperature_jplMUR_daily_01yr",
                q95_1yr_sst_jpl = "q95_Sea_Surface_Temperature_jplMUR_daily_01yr",
                #q05_1yr_sst_CRW = "q05_Sea_Surface_Temperature_CRW_daily_01yr" ,
                #q95_1yr_sst_CRW = "q95_Sea_Surface_Temperature_CRW_daily_01yr" , #crw sst
                # "TKE", #TKE
                #  "date_r" ,
                # pop_density = "gpw_v4_population_density_rev11_15_min.nc" , #5 km, #pop density, removed, correlated with MHI Shore spear 0.79
                otp_nearshore_sediment = "hi_otp_all_nearshore_sediment.tif" , #nearshore sediment (urban runoff?)
                #otp_all_coastal_mod = "hi_otp_all_coastal_mod.tif" , #coastal mod
                otp_all_effluent = "hi_otp_all_osds_effluent.tif", #effluent
                "MHI_Boat_Spear_hr.tif", #spearfishing
                "MHI_Shore_Spear_hr.tif",
                "coral_cover", 
          #      com_net = "hi_otp_all_fishing_com_net.tif",
  )

rm(spc)

#get rid of duplicates
spc_reduced <- spc_reduced[!duplicated(spc_reduced), ]

#turn year and island column into a factor-- 10 levels 09,10,11,12,13,14,15,16,17,19
spc_reduced$year <- as.factor(spc_reduced$year)
spc_reduced$island <- as.factor(spc_reduced$island)
class(spc_reduced$year)
class(spc_reduced$island)

#Combine cumulative spearfishing effort (MHI_Boat_Spear_hr + MHI_Shore_Spear_hr)
spc_reduced$MHI_spear <- spc_reduced$MHI_Boat_Spear_hr.tif + spc_reduced$MHI_Shore_Spear_hr.tif
unique(is.na(spc_reduced$MHI_spear))

#take away mhi spear and mhi boat spear columns
spc_reduced = spc_reduced %>% 
  dplyr::select(-MHI_Shore_Spear_hr.tif, -MHI_Boat_Spear_hr.tif)

#change all OTP NWHI data NAs to 0
spc_reduced <- spc_reduced %>%
  mutate(
    otp_all_effluent = ifelse(region == "NWHI" & is.na(otp_all_effluent), 0, otp_all_effluent),
    MHI_spear = ifelse(region == "NWHI" & is.na(MHI_spear), 0, MHI_spear),
    otp_nearshore_sediment = ifelse(region == "NWHI" & is.na(otp_nearshore_sediment), 0, otp_nearshore_sediment)
  )

#Change kahoolawe MHI spearfish to 0
spc_reduced[spc_reduced$island == "Kahoolawe", c("MHI_spear")] <- 0
unique(spc_reduced[spc_reduced$island == "Kahoolawe", c("MHI_spear")])

# change NAs in nearshore sediment to 0
spc_reduced$otp_nearshore_sediment[is.na(spc_reduced$otp_nearshore_sediment)] <- 0

#NAs in each row 
colSums(is.na(spc_reduced))

#how many presence values in presence column before na.omit
sum(spc_reduced$presence == 1, na.rm = TRUE) #1804

####################################
#fix mhi spear and effluent NA values, raster extract
####################################
library(raster)
library(dplyr)
library(ggplot2)

#function
fill_missing_column <- function(main_df, patch_df, join_cols, var_to_fill) {
  patched <- dplyr::left_join(
    main_df,
    dplyr::select(patch_df, dplyr::all_of(c(join_cols, var_to_fill))),
    by = join_cols,
    suffix = c("", ".new")
  ) %>%
    dplyr::mutate(
      !!var_to_fill := dplyr::coalesce(.data[[var_to_fill]], .data[[paste0(var_to_fill, ".new")]])
    ) %>%
    dplyr::select(-dplyr::all_of(paste0(var_to_fill, ".new")))
  
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

rm(combined_spear, na_points_spear, na_points_unique_spear, points_spear)
#rm(filled_rows, filled_rows_MHI, changed_rows, filled_count, filled_MHI, unchanged)
##################
#####EFFLUENT#####
##################
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
  var_to_fill = "otp_all_effluent")

#final check
plot(spc_filled$MHI_spear, spc_reduced$MHI_spear)
plot(spc_filled$otp_all_effluent, spc_reduced$otp_all_effluent)

rm(effluent_raster, na_points_eff, na_points_unique_eff, points_eff)
rm(unchanged)

#saveRDS(spc_filled, "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_filled.rds")
colSums(is.na(spc_filled))
spc_reduced <- spc_filled
rm(spc_filled)

##### NA OMIT !!!! #####
colSums(is.na(spc_reduced))
spc_reduced<- na.omit(spc_reduced)
sum(spc_reduced$presence == 1, na.rm = TRUE) 
colSums(is.na(spc_reduced))

#avg values between thee two duplicate rows to keep one, 12350--> 7151
colnames(spc_reduced)

library(dplyr)
#get mean of values that have different depths but identical all other values #6147 values
spc_reduced = spc_reduced %>% 
  group_by(island, method, lat, lon, species, date_, presence, region, year, month, day) %>% 
  dplyr::summarise(
            density = mean(density, na.rm = T),
            depth = mean(depth, na.rm = T),
            rugosity = mean(rugosity, na.rm = T),
            mean_1mo_chla_ESA = mean(mean_1mo_chla_ESA, na.rm = T),
            otp_nearshore_sediment = mean(otp_nearshore_sediment, na.rm = T),
            otp_all_effluent = mean(otp_all_effluent, na.rm = T),
            coral_cover = mean(coral_cover, na.rm = T),
            MHI_spear = mean(MHI_spear, na.rm = T), 
            q05_1yr_sst_jpl = mean(q05_1yr_sst_jpl, na.rm =T),
            q95_1yr_sst_jpl = mean(q95_1yr_sst_jpl, na.rm =T)
  ) %>%
  dplyr::select(depth, method, date_, lat, lon, species, density, presence,
         region, year, month, day, rugosity,
         mean_1mo_chla_ESA, q05_1yr_sst_jpl, q95_1yr_sst_jpl,
         otp_nearshore_sediment, coral_cover, otp_all_effluent, MHI_spear) #bathymetry

#SAVE CUMULATIVE LAYER
spc_full <- spc_reduced[!duplicated(spc_reduced),] 
save(spc_reduced, file ="/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_full_07.07.RData")#spc_edited_cumulative.RData
saveRDS(spc_reduced, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_full_07.07")

#SAVE JUST MHI
spc_mhi = subset(spc_reduced, region == "MHI")
save(spc_mhi, file ="/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_mhi_07.07.RData") #spc_edited_cumulative_JUSTMHI_roi.RData
saveRDS(spc_reduced, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_mhi_07.07")

###################### END ###################
     
colSums(is.na(spc_reduced))

## coral cover
spc_reduced %>%
  group_by(island) %>%
  summarize(
    total_obs = n(),
    na_count = sum(is.na(coral_cover)),
    percent_na = round((na_count / total_obs) * 100, 2)
  )

missing_coral <- spc_reduced[is.na(spc_reduced$coral_cover), ]
ggplot(missing_coral, aes(x = lon, y = lat)) +
  geom_point(color = "darkorange", size = 3) +
  labs(title = "Missing Coral Cover Data",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

## spear
missing_spear <- spc_reduced[is.na(spc_reduced$MHI_spear), ]
ggplot(missing_spear, aes(x = lon, y = lat)) +
  geom_point(color = "darkgreen", size = 3) +
  labs(title = "Missing MHI Spearfishing Data",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

## net
missing_net <- spc_reduced[is.na(spc_reduced$com_net), ]
ggplot(missing_net, aes(x = lon, y = lat)) +
  geom_point(color = "darkgreen", size = 3) +
  labs(title = "Missing com net Data",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

spc_reduced %>%
  group_by(island) %>%
  summarize(
    total_obs = n(),
    na_count = sum(is.na(com_net)),
    percent_na = round((na_count / total_obs) * 100, 2)
  )


## sediment
spc_reduced %>%
  group_by(island) %>%
  summarize(
    total_obs = n(),
    na_count = sum(is.na(otp_nearshore_sediment)),
    percent_na = round((na_count / total_obs) * 100, 2)
  )

missing_sed <- spc_reduced[is.na(spc_reduced$otp_nearshore_sediment), ]
ggplot(missing_sed, aes(x = lon, y = lat)) +
  geom_point(color = "darkgreen", size = 3) +
  labs(title = "Missing sediment Data",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

## chla
spc_reduced %>%
  group_by(island) %>%
  summarize(
    total_obs = n(),
    na_count = sum(is.na(mean_1mo_chla_ESA)),
    percent_na = round((na_count / total_obs) * 100, 2)
  )

##sst
spc_reduced %>%
  group_by(island) %>%
  summarize(
    total_obs = n(),
    na_count = sum(is.na(q05_1yr_sst_CRW)),
    percent_na = round((na_count / total_obs) * 100, 2)
  )

spc_reduced %>%
  group_by(island) %>%
  summarize(
    total_obs = n(),
    na_count = sum(is.na(q95_1yr_sst_CRW)),
    percent_na = round((na_count / total_obs) * 100, 2)
  )

spc_reduced %>%
  group_by(year) %>%
  summarize(
    total_obs = n(),
    na_count = sum(is.na(q05_1yr_sst_CRW)),
    percent_na = round((na_count / total_obs) * 100, 2)
  )

colnames(spc_reduced)
unique(spc_reduced$island[which(spc_reduced$q05_1yr_sst_CRW>50)])
plot(spc_reduced$lon[which(spc_reduced$q05_1yr_sst_CRW>50)], (spc_reduced$lat[which(spc_reduced$q05_1yr_sst_CRW>50)]))

  
## sst map

#pa by lat/lon SST q05
  ggplot(spc_reduced, aes(x = lon, y = lat)) +
    geom_point(aes(color = q05_1yr_sst_jpl, shape = factor(presence)), size = 2, alpha = 0.7) +
    scale_color_viridis_c(name = "q05 SST (°C)", option = "C") +
    scale_shape_manual(values = c(1, 19), name = "Presence", labels = c("Absent", "Present")) +
    theme_minimal() +
    labs(title = "Presence/Absence and q05 SST",
         x = "Longitude",
         y = "Latitude") +
    theme(legend.position = "right")
  
#sst q95
  ggplot(spc_reduced, aes(x = lon, y = lat)) +
    geom_point(aes(color = q95_1yr_sst_CRW, shape = factor(presence)), size = 2, alpha = 0.7) +
    scale_color_viridis_c(name = "q95 SST (°C)", option = "C") +
    scale_shape_manual(values = c(1, 19), name = "Presence", labels = c("Absent", "Present")) +
    theme_minimal() +
    labs(title = "Presence/Absence and q95 SST",
         x = "Longitude",
         y = "Latitude") +
    theme(legend.position = "right")
  
#rugosity
  ggplot(spc_reduced, aes(x = lon, y = lat)) +
    geom_point(aes(color = rugosity, shape = factor(presence)), size = 2, alpha = 0.7) +
    scale_color_viridis_c(name = "rugosity", option = "C") +
    scale_shape_manual(values = c(1, 19), name = "Presence", labels = c("Absent", "Present")) +
    theme_minimal() +
    labs(title = "Presence/Absence and rugosity",
         x = "Longitude",
         y = "Latitude") +
    theme(legend.position = "right")
  
#mean 1mo chla
  ggplot(spc_reduced, aes(x = lon, y = lat)) +
    geom_point(aes(color = mean_1mo_chla_ESA, shape = factor(presence)), size = 2, alpha = 0.7) +
    scale_color_viridis_c(name = "chla mean 1mo", option = "C") +
    scale_shape_manual(values = c(1, 19), name = "Presence", labels = c("Absent", "Present")) +
    theme_minimal() +
    labs(title = "Presence/Absence and chla",
         x = "Longitude",
         y = "Latitude") +
    theme(legend.position = "right")
  
#nearshore sediment
  ggplot(spc_reduced, aes(x = lon, y = lat)) +
    geom_point(aes(color = otp_nearshore_sediment, shape = factor(presence)), size = 2, alpha = 0.7) +
    scale_color_viridis_c(name = "nearshores sediment", option = "C") +
    scale_shape_manual(values = c(1, 19), name = "Presence", labels = c("Absent", "Present")) +
    theme_minimal() +
    labs(title = "Presence/Absence and chla",
         x = "Longitude",
         y = "Latitude") +
    theme(legend.position = "right")
  
#effluent
  ggplot(spc_reduced, aes(x = lon, y = lat)) +
    geom_point(aes(color = otp_all_effluent, shape = factor(presence)), size = 2, alpha = 0.7) +
    scale_color_viridis_c(name = "effluent", option = "C") +
    scale_shape_manual(values = c(1, 19), name = "Presence", labels = c("Absent", "Present")) +
    theme_minimal() +
    labs(title = "Presence/Absence and effluent",
         x = "Longitude",
         y = "Latitude") +
    theme(legend.position = "right")
  
#coral cover
  ggplot(spc_reduced, aes(x = lon, y = lat)) +
    geom_point(aes(color = coral_cover, shape = factor(presence)), size = 2, alpha = 0.7) +
    scale_color_viridis_c(name = "coral cover", option = "C") +
    scale_shape_manual(values = c(1, 19), name = "Presence", labels = c("Absent", "Present")) +
    theme_minimal() +
    labs(title = "Presence/Absence and coral cover",
         x = "Longitude",
         y = "Latitude") +
    theme(legend.position = "right")
  
#spear
  ggplot(spc_reduced, aes(x = lon, y = lat)) +
    geom_point(aes(color = MHI_spear, shape = factor(presence)), size = 2, alpha = 0.7) +
    scale_color_viridis_c(name = "spear", option = "C") +
    scale_shape_manual(values = c(1, 19), name = "Presence", labels = c("Absent", "Present")) +
    theme_minimal() +
    labs(title = "Presence/Absence and spear",
         x = "Longitude",
         y = "Latitude") +
    theme(legend.position = "right")
  
  

  