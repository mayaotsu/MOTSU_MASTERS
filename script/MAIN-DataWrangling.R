# clean desktop
rm(list = ls())
library(dplyr)
library(ggplot2)

#load spc
load("/Users/mayaotsu/Downloads/calibr_LUKA_abund.RData"); df1 = df
load("/Users/mayaotsu/Downloads/calibr_LUFU_abund.RData"); df2 = df

spc = rbind(df1, df2) %>% 
  filter(method == "nSPC")

rm(df1, df2, df)

colnames(spc)[5:6] = c("lat", "lon")

#load eds-static variables, do not forget to transform lon range to 0-360
load("/Users/mayaotsu/Documents/MOTSU_MASTERS/eds_bathymetry_rugosity.RData")
static <- df
static$lon = ifelse(df$lon < 0, df$lon + 360, df$lon)
static = static[,c(3:13)]

#add eds static variables to spc
spc = left_join(spc, static)
rm(df, static)

#add rugosity
spc = spc %>% 
  mutate(rugosity = Bathymetry_HMRG_MHI_50m_all_units_rugosity.nc) %>% 
  mutate(rugosity = ifelse(is.na(rugosity), Bathymetry_CRM_vol10_3s_all_units_rugosity.nc, rugosity)) %>% #only mains
  mutate(rugosity = ifelse(is.na(rugosity), Bathymetry_HURL_NWHI_60m_all_units_rugosity.nc, rugosity)) %>% #nwhi
  mutate(rugosity = ifelse(is.na(rugosity), Bathymetry_ETOPO_2022_v1_15s_all_units_rugosity.nc, rugosity)) #both

#add bathymetry
spc = spc %>% 
  mutate(bathymetry = Bathymetry_HMRG_MHI_50m) %>% 
  mutate(bathymetry = ifelse(is.na(bathymetry), Bathymetry_CRM_vol10_3s, bathymetry)) %>% 
  mutate(bathymetry = ifelse(is.na(bathymetry), Bathymetry_HURL_NWHI_60m, bathymetry)) %>% 
  mutate(bathymetry = ifelse(is.na(bathymetry), Bathymetry_ETOPO_2022_v1_15s, bathymetry)) 

#only keep what you need
spc = spc %>% 
  dplyr::select(colnames(spc)[1:13], "rugosity", "bathymetry")

#distance is lat/lon cell size for distance search radius around point
#function goes through each row of the dataset, once finds any value larger than -30 then it searches 0.13 decimal degrees 
#around nearby values and takes the mean of nearby values and replaces it
replace_deep_values <- function(df, threshold = -30, distance_threshold = 0.24) {
  
  # Iterate through each row to identify and replace incorrect deep values
  df <- df %>%
    rowwise() %>%
    mutate(rugosity = ifelse(
      bathymetry < threshold, {
        nearby_values <- df$rugosity[df$lon >= (lon - distance_threshold) & 
                                       df$lon <= (lon + distance_threshold) &
                                       df$lat >= (lat - distance_threshold) & 
                                       df$lat <= (lat + distance_threshold) &
                                       df$bathymetry >= threshold]
      
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

#take a look, can also do with just presence data
spc %>% 
  ggplot(aes(lon, lat, fill = rugosity)) + 
  geom_point(shape = 21)

spc %>% 
  ggplot(aes(rugosity, density, fill = rugosity)) + 
  geom_point(shape = 21)

spc %>% 
  ggplot(aes(depth, density, fill = rugosity)) + 
  geom_point(shape = 21)

#load dynamic variables
load("/Users/mayaotsu/Documents/MOTSU_MASTERS/eds_time.RData")
dynamic <- df
dynamic = dynamic[,c(3:346)]

#make a "date" column in spc, make sure it's same format
spc$date = paste(spc$year, spc$month, spc$day, sep = "-")
spc$date = as.character(spc$date)
class(df$date)
class(spc$date)

#add eds dynamic variables to spc, make sure decimal places are same
spc = spc %>% 
  mutate(lon = round(lon, 3),
         lat = round(lat, 3))

#8832 observations --> 8848
spc = left_join(spc, dynamic)
rm(df, dynamic)

#take a look
spc %>% 
  ggplot(aes(mean_Sea_Surface_Temperature_CRW_daily_01dy, density, fill = density)) + 
  geom_point(shape = 21)

#load TKE, less tke vals because starts in 2009
TKE <- readRDS("/Users/mayaotsu/Documents/MOTSU_MASTERS/spcdata_tke.rds")
TKE <- filter(TKE, method == "nSPC")
TKE = TKE %>% 
  mutate(lon = round(longitude, 3),
         lat = round(latitude, 3))
spc <- left_join(spc, TKE[, !names(TKE) %in% c("longitude", "latitude", "longwest")])
rm(TKE)

#load otp
otp <- read.csv("/Users/mayaotsu/Documents/MOTSU_MASTERS/eds_sedac_gfw_otp.csv")
otp$lon = ifelse(otp$lon < 0, otp$lon + 360, otp$lon)
otp = otp %>% 
  mutate(lon = round(lon, 3),
         lat = round(lat, 3))
spc <- left_join(spc, otp)

#increased from 8848 to 8880 observations
#spc_test2 <- left_join(spc, otp, by = c("lat", "lon", "date", "year", "month", "day"))
rm(otp)
#saveRDS(spc, "spcdata_full")

#get rid of unneeded columns from otp, eds
#colnames(spc)

#add log chla column
#spc_reduced$log_mean_1mo_chla_ESA <- log(spc_reduced$mean_1mo_chla_ESA)

#call in coral cover, left join by lat and lon
#increased from 8880 to 8944
cca <- read.csv("/Users/mayaotsu/Documents/MOTSU_MASTERS/coral_cover/spc_coral_cover.csv")
cca = cca %>% 
  mutate(lon = round(lon, 3),
         lat = round(lat, 3))
spc = left_join(spc, cca)
rm(cca)

colnames(spc)

spc_reduced <- spc %>% 
  dplyr::select(1:13, 
                "rugosity","bathymetry", "date", #rugosity and bathymetry
                # "mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01dy", 
                mean_1mo_chla_ESA = "mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01mo", 
                # "log_mean_1mo_chla_ESA", 
                #  "mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01yr",
                #  "q05_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01dy",
                #  "q05_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01mo",
                #  "q05_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01yr",
                #  "q95_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01dy",
                #  "q95_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01mo" ,
                # q95_1yr_chla_ESA = "q95_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01yr", #removed bc correlated with mean 1mo chla, 0.76
                #  "mean_Kd490_ESA_OC_CCI_monthly_01dy",
                # mean_1mo_kd490_ESA = "mean_Kd490_ESA_OC_CCI_monthly_01mo" , correlated with 1mochla 0.89
                #  "mean_Kd490_ESA_OC_CCI_monthly_01yr", 
                #  "q05_Kd490_ESA_OC_CCI_monthly_01dy", 
                #  "q05_Kd490_ESA_OC_CCI_monthly_01mo", 
                #  "q05_Kd490_ESA_OC_CCI_monthly_01yr" ,
                #  "q95_Kd490_ESA_OC_CCI_monthly_01dy" , 
                #  "q95_Kd490_ESA_OC_CCI_monthly_01mo" ,
                #  "q95_Kd490_ESA_OC_CCI_monthly_01yr" , #kd490
                # mean_1day_sst_CRW = "mean_Sea_Surface_Temperature_CRW_daily_01dy", taken out bc correlated with monthly sst 0.94
                mean_1mo_sst_CRW = "mean_Sea_Surface_Temperature_CRW_daily_01mo" , 
                #  "mean_Sea_Surface_Temperature_CRW_daily_01yr", 
                #  "q05_Sea_Surface_Temperature_CRW_daily_01dy" ,
                #  "q05_Sea_Surface_Temperature_CRW_daily_01mo",
                q05_1yr_sst_CRW = "q05_Sea_Surface_Temperature_CRW_daily_01yr" ,
                #  "q95_Sea_Surface_Temperature_CRW_daily_01dy" ,
                #  "q95_Sea_Surface_Temperature_CRW_daily_01mo" ,
                q95_1yr_sst_CRW ="q95_Sea_Surface_Temperature_CRW_daily_01yr" , #crw sst
                #  "mean_Wind_Speed_ASCAT_daily_01dy",
                #  "mean_Wind_Speed_ASCAT_daily_01mo",
                #  "mean_Wind_Speed_ASCAT_daily_01yr",
                #  "q05_Wind_Speed_ASCAT_daily_01dy" ,
                #  "q05_Wind_Speed_ASCAT_daily_01mo", 
                #  "q05_Wind_Speed_ASCAT_daily_01yr",
                #  "q95_Wind_Speed_ASCAT_daily_01dy" ,
                #  "q95_Wind_Speed_ASCAT_daily_01mo" ,
                #  "q95_Wind_Speed_ASCAT_daily_01yr" , #wind
                "TKE", #TKE
                #  "date_r" ,
               # pop_density = "gpw_v4_population_density_rev11_15_min.nc" , #5 km, #pop density, removed, correlated with MHI Shore spear 0.79
                #"gpw_v4_population_density_rev11_1_deg.nc" , #30 km,
                #"gpw_v4_population_density_rev11_2pt5_min.nc",  #60 km,
                #"gpw_v4_population_density_rev11_30_min.nc",# 110 km 
                otp_nearshore_sediment = "hi_otp_all_nearshore_sediment.tif" , #nearshore sediment (urban runoff?)
                otp_all_coastal_mod = "hi_otp_all_coastal_mod.tif" , #coastal mod
                otp_all_effluent = "hi_otp_all_osds_effluent.tif", #effluent
                "MHI_Boat_Spear_hr.tif", #spearfishing
                "MHI_Shore_Spear_hr.tif",
                "coral_cover")
#spc_reduced$log_mean_1mo_chla_ESA <- log(spc_reduced$mean_1mo_chla_ESA)

spc_reduced <- spc_reduced[!duplicated(spc_reduced),]
saveRDS(spc_reduced, "spcdata_reduced")

###################### END ##################

spc_lufu <- spc %>% filter(species == "LUFU" & presence == 1)
spc_luka <- spc %>% filter(species == "LUKA" & presence == 1)
spc_combined <- bind_rows(spc_lufu_presence, spc_luka_presence)

#combined plot
ggplot(spc_combined, aes(x = lon, y = lat, color = species)) +
  geom_point(shape = 21, size = 3) +
  scale_color_manual(values = c("LUFU" = "blue", "LUKA" = "red"), 
                     labels = c("LUFU" = "Lutjanus fulvus", "LUKA" = "Lutjanus kasmira")) +
  labs(title = "Presence of Lutjanus fulvus (LUFU) and Lutjanus kasmira (LUKA)",
       x = "Latitude",
       y = "Longitude",
       color = "Species") +
  theme_minimal()

#toau plot (LUFU)
ggplot(spc_lufu, aes(x = lon, y = lat, color = species)) +
  geom_point(shape = 21, size = 2, fill = "blue") +
  labs(title = "Presence of Lutjanus fulvus (LUFU)",
       x = "Longitude",
       y = "Latitude",
       color = "Species") +
  theme_minimal() + coord_fixed() + xlim(180, 206) +  ylim(18, 30)

#taape plot (LUKA)
ggplot(spc_luka, aes(x = lon, y = lat, color = species)) +
  geom_point(shape = 21, size = 2, fill = "red") +
  labs(title = "Presence of Lutjanus kasmira (LUKA)",
       x = "Longitude",
       y = "Latitude",
       color = "Species") +
  theme_minimal() + coord_fixed() + xlim(180, 206) +  ylim(18, 30)
