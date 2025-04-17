# clean desktop
rm(list = ls()) 
library(dplyr)
library(ggplot2)

#load spc
load("/Users/mayaotsu/Downloads/calibr_LUKA_abund.RData"); df1 = df
load("/Users/mayaotsu/Downloads/calibr_LUFU_abund.RData"); df2 = df
load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/SPC25_CEAR.RData"); df3 = df

spc = rbind(rbind(df1, df2, df3) %>% 
  filter(method == "nSPC"))

rm(df1, df2,df,df3)

colnames(spc)[5:6] = c("lat", "lon")

#load eds-static variables, do not forget to transform lon range to 0-360
#load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/EDS_Climatologies_2025-04-16.RData") #eds_bathymetry_rugosity.Rdata
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_bathymetry_rugosity.RData")
static <- df
static$lon = ifelse(df$lon < 0, df$lon + 360, df$lon)
static = static[,c(3:13)]

# make sure decimal places are same
spc = spc %>% 
  mutate(lon = round(lon, 4),
         lat = round(lat, 5))
spc$lat <- as.numeric(spc$lat)

static = static %>% 
  mutate(lon = round(lon, 4),
         lat = round(lat, 5))
static$lat <- as.numeric(static$lat)

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
#spc %>% 
 # ggplot(aes(lon, lat, fill = rugosity)) + 
 # geom_point(shape = 21)

#spc %>% 
 # ggplot(aes(rugosity, density, fill = rugosity)) + 
  #geom_point(shape = 21)

#spc %>% 
 # ggplot(aes(depth, density, fill = rugosity)) + 
  #geom_point(shape = 21)

#load dynamic variables
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_time.Rdata")
#df <- read.csv("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_time.csv")
dynamic <- df[, 3:346]
#if Rdata:
#dynamic = df
#dynamic = dynamic[,c(3:346)] how to keep thiscode but change for a csv file instead of rdata

#make a "date" column in spc, make sure it's same format
spc$date = paste(spc$year, spc$month, spc$day, sep = "-")
spc$date = as.character(spc$date)
#class(df$date)
#class(spc$date)

#add eds dynamic variables to spc, make sure decimal places are same
spc = spc %>% 
  mutate(lon = round(lon, 3),
         lat = round(lat, 3))

#8832 observations --> 8848
spc = left_join(spc, dynamic)
rm(df, dynamic)

#take a look
# spc %>%
# ggplot(aes(mean_Sea_Surface_Temperature_CRW_daily_01dy, density, fill = density)) +
# geom_point(shape = 21)

#load TKE, less tke vals because starts in 2009
TKE <- readRDS("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spcdata_tke.rds")
TKE <- filter(TKE, method == "nSPC")
TKE = TKE %>% 
  mutate(lon = round(longitude, 3),
         lat = round(latitude, 3))
spc <- left_join(spc, TKE[, !names(TKE) %in% c("longitude", "latitude", "longwest")])
rm(TKE)

#load otp
otp <- read.csv("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_sedac_gfw_otp_old.csv")
otp$lon = ifelse(otp$lon < 0, otp$lon + 360, otp$lon)
otp = otp %>% 
  mutate(lon = round(lon, 3),
         lat = round(lat, 3))
spc <- left_join(spc, otp)

#increased from 8848 to 8880 observations
#spc_test2 <- left_join(spc, otp, by = c("lat", "lon", "date", "year", "month", "day"))
rm(otp)

#get rid of unneeded columns from otp, eds

#call in coral cover, left join by lat and lon
#increased from 8880 to 8944
cca <- read.csv("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_coral_cover.csv")
#cca <- read.csv("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_coral_cover.csv")
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
                q95_1yr_chla_ESA = "q95_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01yr", #removed bc correlated with mean 1mo chla, 0.76
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
                #mean_1mo_sst_CRW = "mean_Sea_Surface_Temperature_CRW_daily_01mo" , 
                #  "mean_Sea_Surface_Temperature_CRW_daily_01yr", 
                #  "q05_Sea_Surface_Temperature_CRW_daily_01dy" ,
                #  "q05_Sea_Surface_Temperature_CRW_daily_01mo",
                q05_1yr_sst_CRW = "q05_Sea_Surface_Temperature_CRW_daily_01yr" ,
                #  "q95_Sea_Surface_Temperature_CRW_daily_01dy" ,
                #  "q95_Sea_Surface_Temperature_CRW_daily_01mo" ,
    # get rid q95_1yr_sst_CRW ="q95_Sea_Surface_Temperature_CRW_daily_01yr" , #crw sst
                #  "mean_Wind_Speed_ASCAT_daily_01dy",
                #  "mean_Wind_Speed_ASCAT_daily_01mo",
                #  "mean_Wind_Speed_ASCAT_daily_01yr",
                #  "q05_Wind_Speed_ASCAT_daily_01dy" ,
                #  "q05_Wind_Speed_ASCAT_daily_01mo", 
                #  "q05_Wind_Speed_ASCAT_daily_01yr",
                #  "q95_Wind_Speed_ASCAT_daily_01dy" ,
                #  "q95_Wind_Speed_ASCAT_daily_01mo" ,
                #  "q95_Wind_Speed_ASCAT_daily_01yr" , #wind
                # "TKE", #TKE
                #  "date_r" ,
                # pop_density = "gpw_v4_population_density_rev11_15_min.nc" , #5 km, #pop density, removed, correlated with MHI Shore spear 0.79
                #"gpw_v4_population_density_rev11_1_deg.nc" , #30 km,
                #"gpw_v4_population_density_rev11_2pt5_min.nc",  #60 km,
                #"gpw_v4_population_density_rev11_30_min.nc",# 110 km 
                otp_nearshore_sediment = "hi_otp_all_nearshore_sediment.tif" , #nearshore sediment (urban runoff?)
                #otp_all_coastal_mod = "hi_otp_all_coastal_mod.tif" , #coastal mod
                otp_all_effluent = "hi_otp_all_osds_effluent.tif", #effluent
                "MHI_Boat_Spear_hr.tif", #spearfishing
                "MHI_Shore_Spear_hr.tif",
                "coral_cover", 
                #"hi_otp_all_fishing.tif", 
                #"hi_otp_all_fishing_com.tif", 
                #com_line = "hi_otp_all_fishing_com_line.tif",
                com_net = "hi_otp_all_fishing_com_net.tif",
                #com_spear = "hi_otp_all_fishing_com_spear.tif", 
                #"hi_otp_all_fishing_rec.tif", 
                #rec_boat = "hi_otp_all_fishing_rec_boat.tif",
                #rec_boat_spear = "hi_otp_all_fishing_rec_boat_spear.tif",
                #rec_shore = "hi_otp_all_fishing_rec_shore.tif", 
                #rec_shore_line = "hi_otp_all_fishing_rec_shore_line.tif",
                #rec_shore_net = "hi_otp_all_fishing_rec_shore_net.tif", 
                #rec_shore_spear = "hi_otp_all_fishing_rec_shore_spear.tif"
                
                
  )

#turn year and island column into a factor-- 10 levels 09,10,11,12,13,14,15,16,17,19
spc_reduced$year <- as.factor(spc_reduced$year)
spc_reduced$island <- as.factor(spc_reduced$island)
class(spc_reduced$year)
class(spc_reduced$island)

#change all OTP NWHI data NAs except sedimentation data to 0
library(tidyr)
columns_to_modify <- c(
  #"otp_all_coastal_mod", 
  "otp_all_effluent",
  #"com_line",
  "com_net",
  #"com_spear",
  #"rec_boat",
  #"rec_boat_spear",
  #"rec_shore",
  #"rec_shore_line",
  #"rec_shore_net",
  #"rec_shore_spear",
  "MHI_Boat_Spear_hr.tif",
  "MHI_Shore_Spear_hr.tif"
)
spc_reduced <- spc_reduced %>%
  mutate(across(all_of(columns_to_modify), ~ ifelse(region == "NWHI" & is.na(.), 0, .)))

#Comine cumulative spearfishing effort (MHI_Boat_Spear_hr + MHI_Shore_Spear_hr)
spc_reduced$MHI_spear <- spc_reduced$MHI_Boat_Spear_hr.tif + spc_reduced$MHI_Shore_Spear_hr.tif
unique(is.na(spc_reduced$MHI_spear))

#take away mhi spear and mhi boat spear columns
spc_reduced = spc_reduced %>% 
  dplyr::select(-MHI_Shore_Spear_hr.tif, -MHI_Boat_Spear_hr.tif)

#exclude midway 
spc_reduced <- spc_reduced %>% filter(island!= "Midway")
unique(spc_reduced$island)

#set sediment and comm net NAs to 0s
spc_reduced <- spc_reduced %>%
  mutate(com_net = replace_na(com_net, 0),
         otp_nearshore_sediment = replace_na(otp_nearshore_sediment, 0)
  )

#get rid of remaining rows with NAs
spc_reduced <- na.omit(spc_reduced)

#SAVE CUMULATIVE LAYER
spc_reduced_spearcumulative_roi <- spc_reduced[!duplicated(spc_reduced),]
save(spc_reduced_spearcumulative_roi, file ="/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_edited_cumulative.RData")
saveRDS(spc_reduced_spearcumulative_roi, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_edited_cumulative")

#save rdata
#spc_reduced <- spc_reduced[!duplicated(spc_reduced),]
#save(spc_reduced, file ="/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_edited.RData")

#saverds
#saveRDS(spc_reduced, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_edited")

#SAVE JUST MHI
spc_reduced_roi = subset(spc_reduced, region == "MHI")
save(spc_reduced, file ="/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_edited_cumulative_JUSTMHI_roi.RData")
saveRDS(spc_reduced, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_edited_cumulative_JUSTMHI_roi")

###################### END ###################
     