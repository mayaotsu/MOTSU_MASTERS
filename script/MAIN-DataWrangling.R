# clean desktop
rm(list = ls()) 
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
  filter(year >= 2009)


# df = rbind(rbind(df1, df2, df3)) %>% 
#   filter(method == "nSPC") %>% 
#   filter(region %in% c("MHI", "NWHI")) %>% 
#   filter(year >= 2009) %>% 
#   select(longitude, latitude, day, month, year) %>% 
#   distinct()
# rm(df1,df2,df3)
# save(df, file ="/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/eds_rerun.RData")
# 
# # rerun eds with eds_rerun df

rm(df1, df2,df,df3)

colnames(spc)[5:6] = c("lat", "lon")

#load eds-static variables, do not forget to transform lon range to 0-360
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/EDS_Climatologies_2025-04-16.RData") #eds_bathymetry_rugosity.Rdata
#load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_bathymetry_rugosity.RData")
static <- df
static$lon = ifelse(df$lon < 0, df$lon + 360, df$lon)
static = static[,c(3,4,6,9,11,13)]

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

#add bathymetry
#spc = spc %>% 
#  mutate(bathymetry = Bathymetry_HMRG_MHI_50m) %>% 
  # mutate(bathymetry = ifelse(is.na(bathymetry), Bathymetry_CRM_vol10_3s, bathymetry)) %>% 
  # mutate(bathymetry = ifelse(is.na(bathymetry), Bathymetry_HURL_NWHI_60m, bathymetry)) %>% 
  # mutate(bathymetry = ifelse(is.na(bathymetry), Bathymetry_ETOPO_2022_v1_15s, bathymetry)) 

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

#take a look, can also do with just presence data
#spc %>% 
 # ggplot(aes(lon, lat, fill = rugosity)) + 
 # geom_point(shape = 21)
 
# spc <- spc %>%
#   mutate(lat = round(lat, 3),
#          lon = round(lon, 3))

#load dynamic variables
#df = read.csv("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_time.csv")
#save(df, file = "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_time.RData")
load("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_time.Rdata")
#load("/Users/mayaotsu/Documents/Github/env_data_summary/outputs/EDS_Timeseries_Sea_Surface_Temperature_CRW_Daily.Rdata")
dynamic = df
dynamic = dynamic[,c(3:346)] #87 #346

spc = spc %>% dplyr::select(-date_)

#make a "date" column in spc, make sure it's same format
spc$date = paste(spc$year, spc$month, spc$day, sep = "-")
spc$date = as.character(spc$date)
class(df$date)
class(spc$date)

#add eds dynamic variables to spc, make sure decimal places are same
 # spc = spc %>% 
 #   mutate(lon = round(lon, 3),
 #          lat = round(lat, 3))

spc = left_join(spc, dynamic)
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
otp <- read.csv("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_sedac_gfw_otp_new.csv")
#otp = load("/Users/mayaotsu/Documents/Github/env_data_summary/outputs/EDS_Climatologies_NonERDDAP_2025-04-21.RData")
#otp <- df   
otp$lon = ifelse(otp$lon < 0, otp$lon + 360, otp$lon)
#otp = otp %>% select( date_r, -year, -month, -day, -date) 

 # otp = otp %>% 
 #   mutate(lon = round(lon, 3),
 #          lat = round(lat, 3))

spc <- left_join(spc, otp)

#spc_test2 <- left_join(spc, otp, by = c("lat", "lon", "date", "year", "month", "day"))
rm(otp)

#call in coral cover, left join by lat and lon
#cca <- read.csv("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_coral_cover.csv") #old
cca1 <- read.csv("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/eds_coral_cover.csv")
cca <- read.csv("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_coral_cover.csv")
cca$lon = ifelse(cca$lon < 0, cca$lon + 360, cca$lon)

# cca = cca %>%
#   mutate(lon = round(lon, 3),
#          lat = round(lat, 3))

spc = left_join(spc, cca)
rm(cca)

colnames(spc)

spc_reduced <- spc %>% 
  dplyr::select(1:14, #island, depth, method, lat, lon, species, density, presence, region, y, m, d, rugosity, date
                #"depth",
                #"rugosity",
                #"bathymetry", 
                #"date",
                # "mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01dy", 
                mean_1mo_chla_ESA = "mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01mo", 
                # "log_mean_1mo_chla_ESA", 
                #  "mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01yr",
                #  "q05_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01dy",
                #  "q05_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01mo",
                #  "q05_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01yr",
                #  "q95_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01dy",
                #  "q95_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01mo" ,
          #      q95_1yr_chla_ESA = "q95_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01yr", #removed bc correlated with mean 1mo chla, 0.76
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
          #        mean_1mo_sst_CRW = "mean_Sea_Surface_Temperature_CRW_daily_01mo" , 
                #  "mean_Sea_Surface_Temperature_CRW_daily_01yr", 
                #  "q05_Sea_Surface_Temperature_CRW_daily_01dy" ,
                #  "q05_Sea_Surface_Temperature_CRW_daily_01mo",
                q05_1yr_sst_CRW = "q05_Sea_Surface_Temperature_CRW_daily_01yr" ,
                #  "q95_Sea_Surface_Temperature_CRW_daily_01dy" ,
                #  "q95_Sea_Surface_Temperature_CRW_daily_01mo" ,
                q95_1yr_sst_CRW = "q95_Sea_Surface_Temperature_CRW_daily_01yr" , #crw sst
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
          #      com_net = "hi_otp_all_fishing_com_net.tif",
                #com_spear = "hi_otp_all_fishing_com_spear.tif", 
                #"hi_otp_all_fishing_rec.tif", 
                #rec_boat = "hi_otp_all_fishing_rec_boat.tif",
                #rec_boat_spear = "hi_otp_all_fishing_rec_boat_spear.tif",
                #rec_shore = "hi_otp_all_fishing_rec_shore.tif", 
                #rec_shore_line = "hi_otp_all_fishing_rec_shore_line.tif",
                #rec_shore_net = "hi_otp_all_fishing_rec_shore_net.tif", 
                #rec_shore_spear = "hi_otp_all_fishing_rec_shore_spear.tif"
                
                
  )

rm(spc)

#temp 28 or less 13306 --> 11443 (weird big numbres ^36)
#spc_reduced <- spc_reduced %>%
#  filter(q05_1yr_sst_CRW <= 30, q95_1yr_sst_CRW <= 30)

#get rid of duplicates
spc_reduced <- spc_reduced[!duplicated(spc_reduced), ]

#turn year and island column into a factor-- 10 levels 09,10,11,12,13,14,15,16,17,19
spc_reduced$year <- as.factor(spc_reduced$year)
spc_reduced$island <- as.factor(spc_reduced$island)
class(spc_reduced$year)
class(spc_reduced$island)

#change all OTP NWHI data NAs to 0
library(tidyr)
columns_to_modify <- c(
  #"otp_all_coastal_mod", 
  "otp_all_effluent",
  #"com_line",
  #"com_net",
  #"com_spear",
  #"rec_boat",
  #"rec_boat_spear",
  #"rec_shore",
  #"rec_shore_line",
  #"rec_shore_net",
  #"rec_shore_spear",
  "MHI_Boat_Spear_hr.tif",
  "MHI_Shore_Spear_hr.tif",
  "otp_nearshore_sediment"
)
spc_reduced <- spc_reduced %>%
  mutate(across(all_of(columns_to_modify), ~ ifelse(region == "NWHI" & is.na(.), 0, .)))

#Comine cumulative spearfishing effort (MHI_Boat_Spear_hr + MHI_Shore_Spear_hr)
spc_reduced$MHI_spear <- spc_reduced$MHI_Boat_Spear_hr.tif + spc_reduced$MHI_Shore_Spear_hr.tif
unique(is.na(spc_reduced$MHI_spear))

#take away mhi spear and mhi boat spear columns
spc_reduced = spc_reduced %>% 
  dplyr::select(-MHI_Shore_Spear_hr.tif, -MHI_Boat_Spear_hr.tif)

#Change kahoolawe MHI spearfish to 0
spc_reduced[spc_reduced$island == "Kahoolawe", c("MHI_spear")] <- 0
unique(spc_reduced[spc_reduced$island == "Kahoolawe", c("MHI_spear")])

# change NAs in nearshore sediment to 0
spc_reduced$otp_nearshore_sediment[is.na(spc_reduced$otp_nearshore_sediment)] <- 0

#NAs in each row 
colSums(is.na(spc_reduced))

#how many presence values in presence column before na.omit
sum(spc_reduced$presence == 1, na.rm = TRUE) #1804

##### NA OMIT !!!! #####
spc_reduced <- na.omit(spc_reduced)
sum(spc_reduced$presence == 1, na.rm = TRUE) #1753 --> 1579, 8805 total obs
colSums(is.na(spc_reduced))

#avg values between thee two duplicate rows to keep one, 12350--> 7151
colnames(spc_reduced)

#7151-> 5065
spc_reduced = spc_reduced %>% 
  group_by(island, method, lat, lon, species, date, presence, region, year, month, day) %>% 
  summarise(density = mean(density, na.rm = T),
            depth = mean(depth, na.rm = T),
            rugosity = mean(rugosity, na.rm = T),
            #bathymetry = mean(bathymetry, na.rm = T), 
            mean_1mo_chla_ESA = mean(mean_1mo_chla_ESA, na.rm = T),
            #q95_1yr_chla_ESA = mean(q95_1yr_chla_ESA, na.rm = T),
            q05_1yr_sst_CRW = mean(q05_1yr_sst_CRW, na.rm = T),
            q95_1yr_sst_CRW = mean(q95_1yr_sst_CRW, na.rm = T),
            otp_nearshore_sediment = mean(otp_nearshore_sediment, na.rm = T),
            otp_all_effluent = mean(otp_all_effluent, na.rm = T),
            coral_cover = mean(coral_cover, na.rm = T),
            #com_net = mean(com_net, na.rm = T),
            MHI_spear = mean(MHI_spear, na.rm = T)
  ) %>%
  dplyr::select(island, depth, method, lat, lon, species, density, presence,
         region, year, month, day, rugosity, date,
         mean_1mo_chla_ESA, q05_1yr_sst_CRW, q95_1yr_sst_CRW,
         otp_nearshore_sediment, coral_cover, otp_all_effluent, MHI_spear) #bathymetry

# sanity checks 
# dup <- spc_reduced[duplicated(spc_reduced[c("island", "method", "lat", "lon", "species", "date", "presence", "region", "year", "month", "day")]) | duplicated(spc_reduced[c("island", "method", "lat", "lon", "species", "date", "presence", "region", "year", "month", "day")], fromLast=T),]
# # how many extra rows do we have? 
# sum(duplicated(spc_reduced[c("island", "method", "lat", "lon", "species", "date", "presence", "region", "year", "month", "day")]))
# # 3740 which is the same as nrow(test)-nrow(spc_reduced)

#SAVE CUMULATIVE LAYER
spc_full <- spc_reduced[!duplicated(spc_reduced),] #spc_reduced_spearcumulative_roi
save(spc_reduced, file ="/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_full.RData")#spc_edited_cumulative.RData
saveRDS(spc_reduced, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_full")

#SAVE JUST MHI
spc_mhi = subset(spc_reduced, region == "MHI")
save(spc_mhi, file ="/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_mhi.RData") #spc_edited_cumulative_JUSTMHI_roi.RData
saveRDS(spc_reduced, "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc_mhi")

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
  ggplot(spc_reduced, aes(x = lon, y = lat)) +
  geom_point(aes(color = q05_1yr_sst_CRW)) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "q05 SST (CRW)", color = "SST (°C)")

  ggplot(spc_reduced, aes(x = lon, y = lat)) +
    geom_point(aes(color = q95_1yr_sst_CRW)) +
    scale_color_viridis_c() +
    theme_minimal() +
    labs(title = "q95 SST (CRW)", color = "SST (°C)")
  
#pa by lat/lon SST q05
  ggplot(spc_reduced, aes(x = lon, y = lat)) +
    geom_point(aes(color = q05_1yr_sst_CRW, shape = factor(presence)), size = 2, alpha = 0.7) +
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
  ggplot(spc, aes(x = lon, y = lat)) +
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
  