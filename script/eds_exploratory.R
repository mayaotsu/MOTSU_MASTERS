library(dplyr)
library(ggplot2)

rm(list=ls())
#getwd()

# load environmental data
#sst_chla_wind_kd490 <- load("/Users/mayaotsu/Documents/MOTSU_MASTERS/eds_time.RData"); eds = df
#bathymetry_rugosity <- load("/Users/mayaotsu/Documents/MOTSU_MASTERS/eds_bathymetry_rugosity.RData"); bathymetry_rugosity = df
load("/Users/mayaotsu/Documents/MOTSU_MASTERS/eds_time.RData")
sst_chla_wind_kd490 <- df
load("/Users/mayaotsu/Documents/MOTSU_MASTERS/eds_bathymetry_rugosity.RData")
bathymetry_rugosity <- df
TKE <- readRDS("/Users/mayaotsu/Documents/MOTSU_MASTERS/spcdata_tke.rds")
eds_sedac_gfw_otp <- read.csv("/Users/mayaotsu/Documents/MOTSU_MASTERS/eds_sedac_gfw_otp.csv")

#glimpse(eds)

#load fish data
load("/Users/mayaotsu/Downloads/calibr_LUKA_abund.RData"); df1 = df
load("/Users/mayaotsu/Downloads/calibr_LUFU_abund.RData"); df2 = df


df = rbind(df1, df2) %>% 
  mutate(longitude = round(longitude, 3),
         latitude = round(latitude, 3)) %>%
  distinct() 


colnames(df)[5:6] = c("lat", "lon")
df$date <- paste(df$year, df$month, df$day, sep = "-")


df_sst_chla_wind_kd490 = left_join(df, sst_chla_wind_kd490)

df = df %>% 
  select(region, island, year, month, day, lon, lat, depth, method, species, density, presence, names(df)[15:355])

#compare sst observations
plot(df$mean_Sea_Surface_Temperature_CRW_daily_01dy, df$mean_Sea_Surface_Temperature_jplMUR_daily_01dy)

plot(df$mean_Kd490_ESA_OC_CCI_monthly_01yr, df$mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01yr)

ggplot(data = bathymetry_rugosity, aes(x = lon, y = lat, fill = Bathymetry_CRM_vol10_3s_all_units_rugosity.nc)) +
  geom_point(shape = 21, alpha = 0.7) + 
  facet_wrap(~island, scales = "free")

#fish data, where taape and toau are across the archipelago, lat/long distance
ggplot(data = df1, aes(x = longitude, y = latitude, fill = density)) +
  geom_point(shape = 21, alpha = 0.7) + 
  facet_wrap(~island, scales = "free") #LUKA

ggplot(data = df2, aes(x = longitude, y = latitude, fill = density)) +
  geom_point(shape = 21, alpha = 0.7) + 
  facet_wrap(~island, scales = "free") #LUFU

#df1= LUKA, df2=LUFU
table(df1$species)
table(df2$species)

#grab random 1000 rows and all columns and save as a df subset to work with merge function
df_subset <- df[sample(1:57102, 1000, replace=F), ]

#correlation matrix of predictors
library(GGally)
subset_sst.chla.kd490.wind <- df[, c("mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly_01yr", "mean_Kd490_ESA_OC_CCI_monthly_01yr",
                            "mean_Sea_Surface_Temperature_CRW_daily_01yr", "mean_Wind_Speed_ASCAT_daily_01yr")]
ggpairs(subset_sst.chla.kd490.wind )

subset_rugosity.bathymetry <- bathymetry_rugosity[, c("Bathymetry_HMRG_MHI_50m", "Bathymetry_HURL_NWHI_60m", 
                                     "Bathymetry_HMRG_MHI_50m_all_units_rugosity.nc","Bathymetry_HURL_NWHI_60m_all_units_rugosity.nc"  )]
ggpairs(subset_rugosity.bathymetry)

#urban runoff (nearshore sediment), chla and effluent 
subset_sediment.chla.effluent <- spc[, c("mean_Chlorophyll_A_ESA_OC_CCI_v6.0_monthly", "hi_otp_all_osds_effluent.tif", 
                                      "hi_otp_all_nearshore_sediment.tif")]
ggpairs(subset_sediment.chla.effluent)

subset_effluent_chla <- 

#env variables in the archipelago
ggplot(data = bathymetry_rugosity, aes(x = lon, y = lat, fill = Bathymetry_CRM_vol10_3s)) +
  geom_point(shape = 21, alpha = 0.7) + 
  facet_wrap(~island, scales = "free")

#summary statistics by islands

#subset for presence only
average_dense = df %>% subset(presence == 1) %>% group_by(species, island) %>% 
  summarise(average = mean(density), median = median(density), maximum = max(density))
ggplot(average_dense) +
  geom_point(aes(x = island, y = average, color = species))

ggplot(average_dense) +
  geom_point(aes(x = island, y = median, color = species))

ggplot(average_dense) +
  geom_point(aes(x = island, y = maximum, color = species))
islands.vec = unique(df$island)
for (i in 1:length(islands.vec)){
  sub = df %>% subset(presence == 1 & island == islands.vec[i]) 
  p <- ggplot(sub)+
    geom_histogram(aes(x = density, fill = species)) + ggtitle(islands.vec[i])
  print(p)
}

#reduced sst, chla, wind kd490: mean daily values
columns_to_keep <- c(1:5, 6, 61, 116, 193, 270)
reduced_sst_chla_wind_kd490 <- sst_chla_wind_kd490 %>%
  select(all_of(columns_to_keep))

df_reduced_sst_chla_wind_kd490 <- merge(df, reduced_sst_chla_wind_kd490, by = c("lat", "lon"))

plot(sst_chla_wind_kd490$mean_Sea_Surface_Temperature_CRW_daily_01dy, sst_chla_wind_kd490$mean_Sea_Surface_Temperature_jplMUR_daily_01dy)
summary(sst_chla_wind_kd490$mean_Sea_Surface_Temperature_CRW_daily_01dy)
summary(sst_chla_wind_kd490$mean_Sea_Surface_Temperature_jplMUR_daily_01dy)


