library(dplyr)

rm(list=ls())


# load EDS data
load("/Users/mayaotsu/Downloads/eds_time.RData"); eds = df
glimpse(eds)


load("/Users/mayaotsu/Downloads/calibr_LUKA_abund.RData"); df1 = df
load("/Users/mayaotsu/Downloads/calibr_LUFU_abund.RData"); df2 = df


df = rbind(df1, df2) %>% 
  mutate(longitude = round(longitude, 3),
         latitude = round(latitude, 3)) %>%
  distinct() 


colnames(df)[5:6] = c("lat", "lon")
df$date <- paste(df$year, df$month, df$day, sep = "-")


df = left_join(df, eds)


df = df %>% 
  select(region, island, year, month, day, lon, lat, depth, method, species, density, presence, names(df)[15:355])


plot(df$mean_Sea_Surface_Temperature_CRW_daily_01dy, df$mean_Sea_Surface_Temperature_jplMUR_daily_01dy)
