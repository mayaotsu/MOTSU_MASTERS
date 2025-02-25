## DATA CLEANING FOR NEW 2024 NCRMMP

rm(list = ls())

library(dplyr)
library(lubridate)

load("/Users/mayaotsu/Downloads/ALL_REA_FISH_RAW.rdata")
select=dplyr::select

colnames(df)

species <- c("LUKA", "CEAR", "LUFU")[3]
nSPC <- c("nSPC")

#filter out for only LUKA/LUFU/CEAR, islands, and spc in method---- 4310 obs
df_new <- df %>%
  filter(REGION %in% c("MHI", "NWHI") & METHOD %in% "nSPC") %>%
  select("ISLAND", "LATITUDE", "LONGITUDE", "DATE_", "METHOD", "SPECIES", "COUNT", "REGION",
         "DEPTH", "DENSITY", "OBS_YEAR") %>% 
  mutate(x = ifelse(SPECIES %in% species, DENSITY, 0)) # if species == LUKA, populate x with density; if not, populate x with 0

df_new = df_new %>% 
  group_by(LONGITUDE, LATITUDE, DATE_, OBS_YEAR, SPECIES, ISLAND, REGION) %>% 
  summarise(DENSITY=sum(x, na.rm = TRUE)) %>% 
  mutate(pa = ifelse(DENSITY > 0, 1, 0)) 

#df_new %>% 
#  filter(ISLAND == "Niihau") %>%
#  filter(OBS_YEAR==2021) %>% 
  #ggplot(aes(x=LONGITUDE, y=LATITUDE, fill= as.factor(pa))) + 
  #geom_point(shape=21) + 
  #facet_wrap(~OBS_YEAR, scales = "free")

dfluka_2024 <- subset(df_new, OBS_YEAR == 2024)
#dfcear_2024 <- subset(df_new, OBS_YEAR == 2024)
dflufu_2024 <- subset(df_new, OBS_YEAR == 2024)

#rename columns 
df <- df %>%
  rename(
    island = ISLAND, lat = LATITUDE, lon = LONGITUDE, date = DATE_, method = METHOD, species = SPECIES, 
    count = COUNT, region = REGION, depth = DEPTH, density = DENSITY
  )

#transform lon
df$lon = ifelse(df$lon < 0, df$lon + 360, df$lon)
#lat and lon decimal places
df <- df %>%
  mutate(lon = round(lon, 3),
         lat = round(lat, 3))

#add year, month,day columns
df <- df %>%
  mutate(
    year = year(date),
    month = month(date),
    day = day(date)
  )

#add presence column ASSIGN PRESENCE VALUES 0??
df$density[is.na(df$density)] <- 0
df$presence <- ifelse(df$density > 0, 1, 0)

#load spc_reduced
load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc.RData")
#combine df and spc_reduced--- 14502 obs, 47 vars
df_combined <- left_join(df, spc_reduced, by = c("lat", "lon"))

#delete duplicates ---- 10996 obs, 47 vars
df_combined <- df_combined %>%
  distinct()

rm(df, spc_reduced)
#save(df_combined, file ="/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc2024.RData")
