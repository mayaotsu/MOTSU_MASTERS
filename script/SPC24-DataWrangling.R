## DATA CLEANING FOR NEW 2024 NCRMMP

rm(list = ls())
library(dplyr)
library(lubridate)

load("/Users/mayaotsu/Downloads/ALL_REA_FISH_RAW.rdata")
select=dplyr::select
colnames(df)

#species <- c("LUKA", "CEAR", "LUFU")[2]
#nSPC <- c("nSPC")

#rename columns 
df <- df %>%
  rename(
    island = "ISLAND", depth = "DEPTH", method = "METHOD", date_ = "DATE_", latitude = "LATITUDE", longitude = "LONGITUDE", 
    species= "SPECIES", density = "DENSITY", count = "COUNT", region = "REGION")

#transform lon
df$longitude = ifelse(df$longitude < 0, df$longitude + 360, df$longitude)
#lat and lon decimal places
df <- df %>%
  mutate(longitude = round(longitude, 4),
         latitude = round(latitude, 5))

#add year, month,day columns
df <- df %>%
  mutate(
    year = year(date_),
    month = month(date_),
    day = day(date_)
  )

#make a presence column
df = df %>%
  mutate(x = ifelse(species == "CEAR", density, 0)) %>% 
  group_by(island, depth, method, date_, latitude, longitude, species, region, year, month, day) %>%  # Grouping by year before summarizing
  summarise(density = sum(x, na.rm = TRUE)) %>% 
  mutate(presence = ifelse(density > 0, 1, 0))
#filter out for only CEAR, islands, and spc in method
df <- df %>%
  subset(region %in% c("MHI", "NWHI") & method == "nSPC" & species == "CEAR") %>%
  select("island", "depth", "method", "date_", "latitude", "longitude", "species", "density", "presence", "region",
         "year", "month", "day") 

#save .RData
save(df, file ="/Users/mayaotsu/Downloads/calibr_CEAR_abund.RData")




 # mutate(x = ifelse(species %in% species, density, 0))
# if species == CEAR, populate x with density; if not, populate x with 0


#df = df %>% 
  #group_by(longitude, latitude, date_, year, species, island, region) %>% 
 # summarise(density=sum(x, na.rm = TRUE)) %>% 
 # mutate(pa = ifelse(density > 0, 1, 0)) 

  
  
  
####
df %>% 
  filter(species == "CEAR") %>%
  filter(year==2016) %>% 
  ggplot(aes(x=lon, y=lat, fill= as.factor(pa), shape = as.factor(pa))) + 
  geom_point(shape=21) +
  facet_wrap(~island, scales = "free")

#dfcear_2024 <- subset(df_new, OBS_YEAR == 2024)

#load spc_reduced
load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc.RData")

#combine df and spc_reduced
df_combined <- df %>%
  left_join(spc_reduced, by = c("lat", "lon", "species", "year"))

#delete duplicates ---- 10996 obs, 47 vars
df_combined <- df_combined %>%
  distinct()

rm(df, spc_reduced)
#save(df_combined, file ="/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/data/spc2024.RData")
