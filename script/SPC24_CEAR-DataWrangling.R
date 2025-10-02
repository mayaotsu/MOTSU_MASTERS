## DATA CLEANING FOR NEW ROI 2024 NCRMMP

rm(list = ls())
library(dplyr)
library(lubridate)
select=dplyr::select

load("/Users/mayaotsu/Downloads/ALL_REA_FISH_RAW.rdata")

#rename columns 
df <- df %>%
  rename(
    island = "ISLAND",
    depth = "DEPTH", 
    method = "METHOD", 
    date_ = "DATE_", 
    latitude = "LATITUDE", 
    longitude = "LONGITUDE", 
    species= "SPECIES", 
    density = "DENSITY", 
    count = "COUNT", 
    region = "REGION")

#transform lon
df$longitude = ifelse(df$longitude < 0, df$longitude + 360, df$longitude)

#lat and lon decimal places
df <- df %>%
  mutate(longitude = round(longitude, 4),
         latitude = round(latitude, 5))

#add year, month,day columns
df <- df %>%
  mutate(
    date_ = as.Date(date_),
    year = year(date_),
    month = month(date_),
    day = day(date_)
  )

#create a new column called x (if species is CEAR, then x will be equal to value density, if not then 0)
df$x = ifelse(df$species == "CEAR", df$density, 0)

df = df %>%
  #mutate(x = ifelse(species == "CEAR", density, 0)) %>% 
  group_by(island, depth, method, date_, latitude, longitude, region, year, month, day) %>%  # Grouping by year before summarizing
  summarise(density = sum(x, na.rm = TRUE)) #%>% 
 # mutate(presence = ifelse(x > 0, 1, 0))

#create new column, if species is CEAR, x will take value from density column, if species is not CEAR, x set to 0
df$presence = ifelse(df$density>0,1,0)
df$species <- "CEAR"
#df <- df %>% filter(species == "CEAR")
boxplot(df$presence[df$species=='CEAR' & (df$year>2009)] ~ df$year[df$species=='CEAR'& (df$year>2009)])

#filter out for only CEAR, islands, and spc in method
df <- df %>%
  subset(region %in% c("MHI", "NWHI") & method == "nSPC" & species == "CEAR") %>%
  select("island", "depth", "method", "date_", "latitude", 
         "longitude", "species", "density", "presence", "region",
         "year", "month", "day") 

df <- df %>% filter(year >= 2009 & year <= 2019)

#save(df, file ="/Users/mayaotsu/Downloads/calibr_CEAR_abund.RData")
saveRDS(df, file = "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_edited_CEAR")
save(df, file ="/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/SPC25_CEAR.RData")

######################################
############### END ###################
######################################
