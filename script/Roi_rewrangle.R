rm(list = ls())
library(dplyr)
library(lubridate)
select=dplyr::select

load('/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full_07.21.RData'); df1 <- df

#load in spc full and then load in the roi full raw dataset
#left join the raw 
# figure oout where presences are, when theres an NA it becomes a 0


# 1️⃣ Remove old CEAR
spc_reduced <- spc_reduced %>%
  filter(species %in% c("LUKA", "LUFU"))

# 2️⃣ Load raw dataset and extract CEAR data
load("/Users/mayaotsu/Downloads/ALL_REA_FISH_RAW.rdata")

df_cear <- df %>%
  rename(
    island    = ISLAND,
    depth     = DEPTH,
    method    = METHOD,
    date_     = DATE_,
    latitude  = LATITUDE,
    longitude = LONGITUDE,
    species   = SPECIES,
    density   = DENSITY,
    region    = REGION
  ) %>%
  filter(species == "CEAR",
         region %in% c("MHI", "NWHI"),
         method == "nSPC") %>%
  mutate(
    longitude = ifelse(longitude < 0, longitude + 360, longitude),
    longitude = round(longitude, 4),
    latitude  = round(latitude, 5),
    date_ = as.Date(date_),
    year  = year(date_),
    month = month(date_),
    day   = day(date_),
    presence = ifelse(density > 0, 1, 0)
  ) %>%
  select(island, depth, method, date_, latitude, longitude, species,
         density, presence, region, year, month, day)


