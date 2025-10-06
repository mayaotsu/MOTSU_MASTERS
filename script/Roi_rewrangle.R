rm(list =ls())
library(dplyr)
library(lubridate)
select=dplyr::select

#load taape, toau, roi df
load('/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full_07.21.RData'); df1 <- df

#1 remove cear
spc_reduced <- spc_reduced %>%
  filter(species %in% c("LUKA", "LUFU")) %>%
  ungroup() %>%
  mutate(
    lat   = round(lat, 5),
    lon   = round(lon, 4),
    date_ = as.Date(date_),
    year  = as.numeric(year),
    month = as.numeric(month),
    day   = as.numeric(day),
    site_id = paste(island, depth, method, date_, lat, lon, region, year, month, day, sep="_")
  )

#2 select variables to keep
unique <- spc_reduced %>%
  select(site_id, island, depth, method, date_, lat, lon, region, 
         rugosity, mean_1mo_chla_ESA, q05_1yr_sst_jpl, q95_1yr_sst_jpl,
         otp_nearshore_sediment, coral_cover, otp_all_effluent, MHI_spear) %>%
  distinct()

#3 Load raw dataset and extract CEAR data
load("/Users/mayaotsu/Downloads/ALL_REA_FISH_RAW.rdata")
raw <- df; rm(df)

raw <- raw %>%
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
  filter(
    species == "CEAR",
    region %in% c("MHI", "NWHI"),
    method == "nSPC"
  ) %>%
  mutate(
    lat = round(latitude, 5),
    lon = round(ifelse(longitude < 0, longitude + 360, longitude), 4),
    date_ = as.Date(date_),
    depth = as.numeric(depth),
    year  = as.numeric(year(date_)),
    month = as.numeric(month(date_)),
    day   = as.numeric(day(date_)),
    presence = ifelse(density > 0, 1, 0),
    site_id = paste(island, depth, method, date_, lat, lon, region, sep="_")
  ) %>%
  select(site_id, density, presence)

#avoid dupliactes
raw_cear_unique <- raw %>%
  group_by(site_id) %>%
  summarise(
    density  = sum(density, na.rm = TRUE),
    presence = ifelse(sum(presence, na.rm = TRUE) > 0, 1, 0),
    .groups = "drop"
  )

# 4 left join cear presence and density to luka/lufu df
cear_full <- unique %>%
  left_join(raw_cear_unique, by = "site_id") %>%
  mutate(
    species  = "CEAR",
    presence = ifelse(is.na(presence), 0, presence),
    density  = ifelse(is.na(density), 0, density)
  )

#5 join cear to lufu/luka df
spc_reduced_final <- bind_rows(spc_reduced, cear_full)

table(spc_reduced_final$species)

saveRDS(spc_reduced_final, 
        file = "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_reduced_final_CEAR.RDS")
save(spc_reduced_final, 
     file = "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_reduced_final_CEAR.RData")


