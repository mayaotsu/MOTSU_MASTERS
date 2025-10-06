rm(list =ls())
library(dplyr)
library(lubridate)
select=dplyr::select

#load taape (2276), toau (2276), roi (2557) df (dataset ive been using to run models)
load('/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full_07.21.RData'); df1 <- df

#1 remove wrong cear to start with just taape and toau to pair roi with later
spc_reduced$lon <- spc_reduced$lon - 360 # change 1: changing spc_reduce lon 
spc_reduced <- spc_reduced %>%
  filter(species %in% c("LUKA", "LUFU")) %>%
  ungroup() %>% 
  mutate(
    lat   = round(lat, 5),
    lon   = round(lon, 4),
    date_ = as.Date(date_),
    year  = as.numeric(as.character(year)), # change 2: make year character then numeric 
    month = as.numeric(month),
    day   = as.numeric(day),
    site_id = paste(island, depth, method, date_, lat, lon, region, year, month, day, sep="_") #unique id
  ) #%>%   
#select(site_id, density, presence)
#2 creating site grid for cear to line up with

# unique <- spc_reduced %>%
#   select(site_id, island, depth, method, date_, lat, lon, region, year, month, day) %>%
#   distinct()

grid <- spc_reduced %>%
  select(site_id, island, depth, method, date_, lat, lon, region,
         year, month, day,
         rugosity, mean_1mo_chla_ESA, q05_1yr_sst_jpl, q95_1yr_sst_jpl,
         otp_nearshore_sediment, coral_cover, otp_all_effluent, MHI_spear) %>%
  distinct()

#3 Load raw dataset and extract CEAR data, matching column names and lat/lon decimal places
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
    lon = round(longitude, 4),
    date_ = as.Date(date_),
    year  = year(date_),
    month = month(date_),
    day   = day(date_),
    presence = ifelse(density > 0, 1, 0),
    site_id = paste(island, depth, method, date_, lat, lon, region, year, month, day, sep="_")
  ) %>%
  select(site_id, density, presence)


#avoid dupliactes
raw_cear_unique <- raw %>%
  group_by(site_id) %>%
  summarise(
    density  = sum(density, na.rm = TRUE),
    presence = ifelse(sum(presence, na.rm = TRUE) > 0, 1, 0), #cear pres 1 if observed, if not 0
    .groups = "drop"
  )

# 4 left join cear presence and density to luka/lufu df
cear_full <- grid %>%
  left_join(raw_cear_unique, by = "site_id") %>%
  mutate(
    species  = "CEAR",
    density  = ifelse(is.na(density), 0, density),
    presence = ifelse(is.na(presence), 0, presence)
  )

#5 join cear to lufu/luka df
spc_reduced_final <- bind_rows(spc_reduced, cear_full)

table(spc_reduced_final$species)

# if we subset our original LUFU/LUKU dataset for one species at a time and subset the final dataset for
# the same species, they should be the exact same 
subset(spc_reduced, species == "LUFU") == subset(spc_reduced_final, species == "LUFU")
unique(subset(spc_reduced, species == "LUFU") == subset(spc_reduced_final, species == "LUFU"))
unique(subset(spc_reduced, species == "LUFU")$density == subset(spc_reduced_final, species == "LUFU")$density)
# which what we find for LUFU (if there was any issues, we would see T/F's)

subset(spc_reduced, species == "LUKA") == subset(spc_reduced_final, species == "LUKA")
unique(subset(spc_reduced, species == "LUKA") == subset(spc_reduced_final, species == "LUKA"))
unique(subset(spc_reduced, species == "LUKA")$density == subset(spc_reduced_final, species == "LUKA")$density)
# and LUKA

# and to show you what happens when they don't equal if you switch out the species
subset(spc_reduced, species == "LUFU") == subset(spc_reduced_final, species == "LUKA")
unique(subset(spc_reduced, species == "LUFU") == subset(spc_reduced_final, species == "LUKA"))

# and what if the rows were all mixed up
unique(subset(spc_reduced, species == "LUKA")[sample(nrow(subset(spc_reduced, species == "LUKA"))),] == subset(spc_reduced_final, species == "LUKA"))
# this just shows what would this output look like if there was disagreement 
saveRDS(spc_reduced_final, 
        file = "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_reduced_final_CEAR.RDS")
save(spc_reduced_final, 
     file = "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_reduced_final_CEAR.RData")


