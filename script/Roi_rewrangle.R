rm(list =ls())
library(dplyr)
library(lubridate)
select=dplyr::select

#load taape (2276), toau (2276), roi (2557) df (dataset ive been using to run models)
load('/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_full_07.21.RData'); df1 <- df

# 1. remove cear to start with just taape and toau to pair cear later
spc_reduced <- spc_reduced %>%
  ungroup() %>% 
  filter(species %in% c("LUKA", "LUFU")) %>%
  mutate(
    lat   = round(lat, 5),
    lon   = round(lon, 4),
    date_ = as.Date(date_),
    year  = as.factor(as.character(year)), # change 2: make year character then numeric 
    month = as.numeric(month),
    day   = as.numeric(day),
    site_id = paste(island, depth, method, date_, lat, lon, region, year, month, day, sep="_") #unique id
  ) 

# 2. create site grid for env variables
grid <- spc_reduced %>%
  select(site_id, island, depth, method, date_, lat, lon, region,
         year, month, day,
         rugosity, mean_1mo_chla_ESA, q05_1yr_sst_jpl, q95_1yr_sst_jpl,
         otp_nearshore_sediment, coral_cover, otp_all_effluent, MHI_spear) %>%
  distinct()

# 3 load in CEAR,
load('/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/cear_calibr.RData')
raw <- df_spc; rm(df_spc)

#get rid of duplicate depths
raw <- raw %>%
  group_by(island, latitude, longitude, method, date_, region) %>%
  summarise(
    depth = mean(depth, na.rm = TRUE),
    density = mean(density, na.rm = TRUE),
    presence = max(presence, na.rm = TRUE),
    .groups = "drop"
  )


# get rid of duplicate observations, average depth, lat lon to 0-360
raw <- raw %>%
  rename(lat = latitude, lon = longitude) %>%
  mutate(
    lon   = ifelse(lon < 0, lon + 360, lon),
    lat   = round(lat, 5),
    lon   = round(lon, 4),
    date_ = as.Date(date_),
    year  = year(date_),
    month = month(date_),
    day   = day(date_),
    site_id = paste(island, depth, method, date_, lat, lon, region,
                    year, month, day, sep = "_")
  ) 

# 4 join cear data to grid and fill NAs
cear_joined <- grid %>%
  left_join(raw %>% select(site_id, presence, density), by = "site_id") %>%
  mutate(
    species  = "CEAR",
    density  = ifelse(is.na(density), 0, density),
    presence = ifelse(is.na(presence), 0, presence)
  )

#5 combine with luka/lufu
spc_final <- bind_rows(spc_reduced, cear_joined)

#sanity checks
table(spc_final$species)
table(spc_final$presence, spc_final$species)

#make sure lufu/luku presence and absence values are the same
table(spc_reduced$species, spc_reduced$presence)
table(spc_final$species, spc_final$presence)

#check env variable columns are the same
identical(colnames(spc_reduced), colnames(spc_final))

identical(spc_reduced$q05_1yr_sst_jpl[spc_reduced$species %in% c("LUKA", "LUFU")],
          spc_final$q05_1yr_sst_jpl[spc_final$species %in% c("LUKA", "LUFU")])

# if we subset our original LUFU/LUKU dataset for one species at a time and subset the final dataset for
# the same species, they should be the exact same 
subset(spc_reduced, species == "LUFU") == subset(spc_final, species == "LUFU")
unique(subset(spc_reduced, species == "LUFU") == subset(spc_final, species == "LUFU"))
unique(subset(spc_reduced, species == "LUFU")$density == subset(spc_final, species == "LUFU")$density)
# which what we find for LUFU (if there was any issues, we would see T/F's)

subset(spc_reduced, species == "LUKA") == subset(spc_final, species == "LUKA")
unique(subset(spc_reduced, species == "LUKA") == subset(spc_final, species == "LUKA"))
unique(subset(spc_reduced, species == "LUKA")$density == subset(spc_final, species == "LUKA")$density)
# and LUKA

# and to show you what happens when they don't equal if you switch out the species
subset(spc_reduced, species == "LUFU") == subset(spc_reduced_final, species == "LUKA")
unique(subset(spc_reduced, species == "LUFU") == subset(spc_reduced_final, species == "LUKA"))

# and what if the rows were all mixed up
unique(subset(spc_reduced, species == "LUKA")[sample(nrow(subset(spc_reduced, species == "LUKA"))),] == subset(spc_reduced_final, species == "LUKA"))
# this just shows what would this output look like if there was disagreement 

saveRDS(spc_final, 
        file = "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_reduced_final_CEAR.RDS")
save(spc_final, 
     file = "/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_reduced_final_CEAR.RData")

table(spc_reduced$species, spc_reduced$presence)
table(spc_final$species, spc_final$presence)

df_spc <- df_spc %>%
  group_by(island, latitude, longitude, method, date_, region) %>%
  summarise(
    depth = mean(depth, na.rm = TRUE),
    density = mean(density, na.rm = TRUE),
    presence = max(presence, na.rm = TRUE),
    .groups = "drop"
  )

