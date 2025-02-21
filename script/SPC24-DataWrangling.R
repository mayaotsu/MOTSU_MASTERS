## DATA CLEANING FOR NEW 2024 NCRMMP
rm(list = ls())
library(dplyr)
library(lubridate)
load("/Users/mayaotsu/Downloads/ALL_REA_FISH_RAW.rdata")

colnames(df)
sum(df$SPECIES == "LUKA", na.rm = TRUE) #4216, 2536
sum(df$SPECIES == "LUFU", na.rm = TRUE) #1956, 974
sum(df$SPECIES == "CEAR", na.rm = TRUE) #12056, 2340

species <- c("Lutjanus kasmira", "Cephalopholis argus", "Lutjanus fulvus")
islands <- c("Hawaii", "Kahoolawe", "Kauai", "Lanai", "Maui", "Molokai", "Niihau", "Oahu",
             "French Frigate", "Gardner", "Kure", "Laysan", "Lisianski", "Maro", "Midway", "Necker", "Nihoa",
             "Pearl & Hermes", "Molokini", "Lehua", "Kaula")
nSPC <- c("nSPC")

#filter out for only LUKA/LUFU/CEAR, islands, and spc in method
df <- df %>%
  filter(SCIENTIFIC_NAME %in% species & ISLAND %in% islands & METHOD %in% nSPC) %>%
  select("ISLAND", "LATITUDE", "LONGITUDE", "DATE_", "METHOD", "SPECIES", "COUNT", "REGION",
         "DEPTH", "DENSITY")

#rename columns
df <- df %>%
  rename(
    island = ISLAND, lat = LATITUDE, lon = LONGITUDE, date = DATE_, method = METHOD, species = SPECIES, 
    count = COUNT, region = REGION, depth = DEPTH, density = DENSITY
  )

#transform lon
df$lon = ifelse(df$lon < 0, df$lon + 360, df$lon)

#add year, month,day columns
df <- df %>%
  mutate(
    year = year(date),
    month = month(date),
    day = day(date)
  )

#add presence column ASSIGN PRESENCE VALUES 0??
df <- df %>%
  mutate(presence = ifelse(density > 0, 1, 0))
