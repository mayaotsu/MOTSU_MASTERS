## DATA CLEANING FOR NEW 2024 NCRMMP
rm(list = ls())
load("/Users/mayaotsu/Downloads/ALL_REA_FISH_RAW.rdata")

colnames(df)
sum(df$SPECIES == "LUKA", na.rm = TRUE) #4216
sum(df$SPECIES == "LUFU", na.rm = TRUE) #1956
sum(df$SPECIES == "CEAR", na.rm = TRUE) #12056

species <- c("Lutjanus kasmira", "Cephalopholis argus", "Lutjanus fulvus")
islands <- c("Hawaii", "Kahoolawe", "Kauai", "Lanai", "Maui", "Molokai", "Niihau", "Oahu",
             "French Frigate", "Gardner", "Kure", "Laysan", "Lisianski", "Maro", "Midway", "Necker", "Nihoa",
             "Pearl & Hermes", "Molokini", "Lehua", "Kaula")
df_filtered <- df %>%
  filter(SCIENTIFIC_NAME %in% species & ISLAND %in% islands) %>%
  select("ISLAND", "LATITUDE", "LONGITUDE", "DATE_", "METHOD", "SPECIES", "COUNT", "REGION",
         "DEPTH", "DENSITY")


sum(df_filtered$SPECIES == "LUKA", na.rm = TRUE) #4216
sum(df_filtered$SPECIES == "LUFU", na.rm = TRUE) #1956
sum(df_filtered$SPECIES == "CEAR", na.rm = TRUE) #12056
colnames(df_filtered)

df_filtered <- df[, c("ISLAND", "LATITUDE", "LONGITUDE", "DATE_", "METHOD", "SPECIES", "COUNT", "REGION",
                      "DEPTH", "DENSITY")]
df_filtered