######################################################################
### Scripts to attach gam-interpolated coral cover to in situ data ###
######################################################################

# Clear the R environment by removing all objects
rm(list = ls())

# Set the working directory
dir = getwd("/Users/mayaotsu/Documents/MOTSU_MASTERS/coral_cover")

# Import custom functions from EDS (Environmental Data Summary)
source("eds_functions.R")
source("ExpandingExtract.R")

# Import survey data for two species (LUFU and LUKA)
load("/Users/mayaotsu/Downloads/calibr_LUKA_abund.RData"); df1 = df
load("/Users/mayaotsu/Downloads/calibr_LUFU_abund.RData"); df2 = df
# Combine the two datasets and keep only distinct records based on specified columns
df = rbind(df1, df2) %>% 
  distinct(region, island, longitude, latitude, year, month, day)

# Standardize column names
colnames(df) = c("region", "island", "lon", "lat", "year", "month", "day")

# Display a table of the number of records per region
table(df$region)

# Convert negative longitude values (Western Hemisphere) to positive by adding 360
df$lon = ifelse(df$lon < 0, df$lon + 360, df$lon)

# Remove rows with missing latitude or longitude values and convert to a spatial object
df = df[complete.cases(df[,c("lon", "lat")]), ]

# Replace spaces in island names with underscores
df$island = gsub(" ", "_", df$island)

# Define the path where the gridded coral cover files are located
EDS_path = paste0("/Users/", Sys.info()[7], "/Desktop/Environmental_Data_Summary/coral_cover")

# Create a list of climatology raster files (gridded coral cover files)
rasterlist = list.files(EDS_path,
                        recursive = T, 
                        pattern = "gridded", 
                        full.names = T)

# Alias the raster::shift function to avoid conflicts
shift = raster::shift

# Start timing the extraction process
start = Sys.time()

# Initialize an empty data frame to store results
spc = NULL

for (raster_i in 1:length(rasterlist)){
  
  # raster_i = 2
  
  rasname_full = rasterlist[raster_i] 
  rasname_sp = strsplit(rasname_full, "/")[[1]][7]
  rasname_sp = gsub(rasname_sp, pattern = "gridded_coral_cover_", replacement = "")
  rasname_sp <- sub("\\.nc$", "", rasname_sp)  
  
  df_sp = df %>% filter(island == rasname_sp)
  df_sp = as.data.frame(df_sp)
  coordinates(df_sp) = ~lon + lat
  
  this_r = raster(rasterlist[raster_i])
  
  if (this_r@extent@xmin < 0) this_r = shift(rotate(shift(this_r, 180)), 180)
  
  crs(df_sp) = crs(this_r) 
  
  this_Ex = ExpandingExtract(this_r, df_sp)
  
  eval(parse(text = paste0("df_sp$", "coral_cover", " = this_Ex$values")))
  
  dfi = as.data.frame(df_sp)
  
  spc = rbind(spc, dfi)
  
  cat(paste0("Step ", raster_i, " of ", length(rasterlist), ": Extraction Complete.\n"))
  
}

# Stop timing the extraction process
# Calculate and print the total time taken for the extraction process
stop = Sys.time()
stop - start
beepr::beep(2)

# Write the final data frame with the extracted coral cover data to a CSV file
#spc$lon = ifelse(spc$lon > 180, spc$lon - 360, spc$lon)
write_csv(spc, "/Users/mayaotsu/Documents/MOTSU_MASTERS/coral_cover/spc_coral_cover.csv")

# Visualize the extracted coral cover data
spc %>% 
  filter(island == "Kure") %>% 
  ggplot(aes(lon, lat, fill = coral_cover)) + 
  geom_point(shape = 21, size = 5, alpha = 0.5) + 
  coord_fixed() + 
  scale_fill_viridis_c("Coral\nCover\n(%)")
