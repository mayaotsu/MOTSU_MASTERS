#download ROMS files from Erddap
rm(list =ls())
library(rerddap)
getwd()
setwd("/Users/mayaotsu/Downloads/OCN683_FisheriesOceanography")

base_dir <- "/Users/mayaotsu/Downloads/OCN683_FisheriesOceanography/ROMS"
dir.create(base_dir, showWarnings = FALSE)

Years=c(2009:2020)
depth_range <- c("0", "50")
depth_label <- "0_50m"

# Reduced bounding box
lon_min <- -161.1
lon_max <- -154.7
lat_min <- 18.8
lat_max <- 22.45

# Depths=list(c("0.50", "0.50"))
# Depth_Labels=c("0.50m")

# for (i in 1:length(Years)){
#   Direct=dir.create(paste0("/Users/mayaotsu/Downloads/OCN683_FisheriesOceanography",as.character(Years[i])))
#   setwd(paste0("/Users/mayaotsu/Downloads/OCN683_FisheriesOceanography/ROMS",as.character(Years[i])))
#   for (j in 1:length(Depth_Labels)){
# Direct_Depth=dir.create(Depth_Labels[j])

# for (i in seq_along(Years)) {
#   # Directory for each year
#   year_dir <- paste0(getwd(), "/", Years[i])
#   dir.create(year_dir, showWarnings = FALSE)
#   setwd(year_dir)
#   # Directory for depth
#   dir.create(depth_label, showWarnings = FALSE)
#   message("📥 Downloading ROMS for Year: ", Years[i],
#           " | Depth range: 0–50 m")
# 
# IP<-griddap(
#   datasetx = "roms_hiig_reanalysis",
#   url = "https://pae-paha.pacioos.hawaii.edu/erddap/",
#   fields = c("u","v"),
#   #depth=Depths[[j]],
#   depth = depth_range,
#   time=c(
#     paste0(Years[i],"-04-01T00:00:00Z"),
#     paste0(Years[i],"-12-05T00:00:00Z")),
#   longitude = c(lon_min, lon_max),
#   latitude = c(lat_min, lat_max),
#   fmt = "nc",
#   store = disk(path = paste0(year_dir, "/", depth_label))
# )
# 
# rm(IP)
# gc()
# }

# Loop over years and months
for (year in Years) {
  
  # Year folder
  year_dir <- file.path(base_dir, as.character(year))
  dir.create(year_dir, showWarnings = FALSE)
  
  # Depth folder inside year
  store_dir <- file.path(year_dir, depth_label)
  dir.create(store_dir, showWarnings = FALSE)
  
  for (month in 4:12) {   # April to December
    
    # Start date
    start_date <- as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))
    
    # Last day of month (base R trick)
    end_date <- as.Date(cut(start_date + 31, "month")) - 1
    
    start_date_str <- paste0(start_date, "T00:00:00Z")
    end_date_str   <- paste0(end_date, "T23:59:59Z")
    
    message("📥 Downloading ROMS for Year: ", year,
            " | Month: ", month,
            " | Depth range: 0–50 m")
    
    # Download via griddap
    IP <- griddap(
      datasetx = "roms_hiig_reanalysis",
      url = "https://pae-paha.pacioos.hawaii.edu/erddap/",
      fields = c("u", "v"),
      depth = depth_range,
      time = c(start_date_str, end_date_str),
      longitude = c(lon_min, lon_max),
      latitude = c(lat_min, lat_max),
      fmt = "nc",
      store = disk(path = store_dir)
    )
    
    # Clean up
    rm(IP)
    gc()
  }
}

      






# Example reduced bbox for MHI
lon_min <- -161.1
lon_max <- -154.7
lat_min <- 18.8
lat_max <- 22.45

# Make sure settled particles are in WGS84
settled_deg <- st_transform(settled %>% filter(settled == TRUE), 4326)

# Make sure bbox is in WGS84
st_crs(bbox_sf) <- 4326
st_crs(settled_deg)
st_crs(bbox_sf)

inside_bbox <- st_within(settled_deg, bbox_sf, sparse = FALSE)[,1]
table(inside_bbox)

pct <- mean(inside_bbox) * 100
cat(sprintf("Settled particles within 0.5 km inside bbox: %d / %d (%.2f%%)\n",
            sum(inside_bbox), nrow(settled_deg), pct))

library(ggplot2)
library(sf)

# Transform reef buffer to WGS84 if needed
reef_buffer_deg <- st_transform(reef_buffer, 4326)

library(ggplot2)
library(sf)

# Transform reef buffer to WGS84 if needed
reef_buffer_deg <- st_transform(reef_buffer, 4326)

ggplot() +
  # Bounding box
  geom_sf(data = bbox_sf, fill = NA, color = "black", size = 1) +
  
  # Reef buffer
  geom_sf(data = reef_buffer_deg, fill = NA, color = "blue", size = 0.5) +
  
  # Settled particles within 0.5 km
  geom_sf(data = settled_deg, color = "red", alpha = 0.6, size = 0.5) +
  
  theme_minimal() +
  ggtitle("Settled particles within 0.5 km of reefs (red) and ROMS bbox (black)") +
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max))



library(rerddap)

# Your new small-area bounding box
lon_min <- -160.8
lon_max <- -154.52
lat_min <- 18.49
lat_max <- 22.51

depth_range <- c("0", "50")
depth_label <- "0_50m"

# where to save
outdir <- "/Users/mayaotsu/Downloads/OCN683_FisheriesOceanography/2020/0_50m"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

install.packages("ncdf4")



