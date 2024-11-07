#################################################################
### Scripts to attach climatologies variables to in situ data ###
### Revised & Maintained by K.R.Tanaka & T.A.Oliver.          ###
### POC: kisei.tanaka@noaa.gov, thomas.oliver@noaa.gov,       ###
### jessica.perelman@noaa.gov, juliette.verstaen@noaa.gov     ###
#################################################################

rm(list = ls())

dir = getwd()
getwd()

# Import EDS functions
source("scripts/eds_functions.R")
source("scripts/HelperCode/ExpandingExtract.R")

# Import survey data, SM = master REA survey file, subset if necessary 
load('data/SURVEY_MASTER.RData')
table(df$region)
df$lon = ifelse(df$lon < 0, df$lon + 360, df$lon)
# df = subset(df, region == "MARIAN")

# Remove NAs in lat & lon columns. Convert into a spatial object
df = df[complete.cases(df[,c("lon", "lat")]), ]
df_sp = df; df_sp = as.data.frame(df_sp)
coordinates(df_sp) = ~lon + lat

# Define path (e.g., M drive)
EDS_path = "/Users/mayaotsu/Desktop/Environmental_Data_Summary/"
# EDS_path = "/mnt/ldrive/ktanaka/Environmental_Data_Summary/"
# EDS_path = paste0("/Users/", Sys.info()[7], "/Desktop/Environmental_Data_Summary/")

# Create list of climatology raster files
rasterlist = list.files(EDS_path,
                        recursive = T, 
                        pattern = "_all_units", 
                        full.names = T)

# Read Parameter and Time Series Summary Definitions
# Extract unique datasets marked as "Climatology" from ParamDF
uP <- read_csv("data/EDS_parameters.csv") %>% 
  filter(Download == "YES") %>%
  filter(Frequency == "Climatology") %>% 
  pull(Dataset) %>% 
  unique()

# Filter rasterlist based on the uP keywords
rasterlist <- rasterlist[grep(paste(uP, collapse = "|"), rasterlist)]
rasterlist = rasterlist[grepl("Bathymetry", rasterlist)]

# Check rasterarized climatological variables
gsub("/Users/mayaotsu/Desktop/Environmental_Data_Summary/Data_Download/", "", rasterlist)

shift = raster::shift

###################
### normal loop ###
###################

start = Sys.time()

for (raster_i in 1:length(rasterlist)){
  
  # raster_i = 10
  
  rasname_full = rasterlist[raster_i] 
  rasname_sp = strsplit(rasname_full, "/")[[1]]
  rasname = rasname_sp[length(rasname_sp)]
  rasname = gsub(rasname, pattern = "-", replacement = ".")
  rasname = gsub(rasname, pattern = "_all_units.nc", replacement = "")
  
  this_r = raster(rasterlist[raster_i])
  
  if (grepl("Bathymetry", rasname) && !grepl("rugosity", rasname)) {
    this_r[this_r > 0] <- NA
  }
  
  if (this_r@extent@xmin < 0) this_r = shift(rotate(shift(this_r, 180)), 180)
  
  crs(df_sp) = crs(this_r) 
  
  cat(paste0("Step ", raster_i, " of ", length(rasterlist), ": ", rasname, "\n"))
  
  this_Ex = ExpandingExtract(this_r, df_sp, Dists = seq(0, 1000, 100))
  
  eval(parse(text = paste0("df_sp$", rasname, " = this_Ex$values")))
  
  cat(paste0("Step ", raster_i, " of ", length(rasterlist), ": Extraction Complete.\n"))
  
}

stop = Sys.time()
start - stop
beepr::beep(2)

df = as.data.frame(df_sp)

save(df, file = paste0("outputs/EDS_Climatologies_", Sys.Date(), ".RData"))
