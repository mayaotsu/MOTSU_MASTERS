library(raster)
library(sp)
library(lubridate)
library(ncdf4)
library(dplyr)
getwd()
# Environmental data
TKE <- readRDS("/Users/mayaotsu/Documents/MOTSU_MASTERS/tkebrick.rds")

# Longline dataset
load("/Users/mayaotsu/Downloads/calibr_LUKA_abund.RData"); df1 = df
load("/Users/mayaotsu/Downloads/calibr_LUFU_abund.RData"); df2 = df
df = rbind(df1, df2)
rm(df1, df2)
# Convert lon to 360
df$longwest <- (360-df$longitude)*-1
  
# Make new data column
df$TKE<- NA
library(lubridate)
df <- as.data.frame(df)
# Get all the environmental data for the time and place of fishing
for (i in 261:nlayers(TKE)) {  # strting at 3 because TKE stack starts in oct and there is no longline data until december 2004
  year=(as.numeric(substr(names(TKE[[i]]),7,10)))
  month=(as.numeric(substr(names(TKE[[i]]),12,13)))
  day=(as.numeric(substr(names(TKE[[i]]),15,16)))
  idx <- which(df$year == year & df$month == month & df$day==day)
  if(length(idx)>0){
    pts <- SpatialPoints(df[idx,c('longwest', 'latitude')], crs(TKE))
    
    df$TKE[idx] <- raster::extract(TKE[[i]], pts, method='bilinear')}
  print(i)
  }

saveRDS(df, "spcdata_tke.rds")
 hist(df$TKE)
na.count <- length(which(is.na(df$TKE)))
sum(na.count)
