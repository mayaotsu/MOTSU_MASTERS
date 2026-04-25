rm(list = ls()) 
library(ncdf4)
library(raster)
library(terra)

df = rast('~/Downloads/cmems_mod_glo_phy_my_0.083deg-climatology_P1M-m_1776812554177.nc')

terra::plot(df)

plot(df, zlim=c(18,29))
#sets the grid
par(mfrow= c(1,2))
plot(df[[3]], main = "Winter SST Climatology \n1993-2016")
plot(df[[9]], main = "Summer SST Climatology \n1993-2016")


#zoomed in
png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/figures/heat_map.png", width = 800, height = 400) # Open device
par(mfrow= c(1,2))
plot(df[[3]], xlim=c(187, 206), ylim=c(15, 27), zlim=c(18,29),
     col=(terrain.colors(100)), 
     main = "Winter SST Climatology \n1993-2016")
plot(df[[9]], xlim=c(187, 206),zlim=c(18,29), 
     col=(terrain.colors(100)), 
     main = "Summer SST Climatology \n1993-2016")
dev.off()  

#use terra
#hold color ramp constant and label island
