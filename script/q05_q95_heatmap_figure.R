rm(list = ls()) 
library(ncdf4)
library(raster)
df = stack('~/Downloads/cmems_mod_glo_phy_my_0.083deg-climatology_P1M-m_1776812554177.nc')

plot(df, zlim=c(18,29))
#sets the grid
par(mfrow= c(1,2))
plot(df[[1]], main = "January SST Climatology 1993-2016")
plot(df[[9]], main = "September SST Climatology 1993-2016")


#sets the grid
par(mfrow= c(1,2))
plot(df[[1]], zlim=c(18,29),
     col=(terrain.colors(100)), 
     main = "January SST Climatology 1993-2016")
plot(df[[9]], zlim=c(18,29), main = "September SST Climatology 1993-2016")
