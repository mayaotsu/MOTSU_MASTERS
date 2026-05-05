rm(list = ls()) 
library(ncdf4)
library(raster)
library(terra)

df = rast('~/Downloads/cmems_mod_glo_phy_my_0.083deg-climatology_P1M-m_1776812554177.nc')

terra::plot(df)
#raster::image(df)
plot(df, zlim=c(18,29))
#sets the grid
par(mfrow= c(1,2))
plot(df[[3]], main = "Winter SST Climatology \n1993-2016")
plot(df[[9]], main = "Summer SST Climatology \n1993-2016")


#zoomed in
png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/figures/heat_map_2.png", width = 800, height = 400) # Open device
zlim_all <- c(17, 28)
par(mfrow= c(1,2))
breaks <- seq(zlim_all[1], zlim_all[2], length.out = 101)
cols <- terrain.colors(100)

plot(df[[3]], 
     xlim=c(187, 206), 
     ylim=c(17, 31), 
     zlim = zlim_all,
     breaks=seq(17, 28, length.out=101),
     main = "Winter SST Climatology \n1993-2016")


plot(df[[9]], 
     xlim=c(187, 206),
     ylim = c(17, 31),
     zlim=zlim_all, 
     breaks=seq(17, 28, length.out=101),
     main = "Summer SST Climatology \n1993-2016")
dev.off()  

#use terra
#hold color ramp constant and label island

library(fields)

png("/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/figures/heat_map_2.png",
    width = 1000, height = 400)

zlim_all <- c(17, 28)
cols <- terrain.colors(100)

# layout: plot + legend | plot + legend
layout(matrix(c(1,2,3,4), nrow = 1), widths = c(1, 0.25, 1, 0.25))

# --- Winter ---
plot(df[[3]], 
     xlim = c(187, 206), 
     ylim = c(17, 31), 
     zlim = zlim_all,
     col = cols,
     legend = FALSE,
     main = "Winter SST Climatology \n1993–2016")

image.plot(
  legend.only = TRUE,
  zlim = zlim_all,
  col = cols,
  legend.lab = "°C",
  axis.args = list(at = seq(17, 28, by = 2))
)

# --- Summer ---
plot(df[[9]], 
     xlim = c(187, 206),
     ylim = c(17, 31),
     zlim = zlim_all, 
     col = cols,
     legend = FALSE,
     main = "Summer SST Climatology \n1993–2016")

image.plot(
  legend.only = TRUE,
  zlim = zlim_all,
  col = cols,
  legend.lab = "°C",
  axis.args = list(at = seq(17, 28, by = 2))
)

dev.off()