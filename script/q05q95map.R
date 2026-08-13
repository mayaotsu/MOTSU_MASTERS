rm(list = ls())
library(ggplot2)
library(rnaturalearth)
library(sf)
library(ggplot2)
library(dplyr)

load("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/data/spc_reduced_final_CEAR.RData")
spc_reduced = spc_final

p <- ggplot(spc_reduced, aes(x = lon, y = lat)) +
  
  # if you have a basemap (e.g. hawaii_map from earlier), add it first so points sit on top:
  # geom_polygon(data = hawaii_map, aes(long, lat, group = group),
  #              fill = "grey95", color = "grey60", linewidth = 0.3,
  #              inherit.aes = FALSE) +
  
  geom_point(aes(color = q05_1yr_sst_jpl),
             size = 2,
             alpha = 0.85,
             shape = 16) +
  
  scale_color_viridis_c(
    name = "5th %ile SST (°C)",
    option = "C",
    guide = guide_colorbar(
      barwidth = 1,
      barheight = 8,
      title.position = "top"
    )
  ) +
  
  coord_fixed() +
  
  labs(
    title = "5th percentile of 1-year SST",
    x = "Longitude (°W)",
    y = "Latitude (°N)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle = element_text(size = 11, color = "grey40", hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

p

ggsave("sst_map.png", plot = p, width = 8, height = 6, dpi = 500, bg = "white")


library(ggplot2)
library(patchwork)

# Shared color scale range so both panels are comparable
sst_q05 <- range(spc_reduced$q05_1yr_sst_jpl, na.rm = TRUE)
sst_q95 <- range(spc_reduced$q95_1yr_sst_jpl, na.rm = TRUE)
  
base_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )

p1 <- ggplot(spc_reduced, aes(x = lon, y = lat)) + 
  geom_point(aes(color = q05_1yr_sst_jpl), size = 2, alpha = 0.85, shape = 16) +
  scale_color_viridis_c(name = "SST (°C)", option = "C", limits = sst_q05) +
  coord_fixed() +
  labs(title = "5th percentile SST", x = "Longitude (°W)", y = "Latitude (°N)") +
  base_theme

p2 <- ggplot(spc_reduced, aes(x = lon, y = lat)) +
  geom_point(aes(color = q95_1yr_sst_jpl), size = 2, alpha = 0.85, shape = 16) +
  scale_color_viridis_c(name = "SST (°C)", option = "C", limits = sst_q95) +
  coord_fixed() +
  labs(title = "Sea Surface Temperature (Q95)", x = "Longitude (°W)", y = "Latitude (°N)") +
  base_theme

# combine, share one legend on the right
p_combined <- p1 + p2 +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "right")
p1
p2
p_combined

ggsave("sst_q05_q95_panels.png", plot = p_combined, width = 11, height = 7, dpi = 500, bg = "white")
