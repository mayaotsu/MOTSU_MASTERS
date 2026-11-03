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
library(dplyr)

# Celsius -> Fahrenheit
c_to_f <- function(x) x * 9/5 + 32

spc_reduced <- spc_reduced %>%
  mutate(
    q05_1yr_sst_f = c_to_f(q05_1yr_sst_jpl),
    q95_1yr_sst_f = c_to_f(q95_1yr_sst_jpl)
  )

# One shared range across BOTH panels so colors are truly comparable
sst_lims <- range(c(spc_reduced$q05_1yr_sst_f, spc_reduced$q95_1yr_sst_f),
                  na.rm = TRUE)

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
  geom_point(aes(color = q05_1yr_sst_f), size = 2, alpha = 0.85, shape = 16) +
  scale_color_viridis_c(name = "SST (°F)", option = "C", limits = sst_lims) +
  coord_fixed() +
  labs(title = "Sea Surface Temperature (Q05)",
       x = "Longitude (°W)", y = "Latitude (°N)") +
  base_theme

p2 <- ggplot(spc_reduced, aes(x = lon, y = lat)) +
  geom_point(aes(color = q95_1yr_sst_f), size = 2, alpha = 0.85, shape = 16) +
  scale_color_viridis_c(name = "SST (°F)", option = "C", limits = sst_lims) +
  coord_fixed() +
  labs(title = "Sea Surface Temperature (Q95)",
       x = "Longitude (°W)", y = "Latitude (°N)") +
  base_theme

p_combined <- p1 + p2 +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "right")

p1
p2
p_combined

#two differnt color ranges
library(ggplot2)
library(patchwork)
library(dplyr)

# Celsius -> Fahrenheit
c_to_f <- function(x) x * 9/5 + 32

spc_reduced <- spc_reduced %>%
  mutate(
    q05_1yr_sst_f = c_to_f(q05_1yr_sst_jpl),
    q95_1yr_sst_f = c_to_f(q95_1yr_sst_jpl)
  )

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
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "right"
  )

# No limits set: each panel scales to its own data range
p1 <- ggplot(spc_reduced, aes(x = lon, y = lat)) +
  geom_point(aes(color = q05_1yr_sst_f), size = 2, alpha = 0.85, shape = 16) +
  scale_color_viridis_c(name = "SST (°F)", option = "C") +
  coord_fixed() +
  labs(title = "Sea Surface Temperature (Q05)",
       x = "Longitude (°W)", y = "Latitude (°N)") +
  base_theme
p1
ggsave("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/figures/q05.png", plot = p1, width = 12, height = 6, dpi = 500, bg = "white")


p2 <- ggplot(spc_reduced, aes(x = lon, y = lat)) +
  geom_point(aes(color = q95_1yr_sst_f), size = 2, alpha = 0.85, shape = 16) +
  scale_color_viridis_c(name = "SST (°F)", option = "C") +
  coord_fixed() +
  labs(title = "Sea Surface Temperature (Q95)",
       x = "Longitude (°W)", y = "Latitude (°N)") +
  base_theme
p2

ggsave("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/figures/q95.png", plot = p2, width = 12, height = 6, dpi = 500, bg = "white")


# Side-by-side in one figure, each with its own legend
p_combined <- p1 + p2 + plot_layout(ncol = 2)

p_combined
ggsave("/Users/mayaotsu/Documents/GitHub/MOTSU_MASTERS/figures/q05_q95_combined.png", plot = p_combined, width = 12, height = 6, dpi = 500, bg = "white")
