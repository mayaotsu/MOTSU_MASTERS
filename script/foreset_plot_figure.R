rm(list = ls()) 

library(png)
library(grid)
library(ggplot2)
library(patchwork)

base_path <- "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/forest_plots/07.21"

# ORDERED to match desired grid:
files <- c(
  "taape_full_reduced_0.001_0.75_forestplot07.21.png",
  "toau_full_reduced_0.001_0.75_forestplot07.21.png",
  "roi_full_reduced_0.001_0.75_forestplot_7.21.png",
  
  "taape_mhi_reduced_0.001_0.75_forestplot07.21.png",
  "toau_mhi_reduced_0.001_0.75_forestplot07.21.png",
  "roi_mhi_reduced_0.001_0.75_forestplot07.21.png"
)

files <- file.path(base_path, files)

# titles <- c(
#   "Taʻape (Full Archipelago)",
#   "Toʻau - (Full Archipelago)",
#   "ROI - (Full Archipelago)",
#   "Taʻape - (MHI)",
#   "Toʻau - (MHI)",
#   "ROI - (MHI)"
# )

titles <- c(
  "(a) ",
  "(b) ",
  "(c)",
  "(d) ",
  "(e) ",
  "(f) "
)

auc_vals <- c(0.87, 0.89, 0.89, #taape full, toau full, roi full
              0.82, 0.79, 0.79) #taape mhi, toau mhi, roi mhi
tss_vals <- c(0.61, 0.65, 0.65, 
              0.51, 0.46, 0.46)

img_to_plot <- function(file, title, auc, tss) {
  img <- readPNG(file)
  g <- rasterGrob(img, interpolate = TRUE)
  
  label <- sprintf("AUC = %.2f\nTSS = %.2f", auc, tss)
  
  ggplot() +
    annotation_custom(g, -Inf, Inf, -Inf, Inf) +
    annotate(
      "text",
      x = 0.90, y = 0.30,
      label = label,
      hjust = 1, vjust = 0,  # push slightly inward
      size = 3.5
    ) +
    theme_void() +
    ggtitle(title) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme(plot.title = element_text(hjust = 0.5, size = 12))
}

plots <- Map(img_to_plot, files, titles, auc_vals, tss_vals)

combined <- wrap_plots(plots, ncol = 3)
print(combined)

ggsave(
  "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/figures/forestplots_combined_2x3.png",
  combined,
  width = 12,
  height = 8,
  dpi = 300
)
dev.off()
