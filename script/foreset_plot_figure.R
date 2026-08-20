rm(list = ls()) 

library(png)
library(grid)
library(ggplot2)
library(patchwork)

base_path <- "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/output/forest_plots/08.12.26/"

# ORDERED to match desired grid:
files <- c(
  "taape_full_reduced_no_island.png",
  "toau_full_reduced_no_island.png",
  "roi_full_reduced_no_island.png",
  
  "taape_mhi_reduced_no_island.png",
  "toau_mhi_reduced_no_island.png",
  "roi_mhi_reduced_no_island.png"
)

files <- file.path(base_path, files)

titles <- c(
  "(a) ",
  "(b) ",
  "(c)",
  "(d) ",
  "(e) ",
  "(f) "
)

#instead of putting these manually is there a way to pull these from the model so it is more reproducible?
auc_vals <- c(0.84, 0.86, 0.87, #taape full, toau full, roi full
              0.80, 0.76, 0.75) #taape mhi, toau mhi, roi mhi
tss_vals <- c(0.55, 0.58, 0.62, 
              0.49, 0.44, 0.40)

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
  "/Users/mayaotsu/Documents/Github/MOTSU_MASTERS/figures/forestplots_combined_no_island.png",
  combined,
  width = 12,
  height = 8,
  dpi = 300
)
dev.off()
