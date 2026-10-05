library(sf)
library(terra)
library(tidyr)
library(dplyr)
library(ggplot2)
source("code/renderings/base_theme.r")

# ==============================================================================
# DATA & PROCESSING
# ==============================================================================

lsat <- rast("data/LandsatCalculation/Landsat8_AllBands.tif")
names(lsat) <- paste0("band_", 1:7)

lc <- rast("data/LandsatCalculation/ESA_WorldCover.tif")
lc_aligned <- project(lc, lsat, method = "near")

rcl_matrix <- rbind(
  c(10, 1),
  c(20, 2),
  c(30, 2),
  c(40, 3),
  c(50, 4),
  c(80, 7),
  c(90, 5),
  c(95, 6)
)
lc_class <- classify(lc_aligned, rcl = rcl_matrix, others = NA)

stats_df <- zonal(lsat, lc_class, fun = "mean") |>
  as_tibble()

colnames(stats_df)[1] <- "Landcover_ID"

long_df <- stats_df |>
  pivot_longer(
    cols         = starts_with("band_"),
    names_to     = "band",
    names_prefix = "band_",
    values_to    = "Reflectance"
  ) |>
  mutate(
    band = as.integer(band),
    zone = as.factor(Landcover_ID)
  )

# ==============================================================================
# PLOT
# ==============================================================================

plot_bands <- ggplot(long_df, aes(x = band, y = Reflectance, group = zone)) +
  geom_line(aes(color = zone), linewidth = lwidth_thick) +
  geom_point(aes(color = zone), size = 2) +
  labs(
    x     = "Landsat 8 band",
    y     = "Reflectance",
    title = NULL
  ) +
  scale_x_continuous(breaks = 1:7) +
  scale_colour_manual(
    name   = "Landcover",
    values = c(
      "1" = col_forests,
      "2" = theme_colors[2],
      "3" = theme_colors[6],
      "4" = theme_colors[10],
      "5" = theme_colors[4],
      "6" = theme_colors[8],
      "7" = theme_colors[1]
    ),
    labels = c("1" = "Tree cover", "2" = "Shrubland", "3" = "Cropland", "4" = "Built-up", "5" = "Wetlands", "6" = "Mangroves", "7" = "Waterbodies")
  ) +
  coord_cartesian(ylim = c(0.0, 0.35)) +
  graph_theme()

ggsave(
  filename = "output/figures/figureDescriptive_LandsatBands.png",
  plot     = plot_bands,
  width    = 10,
  height   = 6,
  dpi      = 300
)