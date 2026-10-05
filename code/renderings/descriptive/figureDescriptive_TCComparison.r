library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(maptiles)
source("code/renderings/base_theme.r")

# ==============================================================================
# PARAMETERS
# ==============================================================================

center_x <- 3921852
center_y <- -86336
half_edge <- 1000

bbox_3857 <- st_bbox(c(
  xmin = center_x - half_edge,
  xmax = center_x + half_edge,
  ymin = center_y - half_edge,
  ymax = center_y + half_edge
), crs = st_crs(3857))

poly_3857 <- st_as_sfc(bbox_3857)

# ==============================================================================
# DATA
# ==============================================================================

# Layer 0: Satellite Imagery (Esri World Imagery)
sat_tiles <- get_tiles(poly_3857, provider = "Esri.WorldImagery", zoom = 16, crop = TRUE)

# Layer 1: Settlement Schemes
schemes <- st_read("data/SettlementSchemes/Settlement_Schemes.shp", quiet = TRUE) |>
  st_transform(3857)


# ==============================================================================
# COLOR & STYLE DEFINITION
# ==============================================================================

legend_levels <- "Settlement Schemes"

fill_colors <- setNames(col_settlements_fill, legend_levels)
border_colors <- setNames(col_settlements, legend_levels)

# ==============================================================================
# PLOT
# ==============================================================================

plot_tc <- ggplot() +
  # Layer 0: Satellite Image
  geom_spatraster_rgb(data = sat_tiles, maxcell = 5e6) +

  # Layer 1: Settlement Schemes
  geom_sf(
    data = schemes,
    aes(fill = "Settlement Schemes", color = "Settlement Schemes"),
    linewidth = lwidth_thick,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    name   = "Land Classification",
    values = fill_colors
  ) +
  scale_color_manual(
    name   = "Land Classification",
    values = border_colors
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(
        fill      = fill_colors,
        color     = border_colors,
        linewidth = lwidth_thick
      )
    ),
    color = "none"
  ) +

  coord_sf(
    xlim = c(bbox_3857["xmin"], bbox_3857["xmax"]),
    ylim = c(bbox_3857["ymin"], bbox_3857["ymax"]),
    expand = FALSE
  ) +
  map_theme(0.8) +
  theme(
    legend.position      = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background    = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.key           = element_rect(fill = "white", color = NA),
    legend.margin        = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt")
  )

ggsave(
  filename = "output/figures/figureDescriptive_TCComparison.png",
  plot     = plot_tc,
  width    = 10,
  height   = 10,
  dpi      = 300
)