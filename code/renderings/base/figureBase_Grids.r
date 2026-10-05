library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(shadowtext)
source("code/renderings/base_theme.r")

# ==============================================================================
# PARAMETERS
# ==============================================================================

center_3857 <- st_sfc(st_point(c(4066181, -25080)), crs = 3857)

center_32737 <- st_transform(center_3857, 32737)
center_x_utm <- st_coordinates(center_32737)[1, 1]
center_y_utm <- st_coordinates(center_32737)[1, 2]

plot_radius <- 1200

# ==============================================================================
# DATA PROCESSING
# ==============================================================================

ndvi_raw <- rast("data/NDVI/1984_1986/NDVI_Kenya_1984_1986_Median_0.tif")

bbox_poly_native <- st_buffer(center_32737, dist = 2000) |>
  st_transform(st_crs(crs(ndvi_raw)))
bbox_native <- st_bbox(bbox_poly_native)

zoom_ext <- ext(
  bbox_native["xmin"], bbox_native["xmax"],
  bbox_native["ymin"], bbox_native["ymax"]
)

# Layer 0: 30m NDVI
layer0_30m <- crop(ndvi_raw, zoom_ext) |>
  project("epsg:32737", res = 30)

names(layer0_30m) <- "NDVI"
df_30m <- as.data.frame(layer0_30m, xy = TRUE)

# Layer 1: 300m Observation Grid
layer1_300m <- aggregate(layer0_30m, fact = 10)
poly_300m <- as.polygons(layer1_300m, dissolve = FALSE) |> st_as_sf()
points_300m <- st_centroid(poly_300m)

# Layer 2: 1200m Cluster Grid
layer2_1200m <- aggregate(layer1_300m, fact = 4)
poly_1200m <- as.polygons(layer2_1200m, dissolve = FALSE) |> st_as_sf()

# Layer 3: Settlement Schemes
schemes <- st_read("data/SettlementSchemes/Settlement_Schemes.shp", quiet = TRUE) |>
  st_transform(32737)


# ==============================================================================
# PLOT
# ==============================================================================

plot_grid <- ggplot() +
  # Layer 0: 30m NDVI
  geom_tile(data = df_30m, aes(x = x, y = y, fill = NDVI)) +
  scale_fill_viridis_c(name = "NDVI", option = "D", direction = 1, na.value = "transparent") +

  # Layer 1: 300m Observation Grid
  geom_sf(data = poly_300m, fill = NA, color = "white", linewidth = lwidth_thin, alpha = 0.8) +

  # Layer 2: Observation Points
  geom_sf(data = points_300m, aes(fill = NDVI), color = "black", shape = 21, size = 4, stroke = 1.2) +

  # Layer 3: 1200m Cluster Grid
  geom_sf(data = poly_1200m, fill = NA, color = "black", linewidth = lwidth_thick) +

  # Layer 4: Settlement Scheme Boundary
  geom_sf(data = schemes, fill = col_settlements_fill, color = col_settlements, linewidth = lwidth_thick) +

  # Layer 5: Labels
  shadowtext::geom_shadowtext(aes(x = center_x_utm - 600, y = center_y_utm + 650, label = "Treatment"),
    color = "black", bg.color = "white", bg.r = 0.1, size = 6, fontface = "bold"
  ) +
  shadowtext::geom_shadowtext(aes(x = center_x_utm - 600, y = center_y_utm - 650, label = "Control"),
    color = "black", bg.color = "white", bg.r = 0.1, size = 6, fontface = "bold"
  ) +
  coord_sf(
    xlim = c(center_x_utm - plot_radius, center_x_utm + plot_radius),
    ylim = c(center_y_utm - plot_radius, center_y_utm + plot_radius),
    expand = FALSE
  ) +
  map_theme(text_scale = 1.2)

ggsave("output/figures/figureBase_Grids.png", plot = plot_grid, width = 8, height = 7, dpi = 300)
