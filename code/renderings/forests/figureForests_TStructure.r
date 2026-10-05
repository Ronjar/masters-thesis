library(arrow)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(ggnewscale)
source("code/renderings/base_theme.r")

terraOptions(tempdir = "~/temp")

# ==============================================================================
# PARAMETERS
# ==============================================================================

center_x <- 4066181
center_y <- -25080

plot_width_half <- 3000
plot_height_half <- 3000

bbox_3857 <- st_bbox(
  c(xmin = center_x - plot_width_half, xmax = center_x + plot_width_half,
    ymin = center_y - plot_height_half, ymax = center_y + plot_height_half),
  crs = st_crs(3857)
)
bbox_poly_3857 <- st_as_sfc(bbox_3857)


# ==============================================================================
# DATA PROCESSING
# ==============================================================================

# 0: Forest Cover 1990
forest_raw <- rast("data/EU_JRC_TMF/ForestCover_Kenya_1990_EUJRCTMF.tif")
forest_proj <- project(forest_raw, "epsg:3857")
forest_crop <- crop(forest_proj, ext(bbox_3857))
df_forest <- as.data.frame(forest_crop, xy = TRUE)
names(df_forest)[3] <- "forest_cover"
df_forest <- df_forest |> filter(forest_cover > 0)


# 1: Forest Disturbance Frequency
freq_raw <- rast("data/EU_JRC_TMF/Degraded_Frequency_Kenya_1990_2025_EUJRCTMF.tif")
freq_proj <- project(freq_raw, "epsg:3857")
freq_crop <- crop(freq_proj, ext(bbox_3857))
df_freq <- as.data.frame(freq_crop, xy = TRUE)
names(df_freq)[3] <- "frequency"
df_freq <- df_freq |> filter(frequency > 0)


# 2 Parquet Data
analysis_data <- read_parquet("data/Regressions/forests/Forest_analysis_data.parquet")

points_32737 <- analysis_data |>
  st_as_sf(coords = c("x", "y"), crs = 32737)

bbox_poly_32737 <- st_transform(bbox_poly_3857, 32737)
points_cropped <- st_crop(points_32737, bbox_poly_32737)

x_vals <- sort(unique(analysis_data$x))
grid_size <- min(diff(x_vals)[diff(x_vals) > 0])

# 3: Grid
grid_sf <- st_buffer(points_cropped, dist = grid_size / 2, endCapStyle = "SQUARE") |>
  st_transform(3857)


df_disturbances <- grid_sf |>
  filter(!is.na(y_degradation) & y_degradation > 0) |>
  mutate(
    x_plot = st_coordinates(st_centroid(geometry))[, 1],
    y_plot = st_coordinates(st_centroid(geometry))[, 2]
  ) |>
  st_drop_geometry()

# 4. Cell Assignments
grid_zones <- grid_sf |>
  mutate(
    zone = case_when(
      is_treated_300 == 1 & is_forest == 0 ~ "T_NF",
      is_treated_300 == 1 & is_forest == 1 ~ "T_F",
      is_treated_300 == 0 & is_forest == 0 ~ "C_NF",
      is_treated_300 == 0 & is_forest == 1 ~ "C_F",
      TRUE ~ "Other"
    )
  ) |>
  filter(zone != "Other") |>
  group_by(zone) |>
  summarise(geometry = st_union(geometry), .groups = "drop")

grid_bbox <- st_bbox(grid_sf)

grid_center_x <- mean(c(grid_bbox["xmin"], grid_bbox["xmax"]))
grid_center_y <- mean(c(grid_bbox["ymin"], grid_bbox["ymax"]))


# ==============================================================================
# PLOT
# ==============================================================================

plot_map <- ggplot() +

  # 0: Forest Cover 1990
  geom_tile(data = df_forest, aes(x = x, y = y), fill = col_forests) +

  # 1: Forest Disturbance Frequency
  geom_tile(data = df_freq, aes(x = x, y = y, fill = frequency), alpha = 0.8) +
  scale_fill_viridis_c(
    name = "Degradation (Pixels)",
    option = "mako",
    direction = -1,
    begin = 0.6,
    end = 0.1,
    na.value = "transparent"
  ) +

  # 3: Grid
  geom_sf(data = grid_sf, fill = NA, color = "grey90", linewidth = lwidth_thin, linetype = "dashed", alpha = 0.8) +

  new_scale_fill() +
  new_scale_color() +

  geom_sf(data = grid_zones, aes(fill = zone, color = zone, linewidth = zone)) +

  scale_fill_manual(
    name = "Cell Assignment",
    values = c(
      "T_F"  = scales::alpha(col_settlements_fill, 0.2),
      "T_NF" = scales::alpha(col_settlements_fill, 0.4),
      "C_F"  = scales::alpha(col_highlands_fill, 0.2),
      "C_NF" = scales::alpha(col_highlands_fill, 0.4)
    ),
    labels = c("T_F" = "Treated Forest", "T_NF" = "Treated Non-Forest", "C_F" = "Control Forest", "C_NF" = "Control Non-Forest")
  ) +
  scale_color_manual(
    name = "Cell Assignment",
    values = c(
      "T_F"  = scales::alpha(col_settlements, 0.8),
      "T_NF" = scales::alpha(col_settlements, 0.8),
      "C_F"  = scales::alpha(col_forests, 0.8),
      "C_NF" = scales::alpha(col_forests, 0.8)
    ),
    labels = c("T_F" = "Treated Forest", "T_NF" = "Treated Non-Forest", "C_F" = "Control Forest", "C_NF" = "Control Non-Forest")
  ) +
  scale_linewidth_manual(
    name = "Cell Assignment",
    values = c("T_F" = lwidth_thick, "T_NF" = lwidth_thick, "C_F" = lwidth_thick, "C_NF" = lwidth_thick),
    labels = c("T_F" = "Treated Forest", "T_NF" = "Treated Non-Forest", "C_F" = "Control Forest", "C_NF" = "Control Non-Forest")
  ) +

  coord_sf(
    xlim = c(grid_bbox["xmin"], grid_bbox["xmax"]),
    ylim = c(grid_bbox["ymin"], grid_bbox["ymax"]),
    expand = FALSE,
    crs = st_crs(3857)
  ) +
  map_theme()

ggsave(
  filename = "output/figures/figureForests_TStructure.png",
  plot     = plot_map,
  width    = 10,
  height   = 7,
  dpi      = 300)