library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(patchwork)
source("code/renderings/base_theme.r")

terraOptions(tempdir = "~/temp")

# ==============================================================================
# DATA
# ==============================================================================

# NDVI 1984-1986
ndvi_84_0 <- rast("data/NDVI/1984_1986/NDVI_Kenya_1984_1986_Median_0.tif")
ndvi_84_1 <- rast("data/NDVI/1984_1986/NDVI_Kenya_1984_1986_Median_1.tif")
ndvi_1984 <- mosaic(ndvi_84_0, ndvi_84_1, fun = "mean")

# NDVI 2023-2025
ndvi_23_0 <- rast("data/NDVI/2023_2025/NDVI_Kenya_2023_2025_Median_0.tif")
ndvi_23_1 <- rast("data/NDVI/2023_2025/NDVI_Kenya_2023_2025_Median_1.tif")
ndvi_2025 <- mosaic(ndvi_23_0, ndvi_23_1, fun = "mean")

# 1990
tmf_1990 <- rast("data/EU_JRC_TMF/ForestCover_Kenya_1990_EUJRCTMF.tif")
tmf_1990[tmf_1990 == 0] <- NA

# 2025 Changes
deg_2025 <- rast("data/EU_JRC_TMF/Degraded_Kenya_2025_EUJRCTMF.tif")
def_2025 <- rast("data/EU_JRC_TMF/Deforested_Kenya_2025_EUJRCTMF.tif")

tmf_2025 <- tmf_1990
tmf_2025[deg_2025 > 0] <- 2
tmf_2025[def_2025 > 0] <- 3

# GeoJSON
target_crs <- crs(ndvi_84_0)
kenya_outline <- st_read("data/Administrative/Kenya_Shape.geojson") |>
  st_transform(target_crs)

county_borders <- st_read("data/Administrative/KenyaCounties_Shape.geojson") |>
  st_transform(target_crs)

lake_victoria <- county_borders |>
  filter(ADM2_NAME == "Administrative unit not available")

district_borders <- county_borders |>
  filter(ADM2_NAME != "Administrative unit not available")

lake_vect <- vect(lake_victoria)
ndvi_1984 <- mask(ndvi_1984, lake_vect, inverse = TRUE)
ndvi_2025 <- mask(ndvi_2025, lake_vect, inverse = TRUE)

rv_window <- st_bbox(c(
  xmin = 34.0,
  xmax = 37.8,
  ymin = -3.0,
  ymax = 1.5
), crs = 4326) |>
  st_as_sfc() |>
  st_transform(target_crs) |>
  st_bbox()


# ==============================================================================
# THEME
# ==============================================================================

master_extent <- st_bbox(ndvi_1984)

theme_master <- map_theme(text_scale = 1.2) +
  theme(
    legend.position = "none",
    axis.title.x    = element_text(size = tsize_title),
    plot.margin     = margin(15, 15, 15, 15, "pt"),
    plot.title      = element_text(
      hjust  = 0,
      margin = margin(b = 8, "pt")
    )
  )

theme_master_legend <- theme_master +
  theme(
    legend.position      = "right",
    legend.justification = c(0, 0.5),
    legend.box.just      = "left",
    legend.margin        = margin(15, 15, 15, 15, "pt")
  )

vector_layers <- list(
  geom_sf(data = lake_victoria, fill = col_water, color = NA, inherit.aes = FALSE),
  geom_sf(data = district_borders, fill = NA, color = "grey70", linewidth = lwidth_thin, inherit.aes = FALSE),
  geom_sf(data = kenya_outline, fill = NA, color = "black", linewidth = lwidth_base, inherit.aes = FALSE)
)

# ==============================================================================
# PLOT
# ==============================================================================

p_ndvi_84 <- ggplot() +
  geom_spatraster(data = ndvi_1984) +
  vector_layers +
  scale_fill_gradientn(
    colors = c("#d73027", "#fdae61", "#ffffbf", "#d9ef8b", "#1a9850", "#006837"),
    limits = c(-0.1, 0.95),
    na.value = NA
  ) +
  coord_sf(xlim = rv_window[c("xmin", "xmax")], ylim = rv_window[c("ymin", "ymax")], expand = FALSE) +
  labs(title = "NDVI", x = "1984-1986") +
  theme_master


p_ndvi_25 <- ggplot() +
  geom_spatraster(data = ndvi_2025) +
  vector_layers +
  scale_fill_gradientn(
    name = "NDVI",
    colors = c("#d73027", "#fdae61", "#ffffbf", "#d9ef8b", "#1a9850", "#006837"),
    limits = c(-0.1, 0.95),
    na.value = NA
  ) +
  coord_sf(xlim = rv_window[c("xmin", "xmax")], ylim = rv_window[c("ymin", "ymax")], expand = FALSE) +
  labs(title = "", x = "2023-2025") +
  theme_master_legend

p_tmf_90 <- ggplot() +
  geom_spatraster(data = as.factor(tmf_1990)) +
  vector_layers +
  scale_fill_manual(values = c("1" = col_forests), na.value = NA) +
  coord_sf(xlim = rv_window[c("xmin", "xmax")], ylim = rv_window[c("ymin", "ymax")], expand = FALSE) +
  labs(title = "Forest Cover", x = "1990") +
  theme_master

p_tmf_25 <- ggplot() +
  geom_spatraster(data = as.factor(tmf_2025)) +
  vector_layers +
  scale_fill_manual(
    name = "Status",
    values = c(
      "1"   = col_forests,
      " "   = NA,
      "2"   = theme_colors[2],
      "  "  = NA,
      "3"   = col_settlements
    ),
    labels = c(
      "1"   = "Undisturbed",
      " "   = "",
      "2"   = "Degraded",
      "  "  = "",
      "3"   = "Deforested"
    ),
    limits = c("1", " ", "2", "  ", "3"),
    drop   = FALSE,
    na.value = NA
  ) +
  guides(
    fill = guide_legend(override.aes = list(color = NA))
  ) +
  coord_sf(xlim = rv_window[c("xmin", "xmax")], ylim = rv_window[c("ymin", "ymax")], expand = FALSE) +
  labs(title = "", x = "2025") +
  theme_master_legend


final_figure <- (p_ndvi_84 | p_ndvi_25) / (p_tmf_90 | p_tmf_25) +
  plot_annotation(
    theme = theme(
      plot.background  = element_blank(),
      panel.background = element_blank()
    )
  ) &
  theme(
    plot.background  = element_blank(),
    panel.background = element_blank()
  )

ggsave(
  filename = "output/figures/figureDescriptive_TimeComparison.png",
  plot     = final_figure,
  width    = 11,
  height   = 10,
  dpi      = 400,
  bg       = "transparent"
)