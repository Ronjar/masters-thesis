library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(dplyr)
source("code/renderings/base_theme.r")

sf_use_s2(FALSE)

# ==============================================================================
# DATA
# ==============================================================================

base_map <- rast("data/KenyaPoliticalGeneral/MapGeoreferenced.tif")

vector_data <- st_read("data/KenyaPoliticalGeneral/KenyaPoliticalGeneral1957.gpkg",
                       layer = "KenyaPoliticalGeneral", quiet = TRUE) |>
  st_make_valid() |>
  st_transform(crs(base_map)) |>
  st_make_valid()

# ==============================================================================
# PROCESSING
# ==============================================================================

# Layer 0: Base Map (Grayscale)
if (nlyr(base_map) >= 3) {
  gray_val <- 0.2989 * base_map[[1]] + 0.5870 * base_map[[2]] + 0.1140 * base_map[[3]]
  if (nlyr(base_map) >= 4) {
    base_map_bw <- c(gray_val, gray_val, gray_val, base_map[[4]])
  } else {
    base_map_bw <- c(gray_val, gray_val, gray_val)
  }
  names(base_map_bw) <- names(base_map)[1:nlyr(base_map_bw)]
} else {
  base_map_bw <- base_map
}

# Layer 1: Kenya Highlands
highlands <- vector_data |>
  filter(type == 2) |>
  st_union() |>
  st_make_valid()

# Layer 2: Crown Forests
forests <- vector_data |>
  filter(type == 1) |>
  st_union() |>
  st_make_valid()

# Layer 3: Settlement Schemes
target_crs <- 32737

settlements_known <- st_read("data/SettlementSchemes/Settlement_Schemes.shp", quiet = TRUE) |>
  st_make_valid() |>
  st_transform(target_crs)

unknown_pnts <- st_read("data/SettlementSchemes/UNKNOWN_schemes_pnts.shp", quiet = TRUE) |>
  st_make_valid() |>
  st_transform(target_crs)

settlements_unknown <- unknown_pnts |>
  filter(!is.na(Scheme_Siz) & Scheme_Siz > 0) |>
  mutate(
    area_sqm = Scheme_Siz * 10000,
    radius_m = sqrt(area_sqm / pi)
  )

settlements_unknown <- st_buffer(settlements_unknown, dist = settlements_unknown$radius_m) |>
  select(-area_sqm, -radius_m) |>
  st_make_valid()

common_cols <- intersect(names(settlements_known), names(settlements_unknown))

settlements <- rbind(
  settlements_known[, common_cols],
  settlements_unknown[, common_cols]
) |>
  filter(S_No != 0) |>
  st_make_valid() |>
  st_transform(crs(base_map)) |>
  st_make_valid()

# ==============================================================================
# LEGEND
# ==============================================================================

legend_levels <- c(
  "Kenya Highlands",
  " ",
  "Crown Forests",
  "  ",
  "Settlement Schemes"
)

fill_colors <- setNames(
  c(col_highlands_fill, NA, col_forests_fill, NA, col_settlements_fill),
  legend_levels
)

border_colors <- setNames(
  c(col_highlands, NA, col_forests, NA, col_settlements),
  legend_levels
)

map_ext <- ext(base_map)

# ==============================================================================
# PLOT
# ==============================================================================

map_plot <- ggplot() +
  # Layer 0: Grayscale Map
  geom_spatraster_rgb(data = base_map_bw) +

  # Layer 1: Kenya Highlands
  geom_sf(
    data = highlands,
    aes(fill = "Kenya Highlands", color = "Kenya Highlands"),
    linewidth = lwidth_base,
    inherit.aes = FALSE
  ) +

  # Layer 2: Crown Forests
  geom_sf(
    data = forests,
    aes(fill = "Crown Forests", color = "Crown Forests"),
    linewidth = lwidth_base,
    inherit.aes = FALSE
  ) +

  # Layer 3: Settlement Schemes
  geom_sf(
    data = settlements,
    aes(fill = "Settlement Schemes", color = "Settlement Schemes"),
    linewidth = lwidth_thin,
    inherit.aes = FALSE
  ) +

  scale_fill_manual(
    name   = "Land Classification",
    values = fill_colors,
    breaks = legend_levels,
    limits = legend_levels,
    drop   = FALSE
  ) +
  scale_color_manual(
    name   = "Land Classification",
    values = border_colors,
    breaks = legend_levels,
    limits = legend_levels,
    drop   = FALSE
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(
        fill      = unname(fill_colors),
        color     = unname(border_colors),
        linewidth = c(lwidth_base, 0, lwidth_base, 0, lwidth_thin)
      )
    ),
    color = "none"
  ) +

  coord_sf(
    xlim   = c(map_ext$xmin, map_ext$xmax),
    ylim   = c(map_ext$ymin, map_ext$ymax),
    expand = FALSE
  ) +
  map_theme() +
  theme(
    legend.position      = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background    = element_rect(fill = "white", color = "black", linewidth = lwidth_thin),
    legend.key           = element_rect(fill = "white", color = NA),
    legend.margin        = margin(15, 15, 15, 15)
  )

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = "output/figures/figureDescriptive_Georef.png",
  plot     = map_plot,
  width    = 9,
  height   = 10,
  dpi      = 300
)