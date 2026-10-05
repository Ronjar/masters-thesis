library(sf)
library(terra)
library(ggplot2)
library(dplyr)
library(nngeo)
source("code/renderings/base_theme.r")

# ==============================================================================
# DATA
# ==============================================================================

target_crs <- 32737 # UTM Zone 37S

kenya_outline <- st_read("data/Administrative/Kenya_Shape.geojson") |>
  st_transform(target_crs)

county_borders <- st_read("data/Administrative/KenyaCounties_Shape.geojson") |>
  st_transform(target_crs)

lake_victoria <- county_borders |>
  filter(ADM2_NAME == "Administrative unit not available")

district_borders <- county_borders |>
  filter(ADM2_NAME != "Administrative unit not available")

settlements_known_area <- st_read("data/SettlementSchemes/Settlement_Schemes.shp") |>
  st_transform(target_crs)

unknown_settlements <- st_read("data/SettlementSchemes/UNKNOWN_schemes_pnts.shp") |>
  st_transform(target_crs)

# ==============================================================================
# PROCESSING
# ==============================================================================

settlements_unknown_area <- unknown_settlements |>
  filter(!is.na(Scheme_Siz) & Scheme_Siz > 0) |>
  mutate(
    area_sqm = Scheme_Siz * 10000,
    radius_m = sqrt(area_sqm / pi)
  )

settlements_unknown_area <- st_buffer(settlements_unknown_area, dist = settlements_unknown_area$radius_m) |>
  select(-area_sqm, -radius_m)

common_cols <- intersect(names(settlements_known_area), names(settlements_unknown_area))

settlements_utm <- rbind(
  settlements_known_area[, common_cols] |> mutate(is_known = 1),
  settlements_unknown_area[, common_cols] |> mutate(is_known = 0)
) |>
  filter(S_No != 0) |>
  st_make_valid()

gap_tolerance <- 100
settlements_borders <- settlements_utm |>
  st_buffer(dist = gap_tolerance) |>
  st_cast("MULTIPOLYGON") |>
  st_union() |>
  st_buffer(dist = -gap_tolerance) |>
  nngeo::st_remove_holes() |>
  st_cast("MULTILINESTRING")


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
# LEGEND
# ==============================================================================

legend_levels <- c(
  "Settlement Schemes",
  " ",
  "Settlement Borders\nused for the Analysis"
)

fill_colors <- setNames(
  c(col_settlements_fill, NA, NA),
  legend_levels
)

border_colors <- setNames(
  c(col_settlements, NA, col_highlands),
  legend_levels
)

# ==============================================================================
# PLOT
# ==============================================================================

p_settlement <- ggplot() +

  # Layer 1: Settlement Schemes
  geom_sf(
    data = settlements_utm,
    aes(fill = "Settlement Schemes", color = "Settlement Schemes"),
    linewidth = lwidth_thin,
    inherit.aes = FALSE
  ) +

  # Layer 2: Settlement Borders
  geom_sf(
    data = settlements_borders,
    aes(fill = "Settlement Borders\nused for the Analysis", color = "Settlement Borders\nused for the Analysis"),
    linewidth = lwidth_thick,
    inherit.aes = FALSE
  ) +

  # Layer 3: Lake Victoria
  geom_sf(
    data = lake_victoria,
    fill = col_water,
    color = NA,
    inherit.aes = FALSE
  ) +

  # Base Map Boundaries
  geom_sf(data = district_borders, fill = NA, color = "black", linewidth = lwidth_thin, inherit.aes = FALSE) +
  geom_sf(data = kenya_outline, fill = NA, color = "black", linewidth = lwidth_base, inherit.aes = FALSE) +

  # Legend
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
        linewidth = c(lwidth_thin, 0, lwidth_thick)
      )
    ),
    color = "none"
  ) +

  coord_sf(
    xlim = rv_window[c("xmin", "xmax")],
    ylim = rv_window[c("ymin", "ymax")],
    expand = FALSE
  ) +
  map_theme(text_scale = 1.5) +
  theme(
    legend.position      = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background    = element_rect(fill = "white", color = "black", linewidth = lwidth_thin),
    legend.key           = element_rect(fill = "white", color = NA),
    legend.margin        = margin(t = 15, r = 15, b = 15, l = 15)
  )

ggsave(
  filename = "output/figures/figureDescriptive_SuperSettlements.png",
  plot     = p_settlement,
  width    = 12,
  height   = 14,
  dpi      = 800,
  bg       = "transparent"
)