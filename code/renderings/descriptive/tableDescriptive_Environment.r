library(sf)
library(terra)
library(dplyr)
library(arrow)
library(kableExtra)
terraOptions(tempdir = "~/temp")


# ==============================================================================
# DATA
# ==============================================================================

gpkg_path <- "data/KenyaPoliticalGeneral/KenyaPoliticalGeneral1957.gpkg"
settlements_shape_path <- "data/SettlementSchemes/Settlement_Schemes.shp"
unknown_pnts_path <- "data/SettlementSchemes/UNKNOWN_schemes_pnts.shp"

ndvi_dir <- "data/NDVI/2023_2025"
ndmi_dir <- "data/NDMI/2023_2025"

cities_path    <- "data/Jedwab_Moradi/Cities/cities_1890_and_1900.TAB"
railroads_path <- "data/Jedwab_Moradi/Railroads/Built/Railroads.TAB"

kenya_zones <- st_read(gpkg_path, layer = "KenyaPoliticalGeneral") |>
  st_transform(32737) |>
  st_make_valid()

forests   <- kenya_zones |> filter(type == 0) |> st_union() |> st_make_valid()
highlands <- kenya_zones |> filter(type == 1) |> st_union() |> st_make_valid()

settlements_known_area <- st_read(settlements_shape_path) |>
  st_transform(32737)

settlements_unknown_area <- st_read(unknown_pnts_path) |>
  st_transform(32737) |>
  filter(!is.na(Scheme_Siz) & Scheme_Siz > 0) |>
  mutate(
    area_sqm = Scheme_Siz * 10000,
    radius_m = sqrt(area_sqm / pi)
  )


# ==============================================================================
# PROCESSING
# ==============================================================================

settlements_unknown_area <- st_buffer(settlements_unknown_area, dist = settlements_unknown_area$radius_m)


common_cols <- intersect(names(settlements_known_area), names(settlements_unknown_area))
settlements_unknown_area <- settlements_unknown_area[, common_cols]

settlements_utm <- rbind(
  settlements_known_area[, names(
    settlements_unknown_area)] |>
      mutate(is_known = 1),
    settlements_unknown_area |>
      mutate(is_known = 0)
  ) |>
  filter(Allocation >= 1962 & Allocation < 2023) |>
  st_make_valid()

gap_tolerance <- 5
settlements_expanded <- st_buffer(settlements_utm, dist = gap_tolerance)
settlements_union_expanded <- st_union(st_cast(settlements_expanded, "MULTIPOLYGON"))
treatment_shape <- st_buffer(settlements_union_expanded, dist = -gap_tolerance) |>
  st_make_valid()


control_shape <- st_difference(highlands, forests) |>
  st_difference(treatment_shape) |>
  st_make_valid()

extract_global_metrics <- function(raster_dir, var_name, display_name) {

  files <- list.files(path = raster_dir, pattern = "\\.tif$", full.names = TRUE)
  r_raw <- lapply(files, rast) |>
    sprc() |>
    mosaic()

  r_300m <- aggregate(r_raw, fact = 10, fun = "mean", na.rm = TRUE)

  r_crs <- crs(r_300m)
  treat_vect <- vect(st_transform(treatment_shape, r_crs))
  ctrl_vect  <- vect(st_transform(control_shape, r_crs))

  val_treat <- terra::extract(r_300m, treat_vect)[, 2] |>
    na.omit()
  val_ctrl  <- terra::extract(r_300m, ctrl_vect)[, 2] |>
    na.omit()

  val_all   <- c(val_treat, val_ctrl)

  m_treat <- mean(val_treat)
  m_ctrl  <- mean(val_ctrl)

  tibble(
    `Variables` = display_name,
    `Mean Overall`   = sprintf("%.3f (%.3f)", mean(val_all), sd(val_all)),
    `Mean Treatment` = sprintf("%.3f (%.3f)", m_treat, sd(val_treat)),
    `Mean Control`   = sprintf("%.3f (%.3f)", m_ctrl, sd(val_ctrl)),
    `Difference`     = sprintf("%.3f", m_treat - m_ctrl),
    obs_all   = length(val_all),
    obs_treat = length(val_treat),
    obs_ctrl  = length(val_ctrl)
  )
}

extract_srtm_metrics <- function(dem_path) {
  raw_dem <- rast(dem_path)
  slope_raster <- terrain(raw_dem, v = "slope", unit = "degrees")

  topo_raster <- c(raw_dem, slope_raster) |>
    setNames(c("Elevation", "Slope"))

  topo_300m <- project(topo_raster, "EPSG:32737", res = 300, method = "average")

  treat_vect <- vect(treatment_shape)
  ctrl_vect  <- vect(control_shape)

  val_treat <- terra::extract(topo_300m, treat_vect)
  val_ctrl  <- terra::extract(topo_300m, ctrl_vect)

  calc_topo_row <- function(var_name) {
    v_treat <- na.omit(val_treat[[var_name]])
    v_ctrl  <- na.omit(val_ctrl[[var_name]])
    v_all   <- c(v_treat, v_ctrl)

    m_treat <- mean(v_treat)
    m_ctrl  <- mean(v_ctrl)

    tibble(
      `Variables`      = var_name,
      `Mean Overall`   = sprintf("%.3f (%.3f)", mean(v_all), sd(v_all)),
      `Mean Treatment` = sprintf("%.3f (%.3f)", m_treat, sd(v_treat)),
      `Mean Control`   = sprintf("%.3f (%.3f)", m_ctrl, sd(v_ctrl)),
      `Difference`     = sprintf("%.3f", m_treat - m_ctrl),
      obs_all   = length(v_all),
      obs_treat = length(v_treat),
      obs_ctrl  = length(v_ctrl)
    )
  }

  bind_rows(
    calc_topo_row("Elevation"),
    calc_topo_row("Slope")
  )
}

extract_distance_metrics <- function(feature_path, display_name, template_raster) {
  features <- st_read(feature_path, quiet = TRUE) |>
    st_transform(crs(template_raster)) |>
    st_make_valid()

  feat_vect <- vect(features)

  geom_type <- unique(st_geometry_type(features))
  if (any(grepl("LINE|POLYGON", geom_type))) {
    r_feat <- rasterize(feat_vect, template_raster)
    r_dist <- terra::distance(r_feat)
  } else {
    r_dist <- terra::distance(template_raster, feat_vect)
  }

  treat_vect <- vect(treatment_shape)
  ctrl_vect  <- vect(control_shape)

  val_treat <- terra::extract(r_dist, treat_vect)[, 2] |>
    na.omit()
  val_ctrl  <- terra::extract(r_dist, ctrl_vect)[, 2] |>
    na.omit()

  val_all   <- c(val_treat, val_ctrl)

  m_treat <- mean(val_treat)
  m_ctrl  <- mean(val_ctrl)

  tibble(
    `Variables`      = display_name,
    `Mean Overall`   = sprintf("%.3f (%.3f)", mean(val_all), sd(val_all)),
    `Mean Treatment` = sprintf("%.3f (%.3f)", m_treat, sd(val_treat)),
    `Mean Control`   = sprintf("%.3f (%.3f)", m_ctrl, sd(val_ctrl)),
    `Difference`     = sprintf("%.3f", m_treat - m_ctrl),
    obs_all   = length(val_all),
    obs_treat = length(val_treat),
    obs_ctrl  = length(val_ctrl)
  )
}

e_buffer <- ext(vect(st_buffer(highlands, 50000)))
e_aligned <- ext(
  floor(e_buffer$xmin / 300) * 300,
  ceiling(e_buffer$xmax / 300) * 300,
  floor(e_buffer$ymin / 300) * 300,
  ceiling(e_buffer$ymax / 300) * 300
)
template_300m <- rast(e_aligned, res = 300, crs = "EPSG:32737")

stats_ndvi <- extract_global_metrics(ndvi_dir, "NDVI", "NDVI")
stats_ndmi <- extract_global_metrics(ndmi_dir, "NDMI", "NDMI")
stats_srtm <- extract_srtm_metrics("data/SRTM/SRTM_Kenya.tif")
stats_rail <- extract_distance_metrics(railroads_path, "Distance to railroad", template_300m)
stats_city <- extract_distance_metrics(cities_path, "Distance to city", template_300m)

summary_data <- bind_rows(stats_ndvi, stats_ndmi, stats_srtm, stats_rail, stats_city)


# ==============================================================================
# TABLE
# ==============================================================================

row_obs <- tibble(
  `Variables`      = "Observations",
  `Mean Overall`   = format(summary_data$obs_all[1], big.mark = ",", scientific = FALSE),
  `Mean Treatment` = format(summary_data$obs_treat[1], big.mark = ",", scientific = FALSE),
  `Mean Control`   = format(summary_data$obs_ctrl[1], big.mark = ",", scientific = FALSE),
  `Difference`     = ""
)

final_table <- summary_data |>
  select(`Variables`, `Mean Overall`, `Mean Treatment`, `Mean Control`, `Difference`) |>
  bind_rows(row_obs)

latex_table <- kable(
  final_table,
  format = "latex",
  booktabs = TRUE,
  align = c("l", "c", "c", "c", "c"),
  linesep = ""
)

cat(latex_table, file = "output/tables/tableDescriptive_Environment.tex")