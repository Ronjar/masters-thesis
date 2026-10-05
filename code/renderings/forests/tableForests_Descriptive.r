library(sf)
library(dplyr)
library(terra)
library(kableExtra)

terraOptions(tempdir = "~/temp")

# ==============================================================================
# DATA
# ==============================================================================
kenya_zones <- st_read("data/KenyaPoliticalGeneral/KenyaPoliticalGeneral1957.gpkg", layer = "KenyaPoliticalGeneral") |>
  st_transform(32737) |>
  st_make_valid()

highlands <- kenya_zones |> filter(type == 2) |> st_union()


settlements_known_area <- st_read("data/SettlementSchemes/Settlement_Schemes.shp", quiet = TRUE) |>
  st_transform(32737)
unknown_settlements <- st_read("data/SettlementSchemes/UNKNOWN_schemes_pnts.shp", quiet = TRUE) |>
  st_transform(32737)

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
  settlements_known_area[, common_cols],
  settlements_unknown_area[, common_cols]
) |>
  filter(S_No != 0) |>
  st_filter(highlands, .predicate = st_intersects) |>
  st_make_valid()

settlements_union <- st_union(settlements_utm) |>
  st_make_valid()


settlements_no_buffer <- settlements_union |>
  st_intersection(highlands) |>
  st_make_valid()

settlements_3km_buffer <- st_buffer(settlements_union, dist = 3000) |>
  st_intersection(highlands) |>
  st_make_valid()

# ==============================================================================
# PROCESSING
# ==============================================================================
process_raster_stats <- function(tif_path, vect_union, vect_3km, is_binary = FALSE) {
  r <- rast(tif_path)

  v_union <- project(vect_union, crs(r))
  v_3km <- project(vect_3km, crs(r))

  r_valid <- !is.na(r)
  area_tot <- global(r_valid, fun = "sum", na.rm = TRUE)$sum
  area_3km <- global(mask(r_valid, v_3km), fun = "sum", na.rm = TRUE)$sum
  area_union <- global(mask(r_valid, v_union), fun = "sum", na.rm = TRUE)$sum

  if (is_binary) {
    r_bin <- r > 0
    total_val <- global(r_bin, fun = "sum", na.rm = TRUE)$sum
    in_3km_val <- global(mask(r_bin, v_3km), fun = "sum", na.rm = TRUE)$sum
    in_union_val <- global(mask(r_bin, v_union), fun = "sum", na.rm = TRUE)$sum
  } else {
    total_val <- global(r, fun = "sum", na.rm = TRUE)$sum
    in_3km_val <- global(mask(r, v_3km), fun = "sum", na.rm = TRUE)$sum
    in_union_val <- global(mask(r, v_union), fun = "sum", na.rm = TRUE)$sum
  }

  list(
    total = total_val,
    in_3km = in_3km_val,
    in_settlement = in_union_val,
    area_total = area_tot,
    area_3km = area_3km,
    area_settlement = area_union
  )
}

v_union_spat <- vect(settlements_no_buffer)
v_3km_spat <- vect(settlements_3km_buffer)

path_deg_tif  <- "data/EU_JRC_TMF/Degraded_Frequency_Kenya_1990_2025_EUJRCTMF.tif"
path_def_tif  <- "data/EU_JRC_TMF/Deforested_Kenya_2025_EUJRCTMF.tif"
path_fc90_tif <- "data/EU_JRC_TMF/ForestCover_Kenya_1990_EUJRCTMF.tif"
path_fc25_tif <- "data/EU_JRC_TMF/ForestCover_Kenya_2025_EUJRCTMF.tif"

cat("Processing Degraded Frequency...\n")
deg_stats  <- process_raster_stats(path_deg_tif, v_union_spat, v_3km_spat, is_binary = TRUE)
cat("Processing Deforested...\n")
def_stats  <- process_raster_stats(path_def_tif, v_union_spat, v_3km_spat, is_binary = TRUE)
cat("Processing Forest Cover 1990...\n")
fc90_stats <- process_raster_stats(path_fc90_tif, v_union_spat, v_3km_spat, is_binary = TRUE)
cat("Processing Forest Cover 2025...\n")
fc25_stats <- process_raster_stats(path_fc25_tif, v_union_spat, v_3km_spat, is_binary = TRUE)


# ==============================================================================
# TABLE
# ==============================================================================
format_cell <- function(val, total) {
  pct <- (val / total) * 100
  val_1000 <- round(val / 1000)

  if (pct >= 99.95) {
    sprintf("%s (100\\%%)", format(val_1000, big.mark = ",", scientific = FALSE))
  } else {
    sprintf("%s (%.1f\\%%)", format(val_1000, big.mark = ",", scientific = FALSE), pct)
  }
}

format_area_pct <- function(area_val, area_total) {
  pct <- (area_val / area_total) * 100

  if (pct >= 99.95) {
    "100\\%"
  } else {
    sprintf("%.1f\\%%", pct)
  }
}

t_spatial <- rbind(
  # 1. Row: Total
  c(format_cell(fc90_stats$total, fc90_stats$total),
    format_cell(fc25_stats$total, fc25_stats$total),
    format_cell(def_stats$total, def_stats$total),
    format_cell(deg_stats$total, deg_stats$total),
    format_area_pct(def_stats$area_total, def_stats$area_total)),

  # 2. Row: Inside Study Area
  c(format_cell(fc90_stats$in_3km, fc90_stats$total),
    format_cell(fc25_stats$in_3km, fc25_stats$total),
    format_cell(def_stats$in_3km, def_stats$total),
    format_cell(deg_stats$in_3km, deg_stats$total),
    format_area_pct(def_stats$area_3km, def_stats$area_total)),

  # 3. Row: Inside Settlements
  c(format_cell(fc90_stats$in_settlement, fc90_stats$total),
    format_cell(fc25_stats$in_settlement, fc25_stats$total),
    format_cell(def_stats$in_settlement, def_stats$total),
    format_cell(deg_stats$in_settlement, deg_stats$total),
    format_area_pct(def_stats$area_settlement, def_stats$area_total)),

  # 4. Row: Total
  c(format_area_pct(fc90_stats$total, fc90_stats$total),
    format_area_pct(fc25_stats$total, fc90_stats$total),
    format_area_pct(def_stats$total, fc90_stats$total),
    format_area_pct(deg_stats$total, fc90_stats$total),
    "-")
)

rownames(t_spatial) <- c(
  "Overall",
  "In study",
  "Schemes",
  "\\% of Forest 1990"
)

colnames(t_spatial) <- c(
  "Forest 1990",
  "Forest 2025",
  "Deforestation",
  "Degradation",
  "of Kenya"
)

t_latex <- kable(t_spatial, format = "latex", booktabs = TRUE, align = "c", escape = FALSE) |>
  add_indent(positions = c(2, 3)) |>
  row_spec(3, extra_latex_after = "\\midrule")

cat(t_latex, file = "output/tables/tableForests_Descriptive.tex")

# ==============================================================================
# KENYA TOTAL AREA PRINT
# ==============================================================================
kenya_full <- st_read("data/Administrative/Kenya_Shape.geojson", quiet = TRUE) |>
  st_transform(32737) |>
  st_make_valid()

kenya_area_sqm <- as.numeric(sum(st_area(kenya_full)))

pixel_area_sqm <- 900

forest_90_area_sqm <- fc90_stats$total * pixel_area_sqm
forest_25_area_sqm <- fc25_stats$total * pixel_area_sqm

cat("\n--- Forest covered area relative to whole Kenya ---\n")
cat(sprintf("Forest Cover 1990: %.2f%%\n", (forest_90_area_sqm / kenya_area_sqm) * 100))
cat(sprintf("Forest Cover 2025: %.2f%%\n", (forest_25_area_sqm / kenya_area_sqm) * 100))
cat("----------------------------------------------------\n")