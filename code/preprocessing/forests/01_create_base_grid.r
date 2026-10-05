
#=============================================================================
# DATA
#=============================================================================

gpkg_path <- "data/KenyaPoliticalGeneral/KenyaPoliticalGeneral1957.gpkg"
kenya_zones <- st_read(gpkg_path, layer = "KenyaPoliticalGeneral") |>
  st_transform(32737) |>
  st_make_valid()

settlements_known_area <- st_read("data/SettlementSchemes/Settlement_Schemes.shp") |>
  st_transform(32737)
unknown_settlements <- st_read("data/SettlementSchemes/UNKNOWN_schemes_pnts.shp") |>
  st_transform(32737)


#=============================================================================
# PROCESSING
#=============================================================================

highlands <- kenya_zones |> filter(type == 2) |> st_union() |> st_make_valid()

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

settlements_utm <- st_filter(settlements_utm, highlands, .predicate = st_intersects)
settlements_utm$scheme_id <- settlements_utm$S_No

gap_tolerance <- 100
settlements_union <- settlements_utm |>
  st_buffer(dist = gap_tolerance) |>
  st_union() |>
  st_buffer(dist = -gap_tolerance) |>
  st_make_valid() |>
  st_buffer(0) |>
  nngeo::st_remove_holes() |>
  st_make_valid() |>
  st_buffer(0) |>
  st_simplify(dTolerance = 10) |>
  st_make_valid()

highlands_control_raw <- st_difference(highlands, settlements_union) |>
  st_make_valid() |>
  st_buffer(0)

master_tmf <- rast("data/EU_JRC_TMF/ForestCover_Kenya_1990_EUJRCTMF.tif")
tmf_extent_sf <- st_as_sfc(st_bbox(master_tmf)) |>
  st_as_sf() |>
  st_set_crs(st_crs(master_tmf)) |>
  st_transform(32737)

message("Creating Study Area")
study_area <- st_buffer(tmf_extent_sf, 9000) |>
  st_intersection(highlands) |>
  st_make_valid() |>
  st_buffer(0) |>
  vect()

master_utm <- terra::project(master_tmf, "EPSG:32737", method = "near")
master_30m <- crop(master_utm, study_area)
master_30m[is.na(master_30m)] <- 0


message("Removing small forest patches")
calc_threshold <- round(small_forest_area_threshold / 900)

forest_bin <- terra::ifel(master_30m > 0, 1, 0)
forest_sieved <- terra::sieve(forest_bin, threshold = calc_threshold, directions = 8)
forest_cleaned_rast <- terra::ifel(forest_sieved == 1, 1, NA)

message("Aggregating Forest for Fast Vectorization")
forest_90m <- terra::aggregate(forest_cleaned_rast, fact = 3, fun = "modal", na.rm = TRUE)

message("Creating Forest Mask")
forest_poly <- terra::as.polygons(forest_90m, dissolve = TRUE) |>
  st_as_sf() |>
  st_simplify(dTolerance = 15) |>
  st_make_valid()

message("Removing Forest Holes")
forest_no_holes_parts <- nngeo::st_remove_holes(forest_poly) |>
  st_make_valid() |>
  suppressWarnings(st_cast("POLYGON"))

forest_no_holes <- forest_no_holes_parts[as.numeric(st_area(forest_no_holes_parts)) >= 20000] |>
  st_union() |>
  st_make_valid()

message("Creating True Control Area")
highlands_control_parts <- st_difference(highlands_control_raw, forest_no_holes) |>
  st_make_valid() |>
  suppressWarnings(st_cast("POLYGON"))

highlands_control <- highlands_control_parts[as.numeric(st_area(highlands_control_parts)) >= 20000] |>
  st_union() |>
  st_make_valid()

message("Creating Grids")
grid_300m_raster <- aggregate(master_30m, fact = 10, fun = "max", na.rm = TRUE)
grid_1200m_raster <- aggregate(grid_300m_raster, fact = 4)
values(grid_1200m_raster) <- 1:ncell(grid_1200m_raster)
names(grid_1200m_raster) <- "cluster_1200m_id"

r_mask <- mask(grid_300m_raster, study_area)

grid_points <- as.points(r_mask) |>
  st_as_sf() |>
  select(-1)


grid_points$point_id <- 1:nrow(grid_points)
grid_points$cluster_1200m_id <- terra::extract(grid_1200m_raster, vect(grid_points)) |>
  pull(2)
grid_points <- st_transform(grid_points, 32737)
grid_points_vect <- vect(grid_points)

message("Creating Base Geometries")
ctrl_base <- st_simplify(highlands_control, dTolerance = 10) |>
  st_make_valid()
  
simple_settlements <- st_simplify(settlements_union, dTolerance = 10) |>
  st_make_valid()
simple_forest_edge <- st_boundary(forest_no_holes) |>
  st_make_valid() |>
  st_simplify(dTolerance = 10) |>
  st_make_valid()


message("Calculating Distances")
base_raster <- grid_300m_raster
values(base_raster) <- NA

r_settlements <- rasterize(vect(simple_settlements), base_raster, field = 1)
r_ctrl_base   <- rasterize(vect(ctrl_base), base_raster, field = 1)

r_forest_area <- rasterize(vect(forest_no_holes), base_raster, field = 1, background = 0)
grid_points$is_forest <- terra::extract(r_forest_area, grid_points_vect) |>
  pull(2)

message("Calculating Base Distances")
dist_to_scheme <- as.numeric(st_distance(grid_points, simple_settlements))
dist_ctrl_base <- as.numeric(st_distance(grid_points, ctrl_base))

grid_points$is_treated_base <- ifelse(dist_to_scheme < dist_ctrl_base, 1, 0)

boundary_settlements <- st_boundary(simple_settlements) |>
  st_make_valid()

border_base <- boundary_settlements |>
  st_intersection(st_boundary(ctrl_base)) |>
  st_make_valid()

dist_border_base <- as.numeric(st_distance(grid_points, border_base))

grid_points$dist_cutoff_base <- ifelse(grid_points$is_treated_base == 1, dist_border_base, -dist_border_base)

message("Calculating Multi-Level Slivers and Cutoffs")

for (d in buffer_dists) {
  d_str <- as.character(d)
  message(paste("Processing distance:", d_str, "meters"))

  ctrl_geom <- highlands_control |>
    st_buffer(-d) |>
    st_buffer(d) |>
    st_make_valid() |>
    st_simplify(dTolerance = 10) |>
    st_make_valid()

  r_ctrl <- rasterize(vect(ctrl_geom), base_raster, field = 1)
  dist_ctrl <- as.numeric(st_distance(grid_points, ctrl_geom))

  treated_col <- paste0("is_treated_", d_str)
  grid_points[[treated_col]] <- ifelse(dist_to_scheme < dist_ctrl, 1, 0)

  border_geom <- boundary_settlements |>
    st_intersection(st_boundary(ctrl_geom)) |>
    st_make_valid()

  dist_border <- as.numeric(st_distance(grid_points, border_geom))

  dist_col <- paste0("dist_cutoff_", d_str)
  grid_points[[dist_col]] <- ifelse(grid_points[[treated_col]] == 1, dist_border, -dist_border)
}

message("Calculating Forest Edge Distance")
r_forest_edge <- rasterize(vect(simple_forest_edge), base_raster, field = 1)
tmp_forest_dist <- as.numeric(st_distance(grid_points, simple_forest_edge))
grid_points$signed_dist_forest_edge <- ifelse(grid_points$is_forest == 1, -tmp_forest_dist, tmp_forest_dist)


message("Assigning Nearest Settlement Scheme")
nearest_scheme_idx <- st_nearest_feature(grid_points, settlements_utm)
grid_points$scheme_id <- settlements_utm$scheme_id[nearest_scheme_idx]
grid_points$is_known <- settlements_utm$is_known[nearest_scheme_idx]

coords <- st_coordinates(grid_points)
grid_df <- st_drop_geometry(grid_points)
grid_df$x <- coords[, "X"]
grid_df$y <- coords[, "Y"]

base_grid_path <- file.path(out_dir, paste0("base_grid.parquet"))
write_parquet(grid_df, base_grid_path)