
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

cities <- st_read("data/Jedwab_Moradi/Cities/cities_1890_and_1900.TAB") |>
  st_transform(32737) |>
  st_make_valid()

railroads <- st_read("data/Jedwab_Moradi/Railroads/Built/Railroads.TAB") |>
  st_transform(32737) |>
  st_make_valid()


#=============================================================================
# PROCESSING
#=============================================================================

forests <- kenya_zones |> filter(type == 1)
highlands <- kenya_zones |> filter(type == 2) |> st_union()

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

settlements_utm <- st_cast(settlements_utm, "MULTIPOLYGON")

# Create the outer border of multiple settlements
gap_tolerance <- 100
settlements_union <- settlements_utm |>
  st_buffer(dist = gap_tolerance) |>
  st_cast("MULTIPOLYGON") |>
  st_union() |>
  st_buffer(dist = -gap_tolerance)

settlements_super <- settlements_union |>
  nngeo::st_remove_holes()

settlements_borders <- settlements_super |>
  st_cast("MULTILINESTRING")


forests_union <- st_union(forests)
study_area <- st_buffer(settlements_borders, 9000) |>
  st_intersection(highlands) |>
  st_difference(forests_union) |>
  st_make_valid() |>
  st_union(settlements_super) |>
  vect()


master_files <- list.files(path = "data/NDVI/2023_2025/", pattern = "\\.tif$", full.names = TRUE)

master_raw <- lapply(master_files, rast) |>
  sprc() |>
  mosaic()

study_area_master_crs <- terra::project(study_area, crs(master_raw))

master_30m <- crop(master_raw, study_area_master_crs)

grid_300m_raster <- aggregate(master_30m, fact = 10)
grid_1200m_raster <- aggregate(grid_300m_raster, fact = 4)
values(grid_1200m_raster) <- 1:ncell(grid_1200m_raster)
names(grid_1200m_raster) <- "cluster_1200m_id"

r_mask <- mask(grid_300m_raster, study_area_master_crs)

grid_points <- as.points(r_mask) |>
  st_as_sf() |>
  select(-1)

grid_points$point_id <- 1:nrow(grid_points)
grid_points$cluster_1200m_id <- terra::extract(grid_1200m_raster, vect(grid_points))[, 2]
grid_points <- st_transform(grid_points, 32737)


grid_points$dist_abs <- as.numeric(st_distance(grid_points, settlements_borders))
grid_points$is_treated <- lengths(st_intersects(grid_points, settlements_union)) > 0

# Remove points that are inside the super settlements polygon but not a settlement
in_super_polygon <- lengths(st_intersects(grid_points, settlements_super)) > 0
grid_points$is_hole <- in_super_polygon & !grid_points$is_treated
grid_points <- grid_points |>
  filter(!is_hole)

grid_points$signed_distance <- ifelse(grid_points$is_treated, grid_points$dist_abs, -grid_points$dist_abs)

nearest_indices <- st_nearest_feature(grid_points, settlements_utm)
grid_points$scheme_id <- settlements_utm$scheme_id[nearest_indices]
grid_points$is_known   <- settlements_utm$is_known[nearest_indices]


nearest_forest_idx <- st_nearest_feature(grid_points, forests)
grid_points$dist_forest <- as.numeric(st_distance(grid_points, forests[nearest_forest_idx, ], by_element = TRUE))

nearest_city_idx <- st_nearest_feature(grid_points, cities)
grid_points$dist_city <- as.numeric(st_distance(grid_points, cities[nearest_city_idx, ], by_element = TRUE))

nearest_rail_idx <- st_nearest_feature(grid_points, railroads)
grid_points$dist_rail <- as.numeric(st_distance(grid_points, railroads[nearest_rail_idx, ], by_element = TRUE))


coords <- st_coordinates(grid_points)
grid_df <- st_drop_geometry(grid_points)
grid_df$x <- coords[, "X"]
grid_df$y <- coords[, "Y"]

base_grid_path <- file.path(out_dir, "base_grid.parquet")
write_parquet(grid_df, base_grid_path)