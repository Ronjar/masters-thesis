grid_sf <- st_as_sf(grid_df, coords = c("x", "y"), crs = 32737)

cities <- st_read("data/Jedwab_Moradi/Cities/cities_1890_and_1900.TAB") |>
  st_transform(32737) |>
  st_make_valid()

railroads <- st_read("data/Jedwab_Moradi/Railroads/Built/Railroads.TAB") |>
  st_transform(32737) |>
  st_make_valid()


nearest_city_idx <- st_nearest_feature(grid_sf, cities)
dist_city <- as.numeric(st_distance(grid_sf, cities[nearest_city_idx, ], by_element = TRUE))

nearest_rail_idx <- st_nearest_feature(grid_sf, railroads)
dist_rail <- as.numeric(st_distance(grid_sf, railroads[nearest_rail_idx, ], by_element = TRUE))

raw_dem <- rast("data/SRTM/SRTM_Kenya.tif")
slope_raster <- terrain(raw_dem, v = "slope", unit = "degrees")
topo_raster <- c(raw_dem, slope_raster) |>
  setNames(c("elevation", "slope"))

topo_aligned <- project(topo_raster, grid_300m_raster, method = "average")

topo_values <- terra::extract(topo_aligned, vect(grid_sf))

cov_data <- tibble(
  point_id = grid_df$point_id,
  elevation = topo_values$elevation,
  slope = topo_values$slope,
  dist_city = dist_city,
  dist_rail = dist_rail
) |>
  drop_na(elevation, slope)