grid_sf <- st_as_sf(grid_df, coords = c("x", "y"), crs = 32737)

raw_dem <- rast("data/SRTM/SRTM_Kenya.tif")
slope_raster <- terrain(raw_dem, v = "slope", unit = "degrees")
topo_raster <- c(raw_dem, slope_raster) |>
  setNames(c("elevation", "slope"))

topo_aligned <- project(topo_raster, grid_300m_raster, method = "average")

topo_values <- terra::extract(topo_aligned, vect(grid_sf))

srtm_data <- tibble(
  point_id = grid_df$point_id,
  elevation = topo_values$elevation,
  slope = topo_values$slope
) |>
  drop_na(elevation, slope)