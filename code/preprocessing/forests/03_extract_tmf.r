grid_sf <- st_as_sf(grid_df, coords = c("x", "y"), crs = 32737) |>
  vect()

deforestation_rast <- rast("data/EU_JRC_TMF/Deforested_Kenya_2025_EUJRCTMF.tif")
degradation_rast <- rast("data/EU_JRC_TMF/Degraded_Frequency_Kenya_1990_2025_EUJRCTMF.tif")

deforestation_utm <- terra::project(deforestation_rast, "EPSG:32737", method = "near")
degradation_utm <- terra::project(degradation_rast, "EPSG:32737", method = "near")

deforestation_utm[is.na(deforestation_utm)] <- 0
degradation_utm[is.na(degradation_utm)] <- 0

deforestation_30m <- crop(deforestation_utm, study_area)
degradation_30m <- crop(degradation_utm, study_area)

deforestation_300m <- aggregate(deforestation_30m, fact = 10, fun = "sum", na.rm = TRUE)
degradation_300m   <- aggregate(degradation_30m,   fact = 10, fun = "sum", na.rm = TRUE)

y_deforestation_extracted <- terra::extract(deforestation_300m, grid_sf)
y_degradation_extracted   <- terra::extract(degradation_300m, grid_sf)


y_data <- tibble(
  point_id = grid_df$point_id,
  y_deforestation = y_deforestation_extracted[, 2],
  y_degradation   = y_degradation_extracted[, 2] / 35
)

y_data <- y_data |>
  drop_na(y_deforestation, y_degradation)