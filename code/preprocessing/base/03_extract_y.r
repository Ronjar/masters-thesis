grid_sf <- st_as_sf(grid_df, coords = c("x", "y"), crs = 32737)

y_files <- list.files(
  path = Y_PATH,
  pattern = "\\.tif$",
  full.names = TRUE
)

y_raw <- lapply(y_files, rast) |>
  sprc() |>
  mosaic()
y_aligned <- project(y_raw, grid_300m_raster, method = "average")

y_extracted <- terra::extract(y_aligned, vect(grid_sf))

y_data <- tibble(
  point_id = grid_df$point_id
)
y_data[[Y_VAR]] <- y_extracted[, 2]

y_data <- y_data |> drop_na(all_of(Y_VAR))