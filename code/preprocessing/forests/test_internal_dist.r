library(arrow)
library(dplyr)
library(sf)

path_forest_data <- "data/Regressions/forests/Forest_analysis_data.parquet"
qgis_gpkg_output <- "data/Regressions/forests/Kenya_Forest_QGIS_MultiLevel_3000m.gpkg"

grid_df <- read_parquet(path_forest_data)

variants <- c("base", "150", "300", "450", "600", "900")

for (v in variants) {
  message(sprintf("Processing variant: %s", v))

  treated_col <- paste0("is_treated_", v)

  df_filtered <- grid_df |>
    filter(abs(dist_cutoff_base) <= 3000) |>
    mutate(
      type = case_when(
        is_forest == 0 & is_treated_base == 0 ~ "0",      # RDD Control
        is_forest == 0 & is_treated_base == 1 ~ "1",      # RDD Treatment
        is_forest == 1 & .data[[treated_col]] == 0 ~ "2", # Forest Control
        is_forest == 1 & .data[[treated_col]] == 1 ~ "3"  # Forest Treatment
      )
    ) |>
    filter(!is.na(type))

  cat(sprintf("RDD-Control: %.0f | RDD-Treatment: %.0f | Forest-Control: %.0f, Forest-Treatment: %.0f\n",
              count(filter(df_filtered, type == "0")),
              count(filter(df_filtered, type == "1")),
              count(filter(df_filtered, type == "2")),
              count(filter(df_filtered, type == "3"))))

  sf_points <- st_as_sf(df_filtered, coords = c("x", "y"), crs = 32737)

  sf_polygons <- sf_points |>
    mutate(geometry = st_buffer(geometry, dist = 150, endCapStyle = "SQUARE")) |>
    group_by(type) |>
    summarise(geometry = st_union(geometry), .groups = "drop") |>
    mutate(variant = v) |>
    st_make_valid()

  layer_name <- paste0("Variant_", v)
  st_write(sf_polygons, dsn = qgis_gpkg_output, layer = layer_name, delete_layer = TRUE, quiet = TRUE)
}