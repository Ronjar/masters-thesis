analysis_data <- grid_df |>
  inner_join(cov_data, by = "point_id") |>
  inner_join(y_data, by = "point_id") |>
  select(
    point_id,
    cluster_1200m_id,
    scheme_id,
    is_known,
    x,
    y,
    is_forest,
    signed_dist_forest_edge,
    starts_with("is_treated_"),
    starts_with("dist_cutoff_"),
    y_deforestation,
    y_degradation,
    elevation,
    slope,
    dist_city,
    dist_rail
  )

final_filepath <- file.path(out_dir, "Forest_analysis_data.parquet")
write_parquet(analysis_data, final_filepath)


#qgis_gpkg_path <- file.path(out_dir, "Kenya_Forest_QGIS_Visualisierung.gpkg")
#
#analysis_sf <- st_as_sf(analysis_data, coords = c("x", "y"), crs = 32737)
#
#st_write(analysis_sf, dsn = qgis_gpkg_path, layer = "01_Analysis_Points_TMF", delete_layer = TRUE, quiet = TRUE)
#st_write(highlands_control, dsn = qgis_gpkg_path, layer = "02_True_Control_Area", delete_layer = TRUE, quiet = TRUE)
#st_write(forest_no_holes, dsn = qgis_gpkg_path, layer = "03_Forest_Mask", delete_layer = TRUE, quiet = TRUE)
#st_write(settlements_union, dsn = qgis_gpkg_path, layer = "04_Settlement_Schemes_Union", delete_layer = TRUE, quiet = TRUE)