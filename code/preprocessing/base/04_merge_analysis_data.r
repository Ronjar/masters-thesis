analysis_data <- grid_df |>
  inner_join(srtm_data, by = "point_id") |>
  inner_join(y_data, by = "point_id")

final_filename <- paste0(Y_VAR, ".parquet")
final_filepath <- file.path(out_dir, final_filename)

write_parquet(analysis_data, final_filepath)