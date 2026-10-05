library(arrow)
library(rdrobust)
library(dplyr)
library(sf)
library(kableExtra)

path_forest_data <- "data/Regressions/forests/20000/Forest_analysis_data.parquet"

prepare_forest_data <- function(data_path) {
  grid_df <- read_parquet(data_path)

  settlements_known_area <- st_read("data/SettlementSchemes/Settlement_Schemes.shp", quiet = TRUE) |>
    mutate(calc_area_sqm = as.numeric(st_area(geometry))) |>
    st_drop_geometry()

  unknown_settlements <- st_read("data/SettlementSchemes/UNKNOWN_schemes_pnts.shp", quiet = TRUE) |>
    mutate(calc_area_sqm = Scheme_Siz * 10000) |>
    st_drop_geometry()

  common_cols <- intersect(names(settlements_known_area), names(unknown_settlements))
  meta_schemes <- rbind(settlements_known_area[, common_cols]) |>
    filter(S_No != 0)

  meta_schemes <- meta_schemes |>
    mutate(
      area_sqkm = calc_area_sqm / 1000000,
      pop_density = ifelse(area_sqkm > 0, Number_Set / area_sqkm, NA_real_),
      is_high_density = ifelse(is.na(pop_density), NA_integer_,
                               ifelse(pop_density >= median(pop_density, na.rm = TRUE), 1L, 0L)),
      is_conventional = ifelse(is.na(Scheme_Typ), NA_integer_,
                              ifelse(Scheme_Typ == "Conventional", 1L, 0L))
    ) |>
    mutate(scheme_id = S_No) |>
    select(scheme_id, is_high_density, is_conventional) |>
    distinct(scheme_id, .keep_all = TRUE)

  final_analysis_data <- grid_df |>
    left_join(meta_schemes, by = "scheme_id") |>
    filter(is_forest == 0)

  final_analysis_data
}

analysis_data <- prepare_forest_data(path_forest_data)

extract_cbt_stats <- function(mod, y_var, x_var) {
  if (is.null(mod)) return(c("-", "-", "-", "-"))

  coef <- mod$coef[1, 1]
  se <- mod$se[3, 1]
  pval <- mod$pv[3, 1]
  bw <- round(mod$bws[1, 1], 0)

  cat(sprintf("Coef: %7.3f | P-Value: %.5f\n", coef, pval))

  stars <- ""
  if (pval < 0.01) stars <- "***"
  else if (pval < 0.05) stars <- "**"
  else if (pval < 0.1) stars <- "*"

  eff_n <- mod$N_h[1] + mod$N_h[2]

  sample_in_bw <- y_var[abs(x_var) <= bw]
  dep_mean <- mean(sample_in_bw, na.rm = TRUE)

  c(
    sprintf("%.3f%s", coef, stars),
    sprintf("(%.3f)", se),
    format(bw, big.mark = ","),
    format(eff_n, big.mark = ","),
    sprintf("%.3f", dep_mean)
  )
}

get_cbt_column <- function(data, x_col) {

  run_mod <- function(y_col) {
    if (!(y_col %in% names(data))) return(NULL)
    valid_data <- data |> filter(!is.na(.data[[y_col]]), !is.na(.data[[x_col]]))
    if (nrow(valid_data) < 50) return(NULL)

    tryCatch({
      rdrobust(
        y = valid_data[[y_col]],
        x = valid_data[[x_col]],
        c = 0,
        cluster = valid_data$scheme_id
      )
    }, error = function(e) NULL)
  }

  mod_edge <- run_mod("signed_dist_forest_edge")
  mod_elev <- run_mod("elevation")
  mod_slop <- run_mod("slope")
  mod_rail <- run_mod("dist_rail")
  mod_city <- run_mod("dist_city")

  mat_edge <- matrix(extract_cbt_stats(mod_edge, data$signed_dist_forest_edge, data[[x_col]]), ncol = 1)
  mat_elev <- matrix(extract_cbt_stats(mod_elev, data$elevation, data[[x_col]]), ncol = 1)
  mat_slop <- matrix(extract_cbt_stats(mod_slop, data$slope, data[[x_col]]), ncol = 1)
  mat_rail <- matrix(extract_cbt_stats(mod_rail, data$dist_rail, data[[x_col]]), ncol = 1)
  mat_city <- matrix(extract_cbt_stats(mod_city, data$dist_city, data[[x_col]]), ncol = 1)

  rbind(mat_edge, mat_elev, mat_slop, mat_rail, mat_city)
}

distances <- c("base", "150", "300", "450", "600")
cols <- list()

for (d in distances) {
  message("Testing covariates for distance specification: ", d)
  x_col <- paste0("dist_cutoff_", d)
  cols[[d]] <- get_cbt_column(analysis_data, x_col)
}

cbt_matrix <- do.call(cbind, cols)

row_names_block <- c("Treatment effects", "std. error", "Bandwidth", "Observations", "Mean")
rownames(cbt_matrix) <- rep(row_names_block, 5)
colnames(cbt_matrix) <- c("base", "150m", "300m", "450m", "600m")

cbt_meta <- matrix(c("Base", "150m", "300m", "450m", "600m"), nrow = 1)
rownames(cbt_meta) <- "Exclusion"

cbt_final <- rbind(cbt_matrix, cbt_meta)

cbt_latex_table <- kable(
  x = cbt_matrix,
  format = "latex",
  booktabs = TRUE,
  align = "c",
  escape = FALSE
) |>
  pack_rows(group_label = "Panel A: Distance to forest edge (m)", start_row = 1,  end_row = 5,  bold = TRUE, italic = TRUE) |>
  pack_rows(group_label = "Panel B: Geographic elevation (m)",    start_row = 6,  end_row = 10,  bold = TRUE, italic = TRUE) |>
  pack_rows(group_label = "Panel C: Terrain slope (degrees)",     start_row = 11,  end_row = 15, bold = TRUE, italic = TRUE) |>
  pack_rows(group_label = "Panel D: Distance to railroad (m)",    start_row = 16, end_row = 20, bold = TRUE, italic = TRUE) |>
  pack_rows(group_label = "Panel E: Distance to city (m)",        start_row = 21, end_row = 25, bold = TRUE, italic = TRUE)

cat(cbt_latex_table, file = "output/tables/tableForests_CBT.tex")