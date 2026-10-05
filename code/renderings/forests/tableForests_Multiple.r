library(arrow)
library(rdrobust)
library(dplyr)
library(sf)
library(kableExtra)

path_forest_data <- "data/Regressions/forests/Forest_analysis_data.parquet"

extract_rd_stats <- function(mod, sample_mean) {
  coef <- mod$coef[1, 1]
  se <- mod$se[3, 1]
  pval <- mod$pv[3, 1]

  cat(sprintf("Coef: %7.3f | P-Value: %.5f\n", coef, pval))

  stars <- ""
  if (pval < 0.01) stars <- "***"
  else if (pval < 0.05) stars <- "**"
  else if (pval < 0.1) stars <- "*"

  bw <- round(mod$bws[1, 1], 0)
  eff_n <- mod$N_h[1] + mod$N_h[2]

  c(sprintf("%.3f%s", coef, stars),
    sprintf("(%.3f)", se),
    sprintf("%.3f", sample_mean),
    format(bw, big.mark = ","),
    format(eff_n, big.mark = ","))
}

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
    filter(is_forest == 1)

  final_analysis_data
}

analysis_data <- prepare_forest_data(path_forest_data)

run_forest_regressions <- function(data, y_var, x_col) {

  run_rd <- function(df, h_val = NULL, covs = cbind(df$elevation, df$slope, df$dist_rail, df$dist_city)) {
    if (nrow(df) == 0) return(c("-", "-", "-", "-", "-"))

    if (is.null(h_val)) {
        mod <- rdrobust(
            y = df[[y_var]],
            x = df[[x_col]],
            c = 0,
            covs = covs,
            cluster = df$cluster_1200m_id
        )
        bw_l <- mod$bws[1, 1]
        bw_r <- mod$bws[1, 2]
    } else {
        mod <- rdrobust(
            y = df[[y_var]],
            x = df[[x_col]],
            c = 0,
            h = h_val,
            covs = covs,
            cluster = df$cluster_1200m_id
        )
        bw_l <- h_val
        bw_r <- h_val
    }

    sample_mean <- mean(df[[y_var]][df[[x_col]] >= -bw_l & df[[x_col]] <= bw_r], na.rm = TRUE)
    extract_rd_stats(mod, sample_mean)
  }

  donut_data   <- data |> filter(abs(.data[[x_col]]) > 300)
  hd_data      <- data |> filter(is_high_density == 1)
  ld_data      <- data |> filter(is_high_density == 0)
  known_data   <- data |> filter(is_known == 1)

  mod_basic  <- run_rd(data, h_val = NULL)
  mod_covs  <- run_rd(data, h_val = NULL, covs = cbind(data$elevation, data$slope, data$dist_rail, data$dist_city, data$signed_dist_forest_edge))
  mod_donut <- run_rd(donut_data, h_val = NULL)
  mod_mse   <- run_rd(data, h_val = 3000)
  mod_wide  <- run_rd(data, h_val = 9000)
  mod_hd    <- run_rd(hd_data, h_val = NULL)
  mod_ld    <- run_rd(ld_data, h_val = NULL)
  mod_known <- run_rd(known_data, h_val = NULL)

  mat <- cbind(
    mod_basic, mod_covs, mod_donut,
    mod_mse, mod_wide,
    mod_hd, mod_ld, mod_known
  )
  rownames(mat) <- c("Treatment effect", "Robust std. error", "Subsample mean", "Bandwidth (m)", "Observations")

  return(mat)
}

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
distances <- c("base", "150", "300", "450", "600")

for (dist_name in distances) {
  message("Running analysis for distance specification: ", dist_name)
  x_var <- paste0("dist_cutoff_", dist_name)

  panel_a <- run_forest_regressions(analysis_data, "y_deforestation", x_var)
  panel_b <- run_forest_regressions(analysis_data, "y_degradation", x_var)

  t_meta <- rbind(
    c(" ", "\\checkmark", " ", " ", " ", " ", " ", " "),
    c(" ", " ", "\\checkmark (300m)", " ", " ", " ", " ", " "),
    c("MSE-optimal", "MSE-optimal", "MSE-optimal", "3000", "9000", "MSE-optimal", "MSE-optimal", "MSE-optimal"),
    c("Full", "Full", "Full", "Full", "Full", "High density", "Low density", "Known only")
  )
  rownames(t_meta) <- c("Forest edge covariate", "Donut exclusion", "Bandwidth estimation", "Analysis sample")

  t_final <- rbind(panel_a, panel_b, t_meta)
  colnames(t_final) <- c(
    "(1)", "(2)", "(3)",
    "(4)", "(5)",
    "(6)", "(7)", "(8)"
  )

  t_latex <- kable(t_final, format = "latex", booktabs = TRUE, align = "c", escape = FALSE) |>
    add_header_above(
      header = c(
          " " = 1,
          "Base" = 3,
          "Bandwidth" = 2,
          "Sub-Samples" = 3
      ),
      bold = TRUE
    ) |>
    pack_rows("Panel A: Deforestation (%)", 1, 5, bold = TRUE, italic = TRUE) |>
    pack_rows("Panel B: Degradation (%)", 6, 10, bold = TRUE, italic = TRUE) |>
    pack_rows("Model specifications", 11, 14, bold = TRUE)

  out_file <- sprintf("output/tables/forests/tableForests_%s.tex", dist_name)
  cat(t_latex, file = out_file)
}