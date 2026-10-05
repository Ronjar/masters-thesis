library(arrow)
library(rdrobust)
library(dplyr)
library(kableExtra)


# ==============================================================================
# DATA
# ==============================================================================

path_ndvi_23_25 <- "data/Regressions/base/NDVI.parquet"
path_ndmi_23_25 <- "data/Regressions/base/NDMI.parquet"


# ==============================================================================
# PROCESSING
# ==============================================================================

extract_rd_stats <- function(mod, y_var, signed_dist) {
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

  sample_in_bw <- y_var[abs(signed_dist) <= bw]
  sample_mean <- mean(sample_in_bw, na.rm = TRUE)

  c(sprintf("%.3f%s", coef * 100, stars),
    sprintf("(%.3f)", se * 100),
    sprintf("%.3f", sample_mean * 100),
    format(bw, big.mark = ","),
    format(eff_n, big.mark = ","))
}

run_t1_baseline <- function(data_path, y_var) {
  analysis_data <- read_parquet(data_path)
  y_full <- analysis_data[[y_var]]
  cov_full <- cbind(analysis_data$elevation, analysis_data$slope, analysis_data$dist_rail, analysis_data$dist_city)

  mod_basic <- rdrobust(
    y = y_full,
    x = analysis_data$signed_distance,
    c = 0,
    cluster = analysis_data$cluster_1200m_id
  )

  mod_covs  <- rdrobust(
    y = y_full,
    x = analysis_data$signed_distance,
    c = 0,
    covs = cov_full,
    cluster = analysis_data$cluster_1200m_id
  )

  donut_data <- analysis_data |> filter(abs(signed_distance) > 300)
  mod_donut  <- rdrobust(
    y = donut_data[[y_var]],
    x = donut_data$signed_distance,
    c = 0,
    covs = cbind(donut_data$elevation, donut_data$slope, donut_data$dist_rail, donut_data$dist_city),
    cluster = donut_data$cluster_1200m_id
  )

  known_data <- analysis_data |> filter(is_known == 1)
  cov_known <- cbind(known_data$elevation, known_data$slope, known_data$dist_rail, known_data$dist_city)
  mod_known  <- rdrobust(
    y = known_data[[y_var]],
    x = known_data$signed_distance,
    c = 0,
    covs = cov_known,
    cluster = known_data$cluster_1200m_id
  )

  mod_mse   <- rdrobust(
    y = y_full,
    x = analysis_data$signed_distance,
    c = 0, h = 3000,
    covs = cov_full,
    cluster = analysis_data$cluster_1200m_id
  )

  mod_wide  <- rdrobust(
    y = y_full,
    x = analysis_data$signed_distance,
    c = 0, h = 9000,
    covs = cov_full,
    cluster = analysis_data$cluster_1200m_id
  )

  mat <- cbind(
    extract_rd_stats(mod_basic, y_full, analysis_data$signed_distance),
    extract_rd_stats(mod_covs, y_full, analysis_data$signed_distance),
    extract_rd_stats(mod_donut, donut_data[[y_var]], donut_data$signed_distance),
    extract_rd_stats(mod_mse, y_full, analysis_data$signed_distance),
    extract_rd_stats(mod_wide, y_full, analysis_data$signed_distance),
    extract_rd_stats(mod_known, known_data[[y_var]], known_data$signed_distance)
  )
  rownames(mat) <- c("Treatment effect", "Robust std. error", "Sample Mean", "Bandwidth (m)", "Observations")
  return(mat)
}

t1_panel_a <- run_t1_baseline(path_ndvi_23_25, "NDVI")
t1_panel_b <- run_t1_baseline(path_ndmi_23_25, "NDMI")


# ==============================================================================
# TABLE
# ==============================================================================

t1_meta <- rbind(
  c(" ", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
  c(" ", " ", "\\checkmark (300m)", " ", " ", " "),
  c(" ", " ", " ", " ", " ", "\\checkmark"),
  c("MSE-optimal", "MSE-optimal", "MSE-optimal", "3000", "9000", "MSE-optimal")
)
rownames(t1_meta) <- c("Covariates", "Donut RDD exclusion", "Exclude unknown area", "Bandwidth estimation")
t1_final <- rbind(t1_panel_a, t1_panel_b, t1_meta)
colnames(t1_final) <- c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)")

t1_latex <- kable(t1_final, format = "latex", booktabs = TRUE, align = "c", escape = FALSE) |>
  pack_rows("Panel A: NDVI", 1, 5, bold = TRUE, italic = TRUE) |>
  pack_rows("Panel B: NDMI", 6, 10, bold = TRUE, italic = TRUE) |>
  pack_rows("Model specifications", 11, 14, bold = TRUE)

cat(t1_latex, file = "output/tables/tableBase_Baseline.tex")