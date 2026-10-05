library(arrow)
library(rdrobust)
library(dplyr)
library(kableExtra)


# ==============================================================================
# DATA
# ==============================================================================

path_ndvi_23_25 <- "data/Regressions/base/NDVI.parquet"
data_cbt <- read_parquet(path_ndvi_23_25)


# ==============================================================================
# PROCESSING
# ==============================================================================

extract_cbt_stats <- function(mod, y_var, signed_dist) {
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
  dep_mean <- mean(sample_in_bw, na.rm = TRUE)

  c(
    sprintf("%.3f%s", coef, stars),
    sprintf("(%.3f)", se),
    format(bw, big.mark = ","),
    format(eff_n, big.mark = ","),
    sprintf("%.3f", dep_mean)
  )
}

get_cbt_column <- function(h_val = NULL) {
  cov_elevation <- rdrobust(y = data_cbt$elevation, x = data_cbt$signed_distance, c = 0, h = h_val, cluster = data_cbt$cluster_1200m_id)
  cov_slope     <- rdrobust(y = data_cbt$slope,     x = data_cbt$signed_distance, c = 0, h = h_val, cluster = data_cbt$cluster_1200m_id)
  cov_rail      <- rdrobust(y = data_cbt$dist_rail, x = data_cbt$signed_distance, c = 0, h = h_val, cluster = data_cbt$cluster_1200m_id)
  cov_city      <- rdrobust(y = data_cbt$dist_city, x = data_cbt$signed_distance, c = 0, h = h_val, cluster = data_cbt$cluster_1200m_id)

  mat_elevation <- matrix(extract_cbt_stats(cov_elevation, data_cbt$elevation, data_cbt$signed_distance), ncol = 1)
  mat_slope     <- matrix(extract_cbt_stats(cov_slope,     data_cbt$slope,     data_cbt$signed_distance), ncol = 1)
  mat_rail      <- matrix(extract_cbt_stats(cov_rail,      data_cbt$dist_rail, data_cbt$signed_distance), ncol = 1)
  mat_city      <- matrix(extract_cbt_stats(cov_city,      data_cbt$dist_city, data_cbt$signed_distance), ncol = 1)

  rbind(mat_elevation, mat_slope, mat_rail, mat_city)
}

col_mse  <- get_cbt_column(h_val = NULL)
col_3000 <- get_cbt_column(h_val = 3000)
col_9000 <- get_cbt_column(h_val = 9000)

cbt_matrix <- cbind(col_mse, col_3000, col_9000)
rownames(cbt_matrix) <- c(
  "Treatment effect", "Robust std. error", "Bandwidth (m)", "Observations", "Mean",
  "Treatment effect", "Robust std. error", "Bandwidth (m)", "Observations", "Mean",
  "Treatment effect", "Robust std. error", "Bandwidth (m)", "Observations", "Mean",
  "Treatment effect", "Robust std. error", "Bandwidth (m)", "Observations", "Mean"
)
colnames(cbt_matrix) <- c("(1)", "(2)", "(3)")


# ==============================================================================
# TABLE
# ==============================================================================

cbt_meta <- matrix(c("MSE-optimal", "3,000m", "9,000m"), nrow = 1)
rownames(cbt_meta) <- "Bandwidth estimation"

cbt_final <- rbind(cbt_matrix, cbt_meta)

cbt_latex_table <- kable(
  x = cbt_final,
  format = "latex",
  booktabs = TRUE,
  align = "c",
  escape = FALSE
) |>
  pack_rows(group_label = "Panel A: Geographic elevation (m)", start_row = 1,  end_row = 5,  bold = TRUE, italic = TRUE) |>
  pack_rows(group_label = "Panel B: Terrain slope (degrees)",    start_row = 6,  end_row = 10, bold = TRUE, italic = TRUE) |>
  pack_rows(group_label = "Panel C: Distance to railroad (m)", start_row = 11, end_row = 15, bold = TRUE, italic = TRUE) |>
  pack_rows(group_label = "Panel D: Distance to city (m)",     start_row = 16, end_row = 20, bold = TRUE, italic = TRUE) |>
  pack_rows(group_label = "Model specifications",              start_row = 21, end_row = 21, bold = TRUE)

cat(cbt_latex_table, file = "output/tables/tableBase_CBT.tex")