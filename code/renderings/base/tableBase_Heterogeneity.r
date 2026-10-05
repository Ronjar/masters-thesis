library(arrow)
library(rdrobust)
library(dplyr)
library(sf)
library(kableExtra)

# ==============================================================================
# DATA
# ==============================================================================

path_ndvi_23_25 <- "data/Regressions/base/NDVI.parquet"
path_ndmi_23_25 <- "data/Regressions/base/NDMI.parquet"


# ==============================================================================
# PROCESSING
# ==============================================================================

extract_rd_stats <- function(mod, sample_mean) {
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

  c(sprintf("%.3f%s", coef * 100, stars),
    sprintf("(%.3f)", se * 100),
    sprintf("%.3f", sample_mean * 100),
    format(bw, big.mark = ","),
    format(eff_n, big.mark = ","))
}

prepare_heterogeneous_data <- function(data_path) {
  grid_df <- read_parquet(data_path)

  settlements_known_area <- st_read("data/SettlementSchemes/Settlement_Schemes.shp", quiet = TRUE) |>
    filter(S_No != 0) |>
    mutate(calc_area_sqm = as.numeric(st_area(geometry))) |>
    st_drop_geometry()

  settlements_known_area <- settlements_known_area |>
    mutate(
      regis_duration = as.numeric(Year_Regis) - as.numeric(Allocation),
      long_regis_diff = ifelse(is.na(regis_duration), NA_integer_,
                               ifelse(regis_duration > median(regis_duration, na.rm = TRUE), 1L, 0L)),
      area_sqkm = calc_area_sqm / 1000000,
      pop_density = ifelse(area_sqkm > 0, Number_Set / area_sqkm, NA_real_),
      is_high_density = ifelse(is.na(pop_density), NA_integer_,
                               ifelse(pop_density >= median(pop_density, na.rm = TRUE), 1L, 0L)),
      is_conventional = ifelse(is.na(Scheme_Typ), NA_integer_,
                              ifelse(Scheme_Typ == "Conventional", 1L, 0L))
    ) |>
    mutate(scheme_id = S_No) |>
    select(scheme_id, long_regis_diff, is_high_density, is_conventional) |>
    distinct(scheme_id, .keep_all = TRUE)

  final_analysis_data <- grid_df |>
    left_join(settlements_known_area, by = "scheme_id")
}

run_t2_heterogeneity <- function(prepared_data, y_var) {
  df_conv   <- prepared_data |> filter(is_conventional == 1)
  df_squat  <- prepared_data |> filter(is_conventional == 0)
  df_lowpop <- prepared_data |> filter(is_high_density == 0)
  df_hipop  <- prepared_data |> filter(is_high_density == 1)
  df_fast   <- prepared_data |> filter(long_regis_diff == 0)
  df_slow   <- prepared_data |> filter(long_regis_diff == 1)
  df_combi <- prepared_data |> filter(is_conventional == 1 & is_high_density == 0 & long_regis_diff == 1)

  run_sub <- function(sub_df) {
    mod <- rdrobust(
        y = sub_df[[y_var]],
        x = sub_df$signed_distance,
        c = 0,
        covs = cbind(sub_df$elevation, sub_df$slope, sub_df$dist_rail, sub_df$dist_city),
        cluster = sub_df$cluster_1200m_id
    )

    bw <- round(mod$bws[1, 1], 0)

    sample_mean <- mean(sub_df[[y_var]][abs(sub_df$signed_distance) <= bw], na.rm = TRUE)
    extract_rd_stats(mod, sample_mean)
  }

  mat <- cbind(
    run_sub(df_conv), run_sub(df_squat),
    run_sub(df_lowpop), run_sub(df_hipop),
    run_sub(df_fast), run_sub(df_slow),
    run_sub(df_combi)
  )
  rownames(mat) <- c("Treatment effect", "Robust std. error", "Subsample mean", "Bandwidth", "Observations")
  return(mat)
}

ndvi_prepared <- prepare_heterogeneous_data(path_ndvi_23_25)
ndmi_prepared <- prepare_heterogeneous_data(path_ndmi_23_25)

t2_panel_a <- run_t2_heterogeneity(ndvi_prepared, "NDVI")
t2_panel_b <- run_t2_heterogeneity(ndmi_prepared, "NDMI")


# ==============================================================================
# TABLE
# ==============================================================================

t2_meta <- rbind(
  c("Conventional", "Squatter", "Low density", "High density", "Fast", "Slow", linebreak("Conventional\nLow density\nSlow", align = "c"))
)
rownames(t2_meta) <- c("Analysis subsample")
t2_final <- rbind(t2_panel_a, t2_panel_b, t2_meta)
colnames(t2_final) <- c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)")

t2_latex <- kable(
    x = t2_final,
    format = "latex",
    booktabs = TRUE,
    align = "c",
    escape = FALSE) |>
  add_header_above(
    header = c(
        " " = 1,
        "Scheme type" = 2,
        "Population density" = 2,
        "Registration period" = 2,
        "Combined" = 1
    ),
    bold = TRUE) |>
  pack_rows(
    group_label = "Panel A: NDVI",
    start_row = 1,
    end_row = 5,
    bold = TRUE,
    italic = TRUE
  ) |>
  pack_rows(
    group_label = "Panel B: NDMI",
    start_row = 6,
    end_row = 10,
    bold = TRUE,
    italic = TRUE
  ) |>
  pack_rows(
    group_label = "Model specifications",
    start_row = 11,
    end_row = 11,
    bold = TRUE
  )

cat(t2_latex, file = "output/tables/tableBase_Heterogeneity.tex")