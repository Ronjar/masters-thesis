library(arrow)
library(dplyr)
library(ggplot2)
library(sf)
source("code/renderings/rdd_plot.r")


data_ndvi <- read_parquet("data/Regressions/base/NDVI.parquet")
data_ndmi <- read_parquet("data/Regressions/base/NDMI.parquet")

create_rdd_plot(
  data = data_ndvi,
  var_name = "NDVI",
  y_label = "NDVI (2023-2025)",
  y_limits = c(0.5, 0.7),
  output_filename = "figureBase_NDVI.png",
  text_scale = 1.5
)

create_rdd_plot(
  data = data_ndmi,
  var_name = "NDMI",
  y_label = "NDMI (2023-2025)",
  y_limits = c(0.05, 0.2),
  output_filename = "figureBase_NDMI.png",
  text_scale = 1.5
)


#=============================================================================
# Heterogeneity
#=============================================================================

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
heterogeneity_analysis_data <- data_ndmi |>
  left_join(meta_schemes, by = "scheme_id")

data_ndmi_hcombi <- heterogeneity_analysis_data |>
  filter(is_conventional == 1 & is_high_density == 0 & long_regis_diff == 1)


create_rdd_plot(
  data = data_ndmi_hcombi,
  var_name = "NDMI",
  y_label = "NDMI (2023-2025)",
  h = 1200,
  y_limits = c(0.0, 0.3),
  output_filename = "figureBase_NDMI_HCOMBI.png",
  text_scale = 1.2
)