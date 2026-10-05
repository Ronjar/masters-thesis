library(arrow)
library(dplyr)
library(ggplot2)
source("code/renderings/rdd_plot.r")

data_forests <- read_parquet("data/Regressions/forests/Forest_analysis_data.parquet")

create_rdd_plot(
  data = data_forests,
  var_name = "y_deforestation",
  y_label = "Deforestation (%)",
  dist_var = "dist_cutoff_300",
  y_limits = c(0, 10),
  output_filename = "figureForests_Deforestation.png",
  text_scale = 1.2
)


create_rdd_plot(
  data = data_forests,
  var_name = "y_degradation",
  y_label = "Degradation (%)",
  dist_var = "dist_cutoff_300",
  y_limits = c(0, 10),
  output_filename = "figureForests_Degradation.png",
  text_scale = 1.2
)

create_rdd_plot(
  data = data_forests,
  var_name = "y_deforestation",
  y_label = "Deforestation (%)",
  dist_var = "dist_cutoff_300",
  h = 9000,
  y_limits = c(0, 10),
  output_filename = "figureForests_Deforestation9000m.png",
  text_scale = 1.2
)