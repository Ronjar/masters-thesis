library(arrow)
library(dplyr)
library(ggplot2)
source("code/renderings/rdd_plot.r")

data_ndvi <- read_parquet("data/Regressions/base/NDVI.parquet")

create_rdd_plot(
  data = data_ndvi,
  var_name = "elevation",
  y_label = "Elevation (meters above sea level)",
  y_limits = c(1900, 2200),
  output_filename = "figureBase_CBT_Elevation.png",
  text_scale = 1.5
)

create_rdd_plot(
  data = data_ndvi,
  var_name = "slope",
  y_label = "Slope (degrees)",
  y_limits = c(0, 10),
  output_filename = "figureBase_CBT_Slope.png",
  text_scale = 1.5
)