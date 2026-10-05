library(sf)
library(terra)
library(arrow)
library(dplyr)
library(tidyr)
library(nngeo)

out_dir <- "data/Regressions/base"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
terraOptions(tempdir = "~/temp")

message("Starting!")
source("code/preprocessing/base/01_create_base_grid.r")

runs <- list(
  list(Y_VAR = "NDVI", Y_PATH = "data/NDVI/2023_2025"),
  list(Y_VAR = "NDMI", Y_PATH = "data/NDMI/2023_2025")
)

for (run in runs) {
  Y_VAR  <- run$Y_VAR
  Y_PATH <- run$Y_PATH

  message(sprintf("Step %s", Y_VAR))

  base_grid_path <- file.path(out_dir, "base_grid.parquet")
  grid_df <- read_parquet(base_grid_path)

  source("code/preprocessing/base/02_extract_topography.r")
  source("code/preprocessing/base/03_extract_y.r")
  source("code/preprocessing/base/04_merge_analysis_data.r")
}

message("Finished!")