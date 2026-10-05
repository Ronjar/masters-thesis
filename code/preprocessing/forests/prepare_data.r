library(sf)
library(terra)
library(arrow)
library(dplyr)
library(tidyr)
library(nngeo)

out_dir <- "data/Regressions/forests"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
terraOptions(tempdir = "~/temp")

#=============================================================================
# SETTINGS
#=============================================================================

buffer_dists <- c(150, 300, 450, 600)
small_forest_area_threshold <- 20000

message("Starting!")
source("code/preprocessing/forests/01_create_base_grid.r")
message("Step 1 (Base Grid) completed")
source("code/preprocessing/forests/02_extract_covs.r")
message("Step 2 (Extracting Cov Data) completed")
source("code/preprocessing/forests/03_extract_tmf.r")
message("Step 3 (Extracting TMF Data) completed")
source("code/preprocessing/forests/04_merge_analysis_data.r")
message("Step 4 (Merging data) completed")

message("Finished!")