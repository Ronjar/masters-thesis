library(sf)
library(terra)
library(dplyr)

terraOptions(memfrac = 0.4, tempdir = "~/temp")

# ==============================================================================
# 1. DATA
# ==============================================================================

gpkg_path <- "data/KenyaPoliticalGeneral/KenyaPoliticalGeneral1957.gpkg"
tmf_dir   <- "data/EU_JRC_TMF"

kenya_zones <- st_read(gpkg_path, layer = "KenyaPoliticalGeneral", quiet = TRUE) |>
  st_transform(32737) |>
  st_make_valid()

highlands <- kenya_zones |>
  filter(type == 1) |>
  st_union() |>
  st_make_valid()

r_1990 <- rast(file.path(tmf_dir, "ForestCover_Kenya_1990_EUJRCTMF.tif"))
r_degr <- rast(file.path(tmf_dir, "Degraded_Kenya_2025_EUJRCTMF.tif"))
r_defo <- rast(file.path(tmf_dir, "Deforested_Kenya_2025_EUJRCTMF.tif"))

r_stack <- c(r_1990, r_degr, r_defo)
names(r_stack) <- c("f1990", "degrad", "deforest")

highlands_vect <- project(vect(highlands), crs(r_stack))


# ==============================================================================
# 2. PROCESSING
# ==============================================================================

r_crop <- crop(r_stack, highlands_vect) |>
  mask(highlands_vect) |>
  freq()


pix_total <- freq_table |>
  filter(layer == 1) |>
  pull(count) |>
  sum()

pix_1990   <- freq_table |> filter(layer == 1, value > 0) |> pull(count) |> sum()
pix_degrad <- freq_table |> filter(layer == 2, value > 0) |> pull(count) |> sum()
pix_defore <- freq_table |> filter(layer == 3, value > 0) |> pull(count) |> sum()

pix_heute  <- pix_1990 - pix_defore - pix_degrad

# ==============================================================================
# 3. PRINT
# ==============================================================================
to_mha <- function(p) { (p * 0.09) / 1000000 }
to_pct <- function(p) { (p / pix_1990) * 100 }

cat("\n==============================================\n")
cat(sprintf("Highlands:                  %1.3f Mha\n\n", to_mha(pix_total)))
cat(sprintf("Forest 1990:                %1.3f Mha (%2.1f%%)\n", to_mha(pix_1990), to_pct(pix_1990)))
cat(sprintf("Forest Undisturbed (2025):  %1.3f Mha (%2.1f%%)\n", to_mha(pix_heute), to_pct(pix_heute)))
cat(sprintf("Forest Degraded:            %1.3f Mha (%2.1f%%)\n", to_mha(pix_degrad), to_pct(pix_degrad)))
cat(sprintf("Deforestation:              %1.3f Mha (%2.1f%%)\n", to_mha(pix_defore), to_pct(pix_defore)))
cat("==============================================\n")