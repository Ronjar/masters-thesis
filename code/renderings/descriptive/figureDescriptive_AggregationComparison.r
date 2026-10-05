library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(patchwork)
library(dplyr)
library(ggnewscale)
source("code/renderings/base_theme.r")

terraOptions(tempdir = "~/temp")


# ==============================================================================
# DATA
# ==============================================================================

base_dir <- "data/NDVI"

paths_84_86 <- list.files(file.path(base_dir, "1984_1986"), pattern = "\\.tif$", full.names = TRUE)
paths_85    <- list.files(file.path(base_dir, "1985"), pattern = "\\.tif$", full.names = TRUE)
paths_85_03 <- list.files(file.path(base_dir, "1985_03"), pattern = "\\.tif$", full.names = TRUE)

load_ndvi <- function(paths) {
  r_list <- lapply(paths, rast)
  if (length(r_list) > 1) {
    mosaic(r_list[[1]], r_list[[2]], fun = "mean")
  } else {
    r_list[[1]]
  }
}

ndvi_84_86 <- load_ndvi(paths_84_86)
ndvi_85    <- load_ndvi(paths_85)
ndvi_85_03 <- load_ndvi(paths_85_03)

target_crs <- crs(ndvi_84_86)

kenya_outline <- st_read("data/Administrative/Kenya_Shape.geojson", quiet = TRUE) |> 
  st_transform(target_crs)

county_borders <- st_read("data/Administrative/KenyaCounties_Shape.geojson", quiet = TRUE) |> 
  st_transform(target_crs)

lake_victoria <- county_borders |> filter(ADM2_NAME == "Administrative unit not available")
lake_vect <- vect(lake_victoria)


# ==============================================================================
# PROCESSING
# ==============================================================================

print_stats <- function(r, name) {
  ndvi_count  <- terra::global(r >= -1, "sum", na.rm = TRUE)[1, 1]
  cloud_count <- terra::global(r == -2, "sum", na.rm = TRUE)[1, 1]
  miss_count  <- terra::global(r == -3, "sum", na.rm = TRUE)[1, 1]

  ndvi_count  <- ifelse(is.na(ndvi_count), 0, ndvi_count)
  cloud_count <- ifelse(is.na(cloud_count), 0, cloud_count)
  miss_count  <- ifelse(is.na(miss_count), 0, miss_count)

  total_count <- ndvi_count + cloud_count + miss_count

  cat(sprintf("\n=== Stats für: %s ===\n", name))
  cat(sprintf("Overall pixels for Kenya : %d\n", total_count))
  if(total_count > 0) {
    cat(sprintf("NDVI Pixel        : %12d (%5.2f%%)\n", ndvi_count,  (ndvi_count / total_count) * 100))
    cat(sprintf("Cloud Cover (-2)  : %12d (%5.2f%%)\n", cloud_count, (cloud_count / total_count) * 100))
    cat(sprintf("Missing (-3)      : %12d (%5.2f%%)\n", miss_count,  (miss_count / total_count) * 100))
  }
}

print_stats(ndvi_84_86, "1984_1986")
print_stats(ndvi_85, "1985")
print_stats(ndvi_85_03, "1985_03")

prep_plot_layers <- function(r) {
  r_small <- spatSample(r, size = 1000000, method = "regular", as.raster = TRUE)

  r_ndvi <- r_small
  r_ndvi[r_ndvi < -1] <- NA

  r_ndvi <- mask(r_ndvi, lake_vect, inverse = TRUE)
  
  r_flags <- r_small
  r_flags[r_flags >= -1] <- NA

  list(ndvi = r_ndvi, flags = r_flags)
}

cat("\nPrepping Layers for plot")
layers_84_86 <- prep_plot_layers(ndvi_84_86)
layers_85    <- prep_plot_layers(ndvi_85)
layers_85_03 <- prep_plot_layers(ndvi_85_03)

rv_window <- st_bbox(kenya_outline)


# ==============================================================================
# PLOT
# ==============================================================================

theme_master <- map_theme(text_scale = 1.3) +
  theme(
    plot.margin       = margin(t = 15, r = 10, b = 15, l = 20, unit = "pt"),
    plot.title        = element_text(hjust = 0.5, margin = margin(b = 10, unit = "pt")),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.margin     = margin(15, 15, 30, 15, unit = "pt")
  )

layer_lake   <- geom_sf(data = lake_victoria, fill = col_water, color = NA, inherit.aes = FALSE)
layer_border <- geom_sf(data = kenya_outline, fill = NA, color = "black", linewidth = lwidth_base, inherit.aes = FALSE)


create_plot <- function(layers, title, show_legends = FALSE) {
  p <- ggplot() +
    layer_lake +

    geom_spatraster(data = layers$ndvi) +
    scale_fill_gradientn(
      name = "NDVI",
      colors = c("#d73027", "#fdae61", "#ffffbf", "#d9ef8b", "#1a9850", "#006837"),
      limits = c(-0.1, 0.95),
      na.value = "transparent",
      guide = if (show_legends) guide_colorbar(order = 1, barheight = 10) else "none"
    )
  has_flags <- terra::global(layers$flags, "notNA")[1, 1] > 0

  if (has_flags) {
    p <- p +
        new_scale_fill() +
        geom_spatraster(data = as.factor(layers$flags)) +
        scale_fill_manual(
          name = "Data Quality",
          values = c(
            "-3"  = theme_colors[7],
            " "   = NA,
            "-2"  = theme_colors[10]
          ),
          labels = c(
            "-3"  = "Missing Pixel",
            " "   = "",
            "-2"  = "Cloud Cover"
          ),
          limits = c("-3", " ", "-2"),
          drop = FALSE,
          na.value = "transparent",
          na.translate = FALSE,
          guide = if (show_legends) guide_legend(order = 2, override.aes = list(color = NA)) else "none"
        )
  }
  p <- p +
    layer_border +
    coord_sf(xlim = rv_window[c("xmin", "xmax")], ylim = rv_window[c("ymin", "ymax")], expand = FALSE, datum = NA) +
    labs(title = title) +
    theme_master

  return(p)
}

p1 <- create_plot(layers_84_86, "NDVI 1984-1986", show_legends = FALSE)
p2 <- create_plot(layers_85,    "NDVI 1985",      show_legends = FALSE)
p3 <- create_plot(layers_85_03, "NDVI 03/1985",   show_legends = TRUE)

final_figure <- (p1 | p2 | p3) +
  plot_annotation(
    theme = map_theme(text_scale = 1.3)
  ) &
  map_theme(text_scale = 1.3) +
    theme(
      plot.margin       = margin(t = 15, r = 15, b = 15, l = 15, unit = "pt"),
    )

ggsave(
  filename = "output/figures/figureDescriptive_AggregationComparison.png",
  plot     = final_figure,
  width    = 16,
  height   = 6,
  dpi      = 400,
  bg       = "transparent"
)