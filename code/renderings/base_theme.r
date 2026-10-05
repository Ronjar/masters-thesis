library(ggplot2)
library(ggthemes)
library(scales)

# ==============================================================================
# DESIGN
# ==============================================================================

theme_colors <- ggthemes::tableau_color_pal("Tableau 10")(10)


col_water       <- scales::alpha(theme_colors[1], 0.8)
col_settlements <- theme_colors[3]
col_forests     <- theme_colors[5]
col_highlands   <- theme_colors[6]

col_settlements_fill <- scales::alpha(col_settlements, 0.3)
col_forests_fill     <- scales::alpha(col_forests, 0.3)
col_highlands_fill   <- scales::alpha(col_highlands, 0.3)


lwidth_thin  <- 0.2  # Secondary / subtle lines
lwidth_base  <- 0.6  # Standard lines
lwidth_thick <- 1.0  # Emphasized / focus lines

tsize_small <- 12  # Secondary / detail text (legend.text, axis.text)
tsize_base  <- 14  # Standard / label text (axis.title, legend.title)
tsize_title <- 18  # Emphasized / title text (plot.title)


# ==============================================================================
# THEMES
# ==============================================================================

text_theme <- function(text_scale = 1.0) {
  theme(
    plot.title    = element_text(face = "bold", size = tsize_title * text_scale),
    plot.subtitle = element_text(size = tsize_base * text_scale),
    axis.title    = element_text(size = tsize_title * text_scale),
    axis.text     = element_text(size = tsize_small * text_scale),
    legend.title  = element_text(margin = margin(b = 15, unit = "pt"), face = "bold", size = tsize_base * text_scale),
    legend.text   = element_text(size = tsize_small * text_scale)
  )
}

graph_theme <- function(text_scale = 1.0) {
  theme_classic() +
    text_theme(text_scale = text_scale) +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background  = element_rect(fill = "transparent", color = NA),
      axis.title.x     = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
      axis.title.y     = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))
    )
}

map_theme <- function(text_scale = 1.0) {
  theme_minimal() +
  text_theme(text_scale = text_scale) +
  theme(
    panel.border     = element_blank(),
    panel.grid       = element_blank(),
    axis.title       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    plot.margin      = margin(0, 0, 0, 0, "pt"),
    legend.position  = "right"
  )
}