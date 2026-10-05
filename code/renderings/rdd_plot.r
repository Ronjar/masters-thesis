library(arrow)
library(dplyr)
library(ggplot2)
source("code/renderings/base_theme.r")

create_rdd_plot <- function(data,
                            var_name,
                            y_label,
                            output_filename,
                            dist_var = "signed_distance",
                            ctrl_var = NULL,
                            y_limits = NULL,
                            h = 3000,
                            n_bins_per_side = 10,
                            ci_level = 0.90,
                            text_scale = 1.0) {

  message(sprintf("Creating RDD plot for variable: %s", y_label))

  bin_width <- h / n_bins_per_side
  scale_x <- h / n_bins_per_side * 2

  df <- data |>
    filter(!is.na(.data[[dist_var]]), !is.na(.data[[var_name]])) |>
    filter(abs(.data[[dist_var]]) <= h) |>
    mutate(side = if_else(.data[[dist_var]] < 0, "Left", "Right"))

  df_binned <- df |>
    mutate(
      bin_x = bin_width * floor(.data[[dist_var]] / bin_width) + (bin_width / 2)
    ) |>
    group_by(side, bin_x) |>
    summarise(
      var_mean = mean(.data[[var_name]], na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

  for (i in seq_len(nrow(df_binned))) {
    message(sprintf("x: %f, y: %f, n: %d",
                    df_binned$bin_x[i],
                    df_binned$var_mean[i],
                    df_binned$n[i]))
  }

  make_ci_df <- function(d, level = ci_level, n_grid = 200) {
    if (!is.null(ctrl_var)) {
      f <- as.formula(paste(var_name, "~", dist_var, "+", ctrl_var))
    } else {
      f <- as.formula(paste(var_name, "~", dist_var))
    }

    m <- lm(f, data = d)

    xg <- seq(min(d[[dist_var]], na.rm = TRUE),
              max(d[[dist_var]], na.rm = TRUE),
              length.out = n_grid)

    pred_data <- data.frame(xg)
    colnames(pred_data) <- dist_var

    if (!is.null(ctrl_var)) {
      pred_data[[ctrl_var]] <- mean(df[[ctrl_var]], na.rm = TRUE)
    }

    pred <- predict(m,
                    newdata = pred_data,
                    interval = "confidence",
                    level = level)

    res <- tibble(
      fit = pred[, "fit"],
      lwr = pred[, "lwr"],
      upr = pred[, "upr"]
    )
    res[[dist_var]] <- xg
    return(res)
  }

  ci_left  <- make_ci_df(df |> filter(.data[[dist_var]] < 0)) |> mutate(side = "Left")
  ci_right <- make_ci_df(df |> filter(.data[[dist_var]] >= 0)) |> mutate(side = "Right")
  ci_all   <- bind_rows(ci_left, ci_right)

  p <- ggplot() +
    geom_ribbon(
      data = ci_all,
      aes(x = .data[[dist_var]], ymin = lwr, ymax = upr, group = side),
      fill = "grey60", alpha = 0.4
    ) +
    geom_line(
      data = ci_all,
      aes(x = .data[[dist_var]], y = fit, group = side),
      linewidth = lwidth_base, color = "black"
    ) +
    geom_point(
      data = df_binned,
      aes(x = bin_x, y = var_mean),
      shape = 16, size = 3, color = "black", alpha = 0.8
    ) +
    geom_vline(xintercept = 0, linewidth = lwidth_base, linetype = "dashed", color = "#b22222") +
    labs(
      title = NULL,
      x = "Distance to Settlement Scheme Border (m)",
      y = y_label
    ) +
    scale_x_continuous(breaks = scales::breaks_width(scale_x)) +
    graph_theme(text_scale = text_scale)

  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }

  save_path <- file.path("output/figures/", output_filename)
  ggsave(save_path, plot = p, width = 10, height = 6, dpi = 300)

  return(invisible(p))
}