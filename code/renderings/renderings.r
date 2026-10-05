# ==============================================================================
# RUN ALL FIGURE AND TABLE RENDERINGS
# ==============================================================================

if (file.exists("base_theme.r") && file.exists("../../data")) {
  setwd("../..")
} else if (file.exists("renderings.r") && file.exists("../../data")) {
  setwd("../..")
}

renderings_dir <- "code/renderings"

if (!dir.exists(renderings_dir)) {
  stop("Could not locate 'code/renderings' directory. Please run from project root.")
}

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

all_scripts <- list.files(
  path = renderings_dir,
  pattern = "\\.[rR]$",
  recursive = TRUE,
  full.names = TRUE
)

helpers_to_exclude <- c("renderings.r", "base_theme.r", "rdd_plot.r")
scripts <- all_scripts[!basename(all_scripts) %in% helpers_to_exclude]
scripts <- sort(scripts)

n_total <- length(scripts)
cat(sprintf("\nFound %d rendering scripts in '%s'.\n", n_total, renderings_dir))
cat(paste(rep("=", 70), collapse = ""), "\n\n")

rscript_bin <- file.path(R.home("bin"), "Rscript")

results <- data.frame(
  script  = scripts,
  status  = character(n_total),
  elapsed = numeric(n_total),
  stringsAsFactors = FALSE
)

total_start_time <- Sys.time()

for (i in seq_along(scripts)) {
  script <- scripts[i]
  cat(sprintf("[%d/%d] Starting: %s\n", i, n_total, script))
  cat(paste(rep("-", 70), collapse = ""), "\n")

  t_start <- Sys.time()
  exit_code <- system2(rscript_bin, args = script)
  t_end <- Sys.time()

  elapsed_secs <- round(as.numeric(difftime(t_end, t_start, units = "secs")), 1)
  results$elapsed[i] <- elapsed_secs

  if (exit_code == 0) {
    results$status[i] <- "SUCCESS"
    cat(sprintf("\n==> [%d/%d] SUCCESS (%s s): %s\n\n", i, n_total, elapsed_secs, script))
  } else {
    results$status[i] <- sprintf("FAILED (code %d)", exit_code)
    cat(sprintf("\n==> [%d/%d] FAILED with code %d (%s s): %s\n\n", i, n_total, exit_code, elapsed_secs, script))
  }
}

total_end_time <- Sys.time()
total_elapsed <- round(as.numeric(difftime(total_end_time, total_start_time, units = "secs")), 1)

# ==============================================================================
# SUMMARY REPORT
# ==============================================================================

cat("\n" , paste(rep("=", 70), collapse = ""), "\n")
cat("SUMMARY OF RENDERINGS EXECUTION\n")
cat(paste(rep("=", 70), collapse = ""), "\n")

for (i in seq_along(scripts)) {
  status_icon <- if (results$status[i] == "SUCCESS") "[OK]  " else "[FAIL]"
  cat(sprintf("%s %-55s (%5.1f s)\n", status_icon, results$script[i], results$elapsed[i]))
}

n_success <- sum(results$status == "SUCCESS")
n_failed  <- n_total - n_success

cat(paste(rep("-", 70), collapse = ""), "\n")
cat(sprintf("Total: %d scripts | Success: %d | Failed: %d | Total Time: %.1f s\n",
            n_total, n_success, n_failed, total_elapsed))
cat(paste(rep("=", 70), collapse = ""), "\n\n")

if (n_failed > 0) {
  stop(sprintf("%d rendering script(s) failed.", n_failed))
}
