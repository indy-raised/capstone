# ==============================================================================
# run_all.R
#
# Master script — runs the full replication in order.
#
# BEFORE RUNNING:
#   1. Make sure data/ipc_with_covariates.csv is in place (included in package)
#   2. Set your working directory to the root of this repository
#      e.g.:  setwd("path/to/ipc_replication")
#   3. Install packages by running code/01_setup.R first, or let this script
#      do it automatically (it sources 01_setup.R).
#
# SCRIPTS EXECUTED (in order):
#   01_setup.R          — install/load packages, set global theme
#   02_panel_exploration.R  — exploratory diagnostics and covariate trends
#   04_synth_main.R     — main SCM analysis (Beijing + Guangdong)
#   05_robustness.R     — all placebo and robustness checks
#
# NOTE: 00_data_preprocessing_NOT_RUN.R is intentionally excluded.
#       It documents the restricted-data pipeline for transparency only.
#
# Estimated run time: 25–60 minutes depending on hardware (placebo loops
# run SCM for all provinces and are the main bottleneck).
# ==============================================================================

cat("=== IPC Replication Package ===\n")
cat("Working directory:", getwd(), "\n\n")

# Verify data files are present before starting
required_data <- c("data/ipc_with_covariates.csv", "data/combined_covariates.csv")
missing_data  <- required_data[!file.exists(required_data)]
if (length(missing_data) > 0) {
  stop("Required data file(s) not found:\n  ",
       paste(missing_data, collapse = "\n  "),
       "\nMake sure you are running from the repository root directory.")
}

# Create output directories if they don't exist
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)

cat("--- Step 1: Setup & packages ---\n")
source("code/01_setup.R")

cat("\n--- Step 2: Build panel from pre-built CSVs ---\n")
source("code/02_build_panel.R")

cat("\n--- Step 3: Panel exploration & diagnostics ---\n")
source("code/03_panel_exploration.R")

cat("\n--- Step 4: Main SCM analysis ---\n")
source("code/05_synth_main.R")

cat("\n--- Step 5: Robustness checks ---\n")
source("code/06_robustness.R")

cat("\n=== Replication complete. ===\n")
cat("Figures saved to: output/figures/\n")
cat("Tables saved to:  output/tables/\n")
