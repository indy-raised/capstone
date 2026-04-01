# ==============================================================================
# 02_build_panel.R
#
# REPLICATORS START HERE.
#
# Loads the two pre-built data files included in /data and assembles the
# province-year panel used in all downstream analysis. No raw data access,
# no Google authentication, and no internet connection required.
#
# Inputs  (both included in this package):
#   data/ipc_with_covariates.csv   — province-year panel with all derived
#                                    variables already computed
#   data/combined_covariates.csv   — covariate panel (included for reference
#                                    and to support any re-merging if needed)
#
# Output:
#   ipc_panel  — a tibble in the R environment, used by all downstream scripts
#
# After running this script, proceed to:
#   03_panel_exploration.R   — covariate diagnostics and trend plots
#   04_synth_main.R          — main synthetic control analysis
#   05_robustness.R          — placebo and robustness checks
#
# Or simply run run_all.R to execute everything in sequence.
# ==============================================================================

source("code/01_setup.R")

# ---- Load province-year panel ------------------------------------------------

ipc_panel <- readr::read_csv(
  "data/ipc_with_covariates.csv",
  show_col_types = FALSE
)

cat("Panel loaded:\n")
cat("  Rows:      ", nrow(ipc_panel), "\n")
cat("  Columns:   ", ncol(ipc_panel), "\n")
cat("  Provinces: ", dplyr::n_distinct(ipc_panel$province), "\n")
cat("  Years:     ", min(ipc_panel$year, na.rm = TRUE), "–",
                     max(ipc_panel$year, na.rm = TRUE), "\n\n")

# ---- Sanity check: treated provinces and rollout years ----------------------

cat("IPC rollout years by province (treated only):\n")
ipc_panel %>%
  dplyr::filter(!is.na(Court_Year)) %>%
  dplyr::distinct(province, Court_Year) %>%
  dplyr::arrange(Court_Year, province) %>%
  print(n = Inf)

cat("\nControl provinces (never treated):\n")
ipc_panel %>%
  dplyr::filter(is.na(Court_Year)) %>%
  dplyr::distinct(province) %>%
  dplyr::arrange(province) %>%
  print(n = Inf)

# ---- Sanity check: key outcome variable -------------------------------------

cat("\nFTI per capita — Beijing (first 10 years):\n")
ipc_panel %>%
  dplyr::filter(province == "Beijing") %>%
  dplyr::select(year, foreign_total_investment, population_province, fti_pc) %>%
  dplyr::arrange(year) %>%
  print(n = 10)

# ---- Quick peek at panel structure ------------------------------------------

cat("\nPanel preview (Beijing, Shanghai, Guangdong — selected vars):\n")
ipc_panel %>%
  dplyr::filter(province %in% c("Beijing", "Shanghai", "Guangdong Province")) %>%
  dplyr::select(province, year, Court_Year, treated, rel_time, fti_pc, gdp_log) %>%
  dplyr::arrange(province, year) %>%
  print(n = 30)

cat("\nipc_panel is ready. Proceed to 03_panel_exploration.R or run_all.R.\n")
