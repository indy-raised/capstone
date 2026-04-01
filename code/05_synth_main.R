# ==============================================================================
# 05_synth_main.R
#
# Main synthetic control analysis.
#   - Runs SCM for Beijing and Guangdong Province (treatment year = 2014)
#   - Produces path plots and gap plots
#   - Computes corrected aggregate FTI effect estimates
#   - Exports results to Google Sheets (optional — requires auth)
#
# INPUT:  data/ipc_with_covariates.csv
# OUTPUT: output/figures/fig_scm_path_beijing.png
#         output/figures/fig_scm_gap_beijing.png
#         output/figures/fig_scm_path_guangdong.png
#         output/figures/fig_scm_gap_guangdong.png
#         output/tables/tab_estimates.csv
# ==============================================================================

source("code/01_setup.R")
source("code/04_synth_helpers.R")

ipc_panel <- readr::read_csv("data/ipc_with_covariates.csv")

# ==============================================================================
# SECTION 1: Run SCM
# Shanghai and Guangdong / Beijing are mutual exclusions to avoid
# same-tier mega-city contamination in the donor pool.
# ==============================================================================

res_beijing <- run_synth_for(
  ipc_panel, "Beijing",
  drop_donor = c("Shanghai", "Guangdong Province")
)

res_guangdong <- run_synth_for(
  ipc_panel, "Guangdong Province",
  drop_donor = c("Shanghai", "Beijing")
)

# ==============================================================================
# SECTION 2: Compute global y-axis limits for comparability
# ==============================================================================

global_lims <- shared_ylims(list(res_beijing, res_guangdong))

# ==============================================================================
# SECTION 3: Path and gap plots
# ==============================================================================

plot_scm_path(res_beijing,   ylim = global_lims$fti)
ggsave("output/figures/fig_scm_path_beijing.png",   width = 7, height = 5, dpi = 300)

plot_scm_gap(res_beijing,    ylim = global_lims$gap)
ggsave("output/figures/fig_scm_gap_beijing.png",    width = 7, height = 5, dpi = 300)

plot_scm_path(res_guangdong, ylim = global_lims$fti)
ggsave("output/figures/fig_scm_path_guangdong.png", width = 7, height = 5, dpi = 300)

plot_scm_gap(res_guangdong,  ylim = global_lims$gap)
ggsave("output/figures/fig_scm_gap_guangdong.png",  width = 7, height = 5, dpi = 300)

cat("SCM path and gap figures saved to output/figures/\n")

# ==============================================================================
# SECTION 4: Predictor balance tables
# ==============================================================================

cat("\n--- Beijing Predictor Balance ---\n")
print(res_beijing$tables$tab.pred)

cat("\n--- Guangdong Predictor Balance ---\n")
print(res_guangdong$tables$tab.pred)

# ==============================================================================
# SECTION 5: Aggregate effect estimates
#
# Unit clarification:
#   foreign_total_investment : 10,000 USD (万美元)
#   population_province      : 10,000 persons (万人)
#   fti_pc                   : USD per person  (10,000 / 10,000)
#   gap_pc (from SCM)        : USD per person
#   aggregate = gap_pc x population → 10,000 USD
#   Convert: / 100 → million USD   / 100,000 → billion USD
# ==============================================================================

est_beijing   <- get_corrected_estimates(res_beijing,   ipc_panel)
est_guangdong <- get_corrected_estimates(res_guangdong, ipc_panel)

# Save yearly estimates to output
tab_est <- dplyr::bind_rows(est_beijing$yearly, est_guangdong$yearly)
readr::write_csv(tab_est, "output/tables/tab_estimates.csv")
cat("Saved: output/tables/tab_estimates.csv\n")

cat("\n05_synth_main.R complete.\n")
