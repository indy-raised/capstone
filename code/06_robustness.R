# ==============================================================================
# 06_robustness.R
#
# All robustness and placebo checks. Must run 05_synth_main.R first so that
# res_beijing and res_guangdong exist in the environment.
#
# Tests included:
#   Section 1:  Donor sensitivity (largest donor removed)
#   Section 2:  In-space placebo (spaghetti plot + MSPE ratio dot plot)
#   Section 3:  In-time placebo (fake treatment year = 2008)
#   Section 4:  Y-variable placebo tests — Beijing
#   Section 5:  Y-variable placebo tests — Guangdong
#   Appendix B1: Later-treated donor exclusion (2017–2020)
#   Appendix B2: Anticipation placebo (pseudo-treatment = 2013)
#   Summary:    Full robustness table saved to output/tables/
#
# INPUT:  data/ipc_with_covariates.csv
#         (res_beijing, res_guangdong from 05_synth_main.R)
# OUTPUT: output/figures/fig_placebo_*.png
#         output/tables/tab_robustness_full.csv
# ==============================================================================

source("code/01_setup.R")
source("code/04_synth_helpers.R")

ipc_panel <- readr::read_csv("data/ipc_with_covariates.csv")

# Run main SCM if objects not already in environment
if (!exists("res_beijing")) {
  res_beijing <- run_synth_for(ipc_panel, "Beijing",
                               drop_donor = c("Shanghai", "Guangdong Province"))
}
if (!exists("res_guangdong")) {
  res_guangdong <- run_synth_for(ipc_panel, "Guangdong Province",
                                 drop_donor = c("Shanghai", "Beijing"))
}

# ==============================================================================
# SECTION 1: Donor sensitivity — remove largest donor
# ==============================================================================

cat("\n=== SECTION 1: Donor Sensitivity ===\n")

res_beijing_sensitivity <- run_synth_for(
  ipc_panel, "Beijing",
  drop_donor = c("Shanghai", "Guangdong Province", "Tianjin")
)

res_guangdong_sensitivity <- run_synth_for(
  ipc_panel, "Guangdong Province",
  drop_donor = c("Shanghai", "Beijing", "Fujian Province")
)

sens_lims <- shared_ylims(list(res_beijing_sensitivity, res_guangdong_sensitivity,
                               res_beijing, res_guangdong))

plot_scm_path(res_beijing_sensitivity,   ylim = sens_lims$fti)
ggsave("output/figures/fig_scm_path_beijing_sensitivity.png", width = 7, height = 5, dpi = 300)
plot_scm_gap(res_beijing_sensitivity,    ylim = sens_lims$gap)
ggsave("output/figures/fig_scm_gap_beijing_sensitivity.png",  width = 7, height = 5, dpi = 300)

plot_scm_path(res_guangdong_sensitivity, ylim = sens_lims$fti)
ggsave("output/figures/fig_scm_path_guangdong_sensitivity.png", width = 7, height = 5, dpi = 300)
plot_scm_gap(res_guangdong_sensitivity,  ylim = sens_lims$gap)
ggsave("output/figures/fig_scm_gap_guangdong_sensitivity.png",  width = 7, height = 5, dpi = 300)

cat("Donor sensitivity MSPE ratios:\n")
cat("  Beijing baseline:         ", round(res_beijing$mspe$post / res_beijing$mspe$pre, 2), "\n")
cat("  Beijing (Tianjin dropped):", round(res_beijing_sensitivity$mspe$post /
                                           res_beijing_sensitivity$mspe$pre, 2), "\n")
cat("  Guangdong baseline:       ", round(res_guangdong$mspe$post / res_guangdong$mspe$pre, 2), "\n")
cat("  Guangdong (Fujian dropped):", round(res_guangdong_sensitivity$mspe$post /
                                            res_guangdong_sensitivity$mspe$pre, 2), "\n")

# ==============================================================================
# SECTION 2: In-space placebo — Beijing
# ==============================================================================

cat("\n=== SECTION 2: In-Space Placebo ===\n")

mspe_ratios <- compute_mspe_ratio(
  ipc_panel,
  treat_year        = 2014,
  exclude_provinces = c("Shanghai", "Guangdong Province")
)
mspe_ratios <- mspe_ratios %>% dplyr::mutate(treated = province == "Beijing")

# Outlier detection (Tukey fences)
Q1 <- quantile(mspe_ratios$ratio, 0.25, na.rm = TRUE)
Q3 <- quantile(mspe_ratios$ratio, 0.75, na.rm = TRUE)
upper_cutoff <- Q3 + 1.5 * (Q3 - Q1)
outliers <- mspe_ratios %>% dplyr::filter(ratio > upper_cutoff)

cat("Outlier cutoff:", round(upper_cutoff, 2), "\n")
cat("Outlier provinces:", paste(outliers$province, collapse = ", "), "\n")

# Rank-based p-values
beijing_ratio   <- mspe_ratios %>% dplyr::filter(province == "Beijing") %>% dplyr::pull(ratio)
n_total         <- nrow(mspe_ratios)
n_greater_full  <- sum(mspe_ratios$ratio >= beijing_ratio)
p_value_full    <- n_greater_full / n_total
cat("Randomization p-value (full):             ", round(p_value_full, 3),
    "(", n_greater_full, "/", n_total, ")\n")

mspe_no_anhui    <- mspe_ratios %>% dplyr::filter(province != "Anhui Province")
n_no_anhui       <- nrow(mspe_no_anhui)
n_greater_clean  <- sum(mspe_no_anhui$ratio >= beijing_ratio)
p_value_no_anhui <- n_greater_clean / n_no_anhui
cat("Randomization p-value (Anhui excluded):   ", round(p_value_no_anhui, 3),
    "(", n_greater_clean, "/", n_no_anhui, ")\n")

# Spaghetti plot
placebo_space_all(ipc_panel, "Beijing")
ggsave("output/figures/fig_placebo_spaghetti_beijing.png", width = 8, height = 6, dpi = 300)

# MSPE dot plot (main, Anhui excluded)
p_mspe <- plot_mspe_ratio_dotplot(mspe_ratios)
print(p_mspe)
ggsave("output/figures/fig_mspe_dotplot_beijing.png", p_mspe, width = 7, height = 8, dpi = 300)

# MSPE dot plot (with Anhui, for appendix)
p_mspe_anhui <- plot_mspe_ratio_dotplot(
  mspe_ratios, exclude = NULL,
  title = "Post/Pre MSPE Ratios: All Placebo Provinces (incl. Anhui)"
)
ggsave("output/figures/fig_mspe_dotplot_beijing_with_anhui.png", p_mspe_anhui,
       width = 7, height = 8, dpi = 300)

# Histogram
ggplot2::ggplot(mspe_ratios, ggplot2::aes(x = ratio)) +
  ggplot2::geom_histogram(bins = 30, fill = "gray75", color = "gray75", alpha = 0.8) +
  ggplot2::geom_vline(
    data = mspe_ratios %>% dplyr::filter(treated),
    ggplot2::aes(xintercept = ratio), color = "black", linewidth = 1.2, linetype = "dotted"
  ) +
  ggplot2::labs(title = "Histogram of Post/Pre MSPE Ratios",
                subtitle = "Placebo units vs. Beijing (treated)",
                x = "MSPE Ratio (Post / Pre)", y = "Count")
ggsave("output/figures/fig_mspe_histogram_beijing.png", width = 7, height = 5, dpi = 300)

# ==============================================================================
# SECTION 2b: In-space placebo — Guangdong
# ==============================================================================

mspe_ratios_guangdong <- compute_mspe_ratio(
  ipc_panel,
  treat_year        = 2014,
  exclude_provinces = c("Beijing", "Shanghai", "Guangdong Province")
)
mspe_ratios_guangdong <- dplyr::bind_rows(
  mspe_ratios_guangdong,
  tibble::tibble(
    province  = "Guangdong Province",
    mspe_pre  = res_guangdong$mspe$pre,
    mspe_post = res_guangdong$mspe$post,
    ratio     = res_guangdong$mspe$post / res_guangdong$mspe$pre
  )
) %>% dplyr::mutate(treated = province == "Guangdong Province")

guangdong_ratio  <- mspe_ratios_guangdong %>%
  dplyr::filter(province == "Guangdong Province") %>% dplyr::pull(ratio)
n_total_gd       <- nrow(mspe_ratios_guangdong)
n_greater_gd     <- sum(mspe_ratios_guangdong$ratio >= guangdong_ratio)
p_value_gd       <- n_greater_gd / n_total_gd
cat("\nGDPD randomization p-value:", round(p_value_gd, 3),
    "(", n_greater_gd, "/", n_total_gd, ")\n")

plot_mspe_ratio_dotplot(
  mspe_ratios_guangdong, treated_label = "Guangdong Province",
  title    = "Post/Pre MSPE Ratios: Guangdong vs. Placebo Provinces",
  subtitle = "Placebo inference across donor pool (Beijing and Shanghai excluded)"
)
ggsave("output/figures/fig_mspe_dotplot_guangdong.png", width = 7, height = 8, dpi = 300)

placebo_space_all(
  ipc_panel %>% dplyr::filter(!province %in% c("Beijing", "Shanghai")),
  treated_provinces = "Guangdong Province"
)
ggsave("output/figures/fig_placebo_spaghetti_guangdong.png", width = 8, height = 6, dpi = 300)

# ==============================================================================
# SECTION 3: In-time placebo (fake year = 2008)
# ==============================================================================

cat("\n=== SECTION 3: In-Time Placebo ===\n")

plot_placebo_time_single(
  ipc_panel, "Beijing",
  fake_year  = 2008,
  drop_donor = c("Shanghai", "Guangdong Province"),
  years      = 2000:2013
)
ggsave("output/figures/fig_placebo_intime_beijing.png", width = 7, height = 5, dpi = 300)

plot_placebo_time_single(
  ipc_panel, "Guangdong Province",
  fake_year  = 2008,
  drop_donor = c("Shanghai", "Beijing"),
  years      = 2000:2013
)
ggsave("output/figures/fig_placebo_intime_guangdong.png", width = 7, height = 5, dpi = 300)

# Compute ratio for in-time Beijing (for table)
res_beijing_intime <- run_synth_for(
  ipc_panel, "Beijing", treat_year = 2008, years = 2000:2013,
  drop_donor = c("Shanghai", "Guangdong Province")
)
res_gd_intime <- run_synth_for(
  ipc_panel, "Guangdong Province", treat_year = 2008, years = 2000:2013,
  drop_donor = c("Shanghai", "Beijing")
)
r_beijing_intime   <- res_beijing_intime$mspe$post   / res_beijing_intime$mspe$pre
r_gd_intime        <- res_gd_intime$mspe$post         / res_gd_intime$mspe$pre

# ==============================================================================
# SECTION 4: Y-variable placebo — Beijing
# ==============================================================================

cat("\n=== SECTION 4: Y-Variable Placebo — Beijing ===\n")

res_placebo_income  <- run_synth_for_alt_outcome(ipc_panel, "Beijing",
  "disposable_income_urban_per_cap", drop_donor = c("Shanghai", "Guangdong Province"))
res_placebo_gdp     <- run_synth_for_alt_outcome(ipc_panel, "Beijing",
  "gdp_log", drop_donor = c("Shanghai", "Guangdong Province"))
res_placebo_privent <- run_synth_for_alt_outcome(ipc_panel, "Beijing",
  "num_priv_enterprise_pc", drop_donor = c("Shanghai", "Guangdong Province"))
res_placebo_enroll  <- run_synth_for_alt_outcome(ipc_panel, "Beijing",
  "student_enrollment_pc", drop_donor = c("Shanghai", "Guangdong Province"))

plot_alt_outcome_placebo(res_placebo_income)
plot_alt_outcome_placebo(res_placebo_gdp)
plot_alt_outcome_placebo(res_placebo_privent)
plot_alt_outcome_placebo(res_placebo_enroll)

cat("\nY-Placebo MSPE Ratios — Beijing:\n")
cat("  Main (FTI pc):           ", round(res_beijing$mspe$post / res_beijing$mspe$pre, 2), "\n")
cat("  Urban Income:            ", round(res_placebo_income$mspe$ratio, 2), "\n")
cat("  GDP (log):               ", round(res_placebo_gdp$mspe$ratio, 2), "\n")
cat("  Private Enterprise pc:   ", round(res_placebo_privent$mspe$ratio, 2), "\n")
cat("  Student Enrollment pc:   ", round(res_placebo_enroll$mspe$ratio, 2), "\n")

# ==============================================================================
# SECTION 5: Y-variable placebo — Guangdong
# ==============================================================================

cat("\n=== SECTION 5: Y-Variable Placebo — Guangdong ===\n")

res_gd_placebo_income  <- run_synth_for_alt_outcome(ipc_panel, "Guangdong Province",
  "disposable_income_urban_per_cap", drop_donor = c("Shanghai", "Beijing"))
res_gd_placebo_gdp     <- run_synth_for_alt_outcome(ipc_panel, "Guangdong Province",
  "gdp_log", drop_donor = c("Shanghai", "Beijing"))
res_gd_placebo_privent <- run_synth_for_alt_outcome(ipc_panel, "Guangdong Province",
  "num_priv_enterprise_pc", drop_donor = c("Shanghai", "Beijing"))
res_gd_placebo_enroll  <- run_synth_for_alt_outcome(ipc_panel, "Guangdong Province",
  "student_enrollment_pc", drop_donor = c("Shanghai", "Beijing"))

plot_alt_outcome_placebo(res_gd_placebo_income)
plot_alt_outcome_placebo(res_gd_placebo_gdp)
plot_alt_outcome_placebo(res_gd_placebo_privent)
plot_alt_outcome_placebo(res_gd_placebo_enroll)

cat("\nY-Placebo MSPE Ratios — Guangdong:\n")
cat("  Main (FTI pc):           ", round(res_guangdong$mspe$post / res_guangdong$mspe$pre, 2), "\n")
cat("  Urban Income:            ", round(res_gd_placebo_income$mspe$ratio, 2), "\n")
cat("  GDP (log):               ", round(res_gd_placebo_gdp$mspe$ratio, 2), "\n")
cat("  Private Enterprise pc:   ", round(res_gd_placebo_privent$mspe$ratio, 2), "\n")
cat("  Student Enrollment pc:   ", round(res_gd_placebo_enroll$mspe$ratio, 2), "\n")

# ==============================================================================
# APPENDIX B1: Later-treated donor exclusion (2017–2020)
# ==============================================================================

cat("\n=== APPENDIX B1: Later-Treated Donor Exclusion ===\n")

later_treated_provinces <- ipc_panel %>%
  dplyr::filter(!is.na(Court_Year), Court_Year >= 2017) %>%
  dplyr::distinct(province) %>%
  dplyr::pull(province)
cat("Later-treated provinces (N =", length(later_treated_provinces), "):\n")
print(later_treated_provinces)

res_beijing_nolater <- run_synth_for(
  ipc_panel, "Beijing", years = 2000:2016,
  drop_donor = c("Shanghai", "Guangdong Province", later_treated_provinces)
)
res_guangdong_nolater <- run_synth_for(
  ipc_panel, "Guangdong Province", years = 2000:2016,
  drop_donor = c("Shanghai", "Beijing", later_treated_provinces)
)

b1_lims <- shared_ylims(list(res_beijing_nolater, res_guangdong_nolater))

plot_scm_path_custom(res_beijing_nolater, treat_yr = 2014,
  title    = "Synthetic Control: Beijing (2014)",
  subtitle = "Later-treated provinces (2017\u20132020) excluded from donor pool",
  ylim     = b1_lims$fti)
ggsave("output/figures/fig_b1_path_beijing_nolater.png", width = 7, height = 5, dpi = 300)

plot_scm_gap_custom(res_beijing_nolater, treat_yr = 2014,
  title    = "Gap: Beijing vs. Synthetic (2014)",
  subtitle = paste0("Later-treated excluded | Post/Pre MSPE Ratio = ",
                    round(res_beijing_nolater$mspe$post / res_beijing_nolater$mspe$pre, 2)),
  ylim     = b1_lims$gap)
ggsave("output/figures/fig_b1_gap_beijing_nolater.png", width = 7, height = 5, dpi = 300)

plot_scm_path_custom(res_guangdong_nolater, treat_yr = 2014,
  title    = "Synthetic Control: Guangdong (2014)",
  subtitle = "Later-treated provinces (2017\u20132020) excluded from donor pool",
  ylim     = b1_lims$fti)
ggsave("output/figures/fig_b1_path_guangdong_nolater.png", width = 7, height = 5, dpi = 300)

plot_scm_gap_custom(res_guangdong_nolater, treat_yr = 2014,
  title    = "Gap: Guangdong vs. Synthetic (2014)",
  subtitle = paste0("Later-treated excluded | Post/Pre MSPE Ratio = ",
                    round(res_guangdong_nolater$mspe$post / res_guangdong_nolater$mspe$pre, 2)),
  ylim     = b1_lims$gap)
ggsave("output/figures/fig_b1_gap_guangdong_nolater.png", width = 7, height = 5, dpi = 300)

# ==============================================================================
# APPENDIX B2: Anticipation placebo (pseudo-treatment = 2013)
# ==============================================================================

cat("\n=== APPENDIX B2: Anticipation Placebo (2013) ===\n")

ANTIC_FAKE_YEAR <- 2013
ANTIC_YEARS     <- 2000:2015

res_beijing_antic <- run_synth_for(
  ipc_panel, "Beijing", treat_year = ANTIC_FAKE_YEAR, years = ANTIC_YEARS,
  drop_donor = c("Shanghai", "Guangdong Province")
)
res_guangdong_antic <- run_synth_for(
  ipc_panel, "Guangdong Province", treat_year = ANTIC_FAKE_YEAR, years = ANTIC_YEARS,
  drop_donor = c("Shanghai", "Beijing")
)

r_beijing_antic   <- res_beijing_antic$mspe$post   / res_beijing_antic$mspe$pre
r_guangdong_antic <- res_guangdong_antic$mspe$post / res_guangdong_antic$mspe$pre

b2_lims <- shared_ylims(list(res_beijing_antic, res_guangdong_antic))

plot_scm_path_custom(res_beijing_antic, treat_yr = ANTIC_FAKE_YEAR,
  title    = "Anticipation Placebo: Beijing (Pseudo-Treatment = 2013)",
  subtitle = "Tests whether investment diverged one year before actual IPC launch",
  ylim     = b2_lims$fti)
ggsave("output/figures/fig_b2_path_beijing_antic.png", width = 7, height = 5, dpi = 300)

plot_scm_gap_custom(res_beijing_antic, treat_yr = ANTIC_FAKE_YEAR,
  title    = "Anticipation Placebo Gap: Beijing (Pseudo-Treatment = 2013)",
  subtitle = paste0("Post/Pre MSPE Ratio = ", round(r_beijing_antic, 2),
                    "  |  True-treatment ratio = ",
                    round(res_beijing$mspe$post / res_beijing$mspe$pre, 2)),
  ylim     = b2_lims$gap)
ggsave("output/figures/fig_b2_gap_beijing_antic.png", width = 7, height = 5, dpi = 300)

plot_scm_path_custom(res_guangdong_antic, treat_yr = ANTIC_FAKE_YEAR,
  title    = "Anticipation Placebo: Guangdong (Pseudo-Treatment = 2013)",
  subtitle = "Tests whether investment diverged one year before actual IPC launch",
  ylim     = b2_lims$fti)
ggsave("output/figures/fig_b2_path_guangdong_antic.png", width = 7, height = 5, dpi = 300)

plot_scm_gap_custom(res_guangdong_antic, treat_yr = ANTIC_FAKE_YEAR,
  title    = "Anticipation Placebo Gap: Guangdong (Pseudo-Treatment = 2013)",
  subtitle = paste0("Post/Pre MSPE Ratio = ", round(r_guangdong_antic, 2),
                    "  |  True-treatment ratio = ",
                    round(res_guangdong$mspe$post / res_guangdong$mspe$pre, 2)),
  ylim     = b2_lims$gap)
ggsave("output/figures/fig_b2_gap_guangdong_antic.png", width = 7, height = 5, dpi = 300)

cat("\nAnticipation placebo ratios:\n")
cat("  Beijing   — true (2014):", round(res_beijing$mspe$post / res_beijing$mspe$pre, 2),
    "| pseudo (2013):", round(r_beijing_antic, 2), "\n")
cat("  Guangdong — true (2014):", round(res_guangdong$mspe$post / res_guangdong$mspe$pre, 2),
    "| pseudo (2013):", round(r_guangdong_antic, 2), "\n")

# ==============================================================================
# SUMMARY: Full robustness table
# ==============================================================================

robustness_table_full <- tibble::tibble(
  Test = c(
    "Main SCM (2014 treatment)",
    "B1: Later-treated donors excluded (2017-2020)",
    "B2: Anticipation placebo (pseudo-year = 2013)",
    "Donor sensitivity: largest donor removed",
    "Y-Placebo: Urban disposable income pc",
    "Y-Placebo: Log GDP",
    "Y-Placebo: Private enterprise pc",
    "Y-Placebo: Student enrollment pc",
    "In-time placebo (fake year = 2008)"
  ),
  Beijing_Ratio = c(
    round(res_beijing$mspe$post / res_beijing$mspe$pre, 2),
    round(res_beijing_nolater$mspe$post / res_beijing_nolater$mspe$pre, 2),
    round(r_beijing_antic, 2),
    round(res_beijing_sensitivity$mspe$post / res_beijing_sensitivity$mspe$pre, 2),
    round(res_placebo_income$mspe$ratio, 2),
    round(res_placebo_gdp$mspe$ratio, 2),
    round(res_placebo_privent$mspe$ratio, 2),
    round(res_placebo_enroll$mspe$ratio, 2),
    round(r_beijing_intime, 2)
  ),
  Guangdong_Ratio = c(
    round(res_guangdong$mspe$post / res_guangdong$mspe$pre, 2),
    round(res_guangdong_nolater$mspe$post / res_guangdong_nolater$mspe$pre, 2),
    round(r_guangdong_antic, 2),
    round(res_guangdong_sensitivity$mspe$post / res_guangdong_sensitivity$mspe$pre, 2),
    round(res_gd_placebo_income$mspe$ratio, 2),
    round(res_gd_placebo_gdp$mspe$ratio, 2),
    round(res_gd_placebo_privent$mspe$ratio, 2),
    round(res_gd_placebo_enroll$mspe$ratio, 2),
    round(r_gd_intime, 2)
  )
)

cat("\n--- Full Robustness Table ---\n")
print(robustness_table_full, n = Inf)

readr::write_csv(robustness_table_full, "output/tables/tab_robustness_full.csv")
cat("\nSaved: output/tables/tab_robustness_full.csv\n")
cat("\n05_robustness.R complete.\n")
