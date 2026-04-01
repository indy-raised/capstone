# ==============================================================================
# 02_panel_exploration.R
#
# Loads the processed province-year panel and runs exploratory diagnostics:
#   - Variable trends across provinces
#   - Correlation / multicollinearity check
#   - Raw outcome plot (FTI per capita vs. average donor)
#
# INPUT:  data/ipc_with_covariates.csv
# OUTPUT: output/figures/fig_variable_trends.png
#         output/figures/fig_corrplot.png
#         output/figures/fig_raw_outcome_beijing.png
# ==============================================================================

source("code/01_setup.R")

# ---- Load panel --------------------------------------------------------------
ipc_panel <- readr::read_csv("data/ipc_with_covariates.csv")
cat("Panel loaded:", nrow(ipc_panel), "rows,", ncol(ipc_panel), "columns\n")

# ==============================================================================
# SECTION 1: Variable trend plots
# ==============================================================================

ipc_panel %>%
  select(year, province,
         fti_pc, gdp_log, patent_apps_pc,
         num_priv_enterprise_pc, student_enrollment_pc,
         disposable_income_urban_per_cap, cpi,
         r.d_expenditure_pc, r.d_workers_pc, export_intensity) %>%
  pivot_longer(cols = -c(year, province), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = year, y = value, group = province)) +
  geom_line(alpha = 0.3) +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "Variable Variation 2000–2020", y = "Value", x = "Year")

ggsave("output/figures/fig_variable_trends.png", width = 12, height = 8, dpi = 300)
cat("Saved: output/figures/fig_variable_trends.png\n")

# ==============================================================================
# SECTION 2: Correlation matrix (multicollinearity check)
# ==============================================================================

covariate_vars <- c(
  "fti_pc", "gdp_log", "num_priv_enterprise_pc",
  "registered_unemployed", "cpi", "student_enrollment_pc",
  "disposable_income_rural_per_cap", "disposable_income_urban_per_cap",
  "r.d_expenditure_pc", "r.d_workers_pc", "patent_apps_pc", "export_intensity"
)

corr_data <- ipc_panel %>%
  dplyr::select(all_of(covariate_vars)) %>%
  dplyr::filter(if_all(everything(), ~!is.na(.)))

corr_matrix <- round(cor(corr_data, use = "pairwise.complete.obs"), 2)

short_names <- c(
  "FTI_pc", "logGDP", "PrivEnt_pc", "Unemp", "CPI",
  "Enroll_pc", "RuralInc_pc", "UrbanInc_pc",
  "RDexp_pc", "RDwork_pc", "Patents_pc", "Exports"
)
colnames(corr_matrix) <- rownames(corr_matrix) <- short_names

png("output/figures/fig_corrplot.png", width = 800, height = 800, res = 120)
par(mar = c(2, 2, 3, 2), family = "sans")
corrplot::corrplot(
  corr_matrix,
  method       = "color",
  type         = "upper",
  tl.col       = "black",
  tl.srt       = 30,
  tl.cex       = 0.9,
  addCoef.col  = "black",
  number.cex   = 0.7,
  cl.cex       = 0.9,
  col          = colorRampPalette(c("#F7FBFF", "#6BAED6", "#08306B"))(200),
  title        = "Correlation Matrix of Covariates",
  diag         = FALSE
)
dev.off()
cat("Saved: output/figures/fig_corrplot.png\n")

# Flag high multicollinearity
high_corr <- which(abs(corr_matrix) > 0.85 & abs(corr_matrix) < 1, arr.ind = TRUE)
if (nrow(high_corr) > 0) {
  cat("Highly correlated pairs (|r| > 0.85):\n")
  apply(high_corr, 1, function(idx) {
    cat(" ", rownames(corr_matrix)[idx[1]], "and", colnames(corr_matrix)[idx[2]],
        ": r =", corr_matrix[idx[1], idx[2]], "\n")
  })
} else {
  cat("No strong multicollinearity detected.\n")
}

# ==============================================================================
# SECTION 3: Raw outcome plot helper
# ==============================================================================

plot_raw_outcome <- function(panel, treated_province, treat_year = 2014) {

  avg_control <- panel %>%
    filter(province != treated_province) %>%
    group_by(year) %>%
    summarise(fti = mean(fti_pc, na.rm = TRUE),
              group = "Average Untreated", .groups = "drop")

  treated <- panel %>%
    filter(province == treated_province) %>%
    mutate(group = treated_province) %>%
    select(year, fti = fti_pc, group)

  plot_df <- bind_rows(avg_control, treated)

  color_values        <- c("gray65", "black")
  names(color_values) <- c("Average Untreated", treated_province)

  ggplot(plot_df, aes(x = year, y = fti, color = group)) +
    geom_line(linewidth = 1.3) +
    geom_vline(xintercept = treat_year, linetype = "dotted", linewidth = 1) +
    scale_color_manual(
      values = color_values,
      breaks = c(treated_province, "Average Untreated"),
      name   = NULL
    ) +
    labs(
      title    = paste("Raw Outcome:", treated_province %>%
                         stringr::str_remove(" Province$")),
      subtitle = "Foreign Total Investment per Capita",
      x        = "Year", y = "FTI pc"
    ) +
    theme(legend.position = "bottom")
}

plot_raw_outcome(ipc_panel, "Beijing")
ggsave("output/figures/fig_raw_outcome_beijing.png", width = 7, height = 5, dpi = 300)

plot_raw_outcome(ipc_panel, "Guangdong Province")
ggsave("output/figures/fig_raw_outcome_guangdong.png", width = 7, height = 5, dpi = 300)

cat("Saved: output/figures/fig_raw_outcome_*.png\n")
