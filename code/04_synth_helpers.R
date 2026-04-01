# ==============================================================================
# 04_synth_helpers.R
#
# Defines all helper functions used in the synthetic control analysis.
# This script is sourced by 05_synth_main.R and 06_robustness.R —
# it does not run any analysis on its own.
#
# Functions defined here:
#   run_synth_for()              — core SCM estimation
#   run_synth_for_alt_outcome()  — SCM for Y-variable placebo tests
#   extract_synth_series()       — extract actual/synthetic/gap series
#   plot_scm_path()              — path plot (shared y-axis ready)
#   plot_scm_gap()               — gap plot (shared y-axis ready)
#   plot_scm_path_custom()       — path plot with custom title/subtitle
#   plot_scm_gap_custom()        — gap plot with custom title/subtitle
#   plot_raw_outcome()           — raw outcome vs. average donor
#   plot_alt_outcome_placebo()   — path + gap for Y-variable placebo
#   plot_placebo_time_single()   — in-time placebo path plot
#   plot_mspe_ratio_dotplot()    — lollipop chart for MSPE ratios
#   placebo_space_all()          — spaghetti plot for in-space placebo
#   compute_mspe_ratio()         — loop over all provinces, compute ratios
#   get_corrected_estimates()    — aggregate dollar effects from SCM gap
#   shared_ylims()               — compute shared axis limits across runs
#   get_outlier_provinces()      — Tukey fence outlier detection
#
# Note: export_scm_to_gsheets() is defined in preprocessing_transparency_only.R
# for reference but is not used in the main replication scripts.
# ==============================================================================

# ---- run_synth_for -----------------------------------------------------------

run_synth_for <- function(panel, treated_province, treat_year = 2014,
                          years = 2000:2016, upload_to_gsheets = FALSE,
                          gs_title = NULL, drop_donor = NULL) {

  cat("\n==============================\n")
  cat("Running SCM for:", treated_province, "\n")
  cat("Treatment year:", treat_year, "\n")
  cat("==============================\n")

  predictors <- c(
    "gdp_log", "cpi", "num_priv_enterprise_pc", "registered_unemployed",
    "student_enrollment_pc", "disposable_income_urban_per_cap", "export_intensity"
  )

  outcome_var <- "fti_pc"

  needed_cols <- c("province", "year", "Court_Year", predictors, outcome_var)
  missing_cols <- setdiff(needed_cols, names(panel))
  if (length(missing_cols) > 0)
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))

  df <- panel %>%
    dplyr::filter(year %in% years) %>%
    dplyr::arrange(province, year) %>%
    dplyr::mutate(
      province_id = as.integer(factor(province, levels = sort(unique(province))))
    ) %>%
    dplyr::select(province_id, year, province, Court_Year,
                  dplyr::all_of(c(predictors, outcome_var)))

  treated_id <- df %>%
    dplyr::filter(province == treated_province) %>%
    dplyr::pull(province_id) %>%
    unique()

  if (length(treated_id) != 1)
    stop("Treated province ID not uniquely identified. Check spelling/duplicates.")

  if (!is.null(drop_donor)) {
    donor_ids_to_remove <- df %>%
      dplyr::filter(province %in% drop_donor) %>%
      dplyr::pull(province_id) %>%
      unique()
    df <- df %>% dplyr::filter(!province_id %in% donor_ids_to_remove)
  }

  treated_tyear_ids <- df %>%
    dplyr::filter(Court_Year == treat_year) %>%
    dplyr::pull(province_id) %>%
    unique()

  df <- df %>%
    dplyr::filter(!province_id %in% setdiff(treated_tyear_ids, treated_id)) %>%
    dplyr::filter(!dplyr::if_any(dplyr::all_of(predictors), is.na),
                  !is.na(.data[[outcome_var]]))

  counts       <- df %>% dplyr::count(province_id)
  complete_ids <- counts %>% dplyr::filter(n == length(years)) %>% dplyr::pull(province_id)
  df           <- df %>% dplyr::filter(province_id %in% complete_ids)

  if (!treated_id %in% df$province_id)
    stop("Treated province '", treated_province,
         "' was dropped during filtering (likely missing data).")

  ipc_df <- as.data.frame(df)

  dp <- Synth::dataprep(
    foo                   = ipc_df,
    predictors            = predictors,
    predictors.op         = "mean",
    time.predictors.prior = years[years < treat_year],
    dependent             = outcome_var,
    unit.variable         = "province_id",
    time.variable         = "year",
    treatment.identifier  = treated_id,
    controls.identifier   = setdiff(unique(ipc_df$province_id), treated_id),
    special.predictors    = list(list(outcome_var, (treat_year - 3):(treat_year - 1), "mean")),
    time.optimize.ssr     = years[years < treat_year],
    time.plot             = years
  )

  s.out <- Synth::synth(dp)

  Y1  <- dp$Y1plot
  Y1s <- dp$Y0plot %*% s.out$solution.w
  rmspe_df <- tibble::tibble(
    year      = dp$tag$time.plot,
    actual    = as.numeric(Y1),
    synthetic = as.numeric(Y1s)
  ) %>% dplyr::mutate(gap = actual - synthetic)

  mspe_pre  <- mean(rmspe_df$gap[rmspe_df$year <  treat_year]^2, na.rm = TRUE)
  mspe_post <- mean(rmspe_df$gap[rmspe_df$year >= treat_year]^2, na.rm = TRUE)

  cat("MSPE (Pre):", round(mspe_pre, 4), "| MSPE (Post):", round(mspe_post, 4), "\n")

  tables <- Synth::synth.tab(dataprep.res = dp, synth.res = s.out)

  list(
    dataprep         = dp,
    synth            = s.out,
    tables           = tables,
    mspe             = list(pre = mspe_pre, post = mspe_post),
    panel_used       = ipc_df,
    treated_province = treated_province
  )
}

# ---- run_synth_for_alt_outcome -----------------------------------------------

run_synth_for_alt_outcome <- function(panel, treated_province, outcome_var,
                                      treat_year = 2014, years = 2000:2016,
                                      drop_donor = NULL) {

  cat("\n==============================\n")
  cat("SCM for:", treated_province, "| Outcome:", outcome_var, "\n")
  cat("==============================\n")

  predictors <- c(
    "gdp_log", "cpi", "num_priv_enterprise_pc", "registered_unemployed",
    "student_enrollment_pc", "disposable_income_urban_per_cap"
  )
  predictors <- setdiff(predictors, outcome_var)

  needed_cols  <- c("province", "year", "Court_Year", predictors, outcome_var)
  missing_cols <- setdiff(needed_cols, names(panel))
  if (length(missing_cols) > 0)
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))

  df <- panel %>%
    dplyr::filter(year %in% years) %>%
    dplyr::mutate(province_id = as.numeric(factor(province))) %>%
    dplyr::select(province_id, year, province, Court_Year,
                  dplyr::all_of(c(predictors, outcome_var)))

  treated_id <- df %>%
    dplyr::filter(province == treated_province) %>%
    dplyr::pull(province_id) %>% unique()

  if (length(treated_id) != 1)
    stop("Treated province not uniquely identified")

  if (!is.null(drop_donor)) {
    donor_ids_to_remove <- df %>%
      dplyr::filter(province %in% drop_donor) %>%
      dplyr::pull(province_id) %>% unique()
    df <- df %>% dplyr::filter(!province_id %in% donor_ids_to_remove)
  }

  treated_tyear_ids <- df %>%
    dplyr::filter(Court_Year == treat_year) %>%
    dplyr::pull(province_id) %>% unique()

  df <- df %>%
    dplyr::filter(!province_id %in% setdiff(treated_tyear_ids, treated_id)) %>%
    dplyr::filter(!dplyr::if_any(dplyr::all_of(predictors), is.na),
                  !is.na(.data[[outcome_var]]))

  counts       <- df %>% dplyr::count(province_id)
  complete_ids <- counts %>% dplyr::filter(n == length(years)) %>% dplyr::pull(province_id)
  df           <- df %>% dplyr::filter(province_id %in% complete_ids)

  ipc_df <- as.data.frame(df)

  dp <- Synth::dataprep(
    foo                   = ipc_df,
    predictors            = predictors,
    predictors.op         = "mean",
    time.predictors.prior = years[years < treat_year],
    dependent             = outcome_var,
    unit.variable         = "province_id",
    time.variable         = "year",
    treatment.identifier  = treated_id,
    controls.identifier   = setdiff(unique(ipc_df$province_id), treated_id),
    special.predictors    = list(list(outcome_var, (treat_year - 3):(treat_year - 1), "mean")),
    time.optimize.ssr     = years[years < treat_year],
    time.plot             = years
  )

  s.out <- Synth::synth(dp)

  gap      <- dp$Y1plot - dp$Y0plot %*% s.out$solution.w
  yrs      <- dp$tag$time.plot
  mspe_pre  <- mean(gap[yrs <  treat_year]^2, na.rm = TRUE)
  mspe_post <- mean(gap[yrs >= treat_year]^2, na.rm = TRUE)

  cat("MSPE (Pre):", round(mspe_pre, 4), "| MSPE (Post):", round(mspe_post, 4),
      "| Ratio:", round(mspe_post / mspe_pre, 2), "\n")

  list(
    dataprep         = dp,
    synth            = s.out,
    mspe             = list(pre = mspe_pre, post = mspe_post,
                            ratio = mspe_post / mspe_pre),
    panel_used       = ipc_df,
    treated_province = treated_province,
    outcome_var      = outcome_var
  )
}

# ---- extract_synth_series ----------------------------------------------------

extract_synth_series <- function(res) {
  dp <- res$dataprep
  s  <- res$synth
  tibble::tibble(
    year      = dp$tag$time.plot,
    actual    = as.numeric(dp$Y1plot),
    synthetic = as.numeric(dp$Y0plot %*% s$solution.w),
    gap       = as.numeric(dp$Y1plot - (dp$Y0plot %*% s$solution.w)),
    province  = res$treated_province
  )
}

# ---- plot_scm_path -----------------------------------------------------------

plot_scm_path <- function(res, ylim = NULL) {
  dp   <- res$dataprep
  syn  <- res$synth
  prov <- res$treated_province
  treat_yr <- max(dp$tag$time.optimize.ssr) + 1

  short_prov      <- prov %>% stringr::str_remove(" Province$") %>%
    stringr::str_remove(" Autonomous Region.*$")
  label_treated   <- paste0(short_prov, " (Treated)")
  label_synthetic <- paste0("Synthetic ", short_prov)

  df <- tibble::tibble(
    year = dp$tag$time.plot,
    actual = as.numeric(dp$Y1plot),
    synthetic = as.numeric(dp$Y0plot %*% syn$solution.w)
  ) %>%
    tidyr::pivot_longer(c(actual, synthetic), names_to = "series", values_to = "value") %>%
    dplyr::mutate(series = dplyr::case_when(
      series == "actual"    ~ label_treated,
      series == "synthetic" ~ label_synthetic
    ))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = value,
                                         color = series, linetype = series)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_vline(xintercept = treat_yr, linetype = "dotted") +
    ggplot2::scale_color_manual(
      values = setNames(c("black", "gray55"), c(label_treated, label_synthetic)),
      name = NULL) +
    ggplot2::scale_linetype_manual(
      values = setNames(c("solid", "dashed"), c(label_treated, label_synthetic)),
      name = NULL) +
    ggplot2::labs(
      title = paste0("Synthetic Control: ", short_prov, " (", treat_yr, ")"),
      x = "Year", y = "FTI pc (USD)") +
    theme_ipc() +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.text      = ggplot2::element_text(size = 9),
      legend.key.width = ggplot2::unit(1.5, "cm"),
      plot.title       = ggplot2::element_text(size = 11),
      axis.title       = ggplot2::element_text(size = 10),
      axis.text        = ggplot2::element_text(size = 9)
    )

  if (!is.null(ylim)) p <- p + ggplot2::ylim(ylim)
  print(p)
}

# ---- plot_scm_gap ------------------------------------------------------------

plot_scm_gap <- function(res, ylim = NULL) {
  dp   <- res$dataprep
  syn  <- res$synth
  prov <- res$treated_province
  treat_yr <- max(dp$tag$time.optimize.ssr) + 1

  short_prov <- prov %>% stringr::str_remove(" Province$") %>%
    stringr::str_remove(" Autonomous Region.*$")

  gap <- as.numeric(dp$Y1plot) - as.numeric(dp$Y0plot %*% syn$solution.w)
  df  <- tibble::tibble(year = dp$tag$time.plot, gap = gap)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = gap)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = treat_yr, linetype = "dotted") +
    ggplot2::labs(
      title    = paste0("Gap: ", short_prov, " vs. Synthetic (", treat_yr, ")"),
      subtitle = paste0("Post/Pre MSPE Ratio = ",
                        round(res$mspe$post / res$mspe$pre, 2)),
      x = "Year", y = "Gap (Actual \u2212 Synthetic)") +
    theme_ipc() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 11),
      plot.subtitle = ggplot2::element_text(size = 9),
      axis.title    = ggplot2::element_text(size = 10),
      axis.text     = ggplot2::element_text(size = 9)
    )

  if (!is.null(ylim)) p <- p + ggplot2::ylim(ylim)
  print(p)
}

# ---- plot_scm_path_custom / plot_scm_gap_custom ------------------------------

plot_scm_path_custom <- function(res, treat_yr, title, subtitle = NULL, ylim = NULL) {
  dp  <- res$dataprep; syn <- res$synth; prov <- res$treated_province
  short_prov <- prov |> stringr::str_remove(" Province$") |>
    stringr::str_remove(" Autonomous Region.*$")

  label_actual    <- paste0(short_prov, " (Treated)")
  label_synthetic <- paste0("Synthetic ", short_prov)

  df <- tibble::tibble(
    year = dp$tag$time.plot,
    actual = as.numeric(dp$Y1plot),
    synthetic = as.numeric(dp$Y0plot %*% syn$solution.w)
  ) |>
    tidyr::pivot_longer(c(actual, synthetic), names_to = "series", values_to = "value") |>
    dplyr::mutate(series = dplyr::case_when(
      series == "actual"    ~ label_actual,
      series == "synthetic" ~ label_synthetic))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = value,
                                         color = series, linetype = series)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_vline(xintercept = treat_yr, linetype = "dotted", linewidth = 0.8) +
    ggplot2::scale_color_manual(
      values = stats::setNames(c("black", "gray55"), c(label_actual, label_synthetic)),
      name = NULL) +
    ggplot2::scale_linetype_manual(
      values = stats::setNames(c("solid", "dashed"), c(label_actual, label_synthetic)),
      name = NULL) +
    ggplot2::labs(title = title, subtitle = subtitle, x = "Year", y = "FTI pc (USD)") +
    theme_ipc() +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.text      = ggplot2::element_text(size = 9),
      legend.key.width = ggplot2::unit(1.5, "cm"),
      plot.title       = ggplot2::element_text(size = 11),
      plot.subtitle    = ggplot2::element_text(size = 9, color = "gray40"),
      axis.title       = ggplot2::element_text(size = 10),
      axis.text        = ggplot2::element_text(size = 9)
    )

  if (!is.null(ylim)) p <- p + ggplot2::coord_cartesian(ylim = ylim)
  print(p); invisible(p)
}

plot_scm_gap_custom <- function(res, treat_yr, title, subtitle = NULL, ylim = NULL) {
  dp <- res$dataprep; syn <- res$synth
  ratio <- res$mspe$post / res$mspe$pre
  gap <- as.numeric(dp$Y1plot) - as.numeric(dp$Y0plot %*% syn$solution.w)
  df  <- tibble::tibble(year = dp$tag$time.plot, gap = gap)
  sub <- if (!is.null(subtitle)) subtitle else
    paste0("Post/Pre MSPE Ratio = ", round(ratio, 2))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = gap)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = treat_yr, linetype = "dotted", linewidth = 0.8) +
    ggplot2::labs(title = title, subtitle = sub,
                  x = "Year", y = "Gap (Actual \u2212 Synthetic)") +
    theme_ipc() +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 11),
      plot.subtitle = ggplot2::element_text(size = 9, color = "gray40"),
      axis.title    = ggplot2::element_text(size = 10),
      axis.text     = ggplot2::element_text(size = 9)
    )

  if (!is.null(ylim)) p <- p + ggplot2::coord_cartesian(ylim = ylim)
  print(p); invisible(p)
}

# ---- plot_alt_outcome_placebo ------------------------------------------------

plot_alt_outcome_placebo <- function(res) {
  dp <- res$dataprep; synth <- res$synth
  years_vec        <- dp$tag$time.plot
  actual           <- as.numeric(dp$Y1plot)
  synthetic_series <- as.numeric(dp$Y0plot %*% synth$solution.w)
  gap              <- actual - synthetic_series

  df <- tibble::tibble(year = years_vec, actual = actual,
                       synthetic = synthetic_series, gap = gap)

  short_prov <- res$treated_province %>%
    stringr::str_remove(" Province$") %>% stringr::str_remove(" Autonomous Region.*$")
  short_outcome <- res$outcome_var %>%
    stringr::str_replace("disposable_income_urban_per_cap", "Urban Income pc") %>%
    stringr::str_replace("gdp_log",                        "Log GDP") %>%
    stringr::str_replace("num_priv_enterprise_pc",         "Private Ent. pc") %>%
    stringr::str_replace("student_enrollment_pc",          "Enrollment pc")

  label_theme <- ggplot2::theme(
    plot.title    = ggplot2::element_text(size = 11),
    plot.subtitle = ggplot2::element_text(size = 9),
    axis.title    = ggplot2::element_text(size = 10),
    axis.text     = ggplot2::element_text(size = 9)
  )

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = year)) +
    ggplot2::geom_line(ggplot2::aes(y = actual),    color = "black",  linewidth = 1.2) +
    ggplot2::geom_line(ggplot2::aes(y = synthetic), color = "gray60", linewidth = 1.2) +
    ggplot2::geom_vline(xintercept = 2014, linetype = "dotted") +
    ggplot2::labs(title    = paste0("Y-Placebo: ", short_prov, " \u2014 ", short_outcome),
                  subtitle = "Black = Actual, Gray = Synthetic",
                  x = "Year", y = short_outcome) +
    theme_ipc() + label_theme

  p2 <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = gap)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = 2014, linetype = "dotted") +
    ggplot2::labs(title    = paste0("Gap: ", short_prov, " \u2014 ", short_outcome),
                  subtitle = paste("MSPE Ratio:", round(res$mspe$ratio, 2)),
                  x = "Year", y = "Gap (Actual \u2212 Synthetic)") +
    theme_ipc() + label_theme

  print(p1); print(p2); invisible(res)
}

# ---- plot_placebo_time_single ------------------------------------------------

plot_placebo_time_single <- function(panel, treated_province, fake_year = 2008,
                                     drop_donor = NULL, years = 2000:2016) {
  res_fake <- run_synth_for(panel, treated_province = treated_province,
                            treat_year = fake_year, years = years,
                            drop_donor = drop_donor, upload_to_gsheets = FALSE)
  df_fake <- extract_synth_series(res_fake)

  short_prov      <- treated_province %>% stringr::str_remove(" Province$") %>%
    stringr::str_remove(" Autonomous Region.*$")
  label_actual    <- paste0(short_prov, " (Actual)")
  label_synthetic <- paste0("Synthetic ", short_prov)

  df_plot <- df_fake %>%
    tidyr::pivot_longer(c(actual, synthetic), names_to = "series", values_to = "value") %>%
    dplyr::mutate(series = dplyr::case_when(
      series == "actual"    ~ label_actual,
      series == "synthetic" ~ label_synthetic))

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = year, y = value,
                                              color = series, linetype = series)) +
    ggplot2::geom_line(linewidth = 1.3) +
    ggplot2::geom_vline(xintercept = fake_year, linetype = "dotted") +
    ggplot2::scale_color_manual(
      values = setNames(c("black", "black"), c(label_actual, label_synthetic)),
      name = NULL) +
    ggplot2::scale_linetype_manual(
      values = setNames(c("solid", "dashed"), c(label_actual, label_synthetic)),
      name = NULL) +
    ggplot2::labs(
      title = paste0("In-Time Placebo: ", short_prov,
                     " (Fake Year = ", fake_year, ")"),
      x = "Year", y = "FTI pc") +
    theme_ipc() +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.text      = ggplot2::element_text(size = 9),
      legend.key.width = ggplot2::unit(1.5, "cm"),
      plot.title       = ggplot2::element_text(size = 11),
      axis.title       = ggplot2::element_text(size = 10),
      axis.text        = ggplot2::element_text(size = 9)
    )
  print(p)
}

# ---- compute_mspe_ratio ------------------------------------------------------

compute_mspe_ratio <- function(panel, treat_year = 2014, years = 2000:2016,
                               exclude_provinces = NULL) {
  if (!is.null(exclude_provinces))
    panel <- panel %>% dplyr::filter(!province %in% exclude_provinces)

  provinces <- unique(panel$province)
  results   <- list()

  for (prov in provinces) {
    message("Placebo: ", prov)
    out <- try(run_synth_for(panel, prov, treat_year = treat_year), silent = TRUE)
    if (inherits(out, "try-error")) next

    dp <- out$dataprep; synth <- out$synth
    gap <- dp$Y1plot - dp$Y0plot %*% synth$solution.w
    yrs <- dp$tag$time.plot

    mspe_pre  <- mean(gap[yrs < treat_year]^2, na.rm = TRUE)
    mspe_post <- mean(gap[yrs >= treat_year]^2, na.rm = TRUE)

    results[[prov]] <- tibble::tibble(
      province  = prov,
      mspe_pre  = mspe_pre,
      mspe_post = mspe_post,
      ratio     = mspe_post / mspe_pre
    )
  }
  dplyr::bind_rows(results)
}

# ---- placebo_space_all -------------------------------------------------------

placebo_space_all <- function(panel, treated_provinces,
                              treat_year = 2014, years = 2000:2016,
                              mspe_mult  = 2) {
  provinces <- unique(panel$province)
  gap_list  <- list()

  for (prov in provinces) {
    message("In-space placebo: ", prov)
    out <- try(run_synth_for(panel, prov, treat_year = treat_year), silent = TRUE)
    if (inherits(out, "try-error")) next

    dp <- out$dataprep; synth <- out$synth
    Y1  <- dp$Y1plot
    Y0w <- dp$Y0plot %*% synth$solution.w
    gap <- as.numeric(Y1 - Y0w)
    yrs <- dp$tag$time.plot

    mspe_pre <- mean(gap[yrs < treat_year]^2, na.rm = TRUE)

    gap_list[[prov]] <- tibble::tibble(
      year     = yrs,
      gap      = gap,
      unit     = prov,
      mspe_pre = mspe_pre,
      label    = ifelse(prov %in% treated_provinces, "Treated", "Donor")
    )
  }

  all_gaps <- dplyr::bind_rows(gap_list)

  treated_mspe <- all_gaps %>%
    dplyr::filter(label == "Treated") %>%
    dplyr::distinct(unit, mspe_pre) %>%
    dplyr::summarise(avg = mean(mspe_pre, na.rm = TRUE)) %>%
    dplyr::pull(avg)

  all_gaps_filt <- all_gaps %>%
    dplyr::filter(label == "Treated" | mspe_pre <= mspe_mult * treated_mspe)

  color_map <- c("Beijing" = "black", "Guangdong Province" = "black")

  ggplot2::ggplot(all_gaps_filt, ggplot2::aes(x = year, y = gap, group = unit)) +
    ggplot2::geom_line(data = all_gaps_filt %>% dplyr::filter(label == "Donor"),
                       color = "gray75", alpha = 0.5, linewidth = 0.7) +
    ggplot2::geom_line(data = all_gaps_filt %>% dplyr::filter(label == "Treated"),
                       ggplot2::aes(color = unit), linewidth = 1.2) +
    ggplot2::scale_color_manual(values = color_map) +
    ggplot2::geom_vline(xintercept = treat_year, linetype = "dotted") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    ggplot2::labs(
      title    = "Placebo-in-Space Test: Spaghetti Plot",
      subtitle = paste0("Gray = donors filtered at ", mspe_mult, "\u00d7 treated pre-MSPE."),
      x = "Year", y = "Gap (Actual \u2212 Synthetic)", color = NULL) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(size = 11),
      plot.subtitle   = ggplot2::element_text(size = 9),
      legend.position = "bottom",
      legend.text     = ggplot2::element_text(size = 9),
      axis.title      = ggplot2::element_text(size = 10),
      axis.text       = ggplot2::element_text(size = 9)
    )
}

# ---- plot_mspe_ratio_dotplot -------------------------------------------------

plot_mspe_ratio_dotplot <- function(mspe_ratios,
                                    treated_label = "Beijing",
                                    title    = "Post/Pre MSPE Ratios: Beijing vs. Placebo Provinces",
                                    subtitle = "Placebo inference across donor pool (Anhui Province excluded)",
                                    xlab     = "Post/Pre MSPE Ratio",
                                    treated_color = "black", donor_color = "black",
                                    treated_shape = 17, donor_shape = 15,
                                    point_size = 3, exclude = NULL) {
  plot_df <- mspe_ratios %>%
    dplyr::filter(!province %in% exclude) %>%
    dplyr::mutate(
      label = province %>%
        stringr::str_remove(" Province$") %>%
        stringr::str_remove(" Autonomous Region$") %>%
        stringr::str_remove(" Zhuang$") %>%
        stringr::str_remove(" Hui$") %>%
        stringr::str_remove(" Uyghur$") %>%
        stringr::str_remove(" Autonomous$"),
      label     = forcats::fct_reorder(label, ratio),
      unit_type = dplyr::if_else(treated, "Treated", "Donor")
    )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = ratio, y = label,
                                         shape = unit_type, color = unit_type)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = ratio, y = label, yend = label),
                          color = "gray85", linewidth = 0.4) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::scale_shape_manual(
      values = c("Treated" = treated_shape, "Donor" = donor_shape), name = NULL) +
    ggplot2::scale_color_manual(
      values = c("Treated" = treated_color, "Donor" = donor_color), name = NULL) +
    ggplot2::scale_x_continuous(
      limits  = c(0, max(plot_df$ratio, na.rm = TRUE) * 1.08),
      expand  = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = NULL) +
    theme_ipc() +
    ggplot2::theme(
      legend.position    = "bottom",
      legend.text        = ggplot2::element_text(size = 9),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "gray90", linewidth = 0.4),
      axis.text.y        = ggplot2::element_text(size = 8),
      axis.text.x        = ggplot2::element_text(size = 9),
      plot.title         = ggplot2::element_text(size = 11),
      plot.subtitle      = ggplot2::element_text(size = 9, color = "gray40"),
      axis.title         = ggplot2::element_text(size = 10)
    )
}

# ---- get_corrected_estimates -------------------------------------------------

get_corrected_estimates <- function(res, panel, post_years = 2014:2016) {
  province_name <- res$treated_province
  dp    <- res$dataprep; synth <- res$synth

  years_vec <- as.numeric(dp$tag$time.plot)
  actual    <- as.numeric(dp$Y1plot)
  synthetic <- as.numeric(dp$Y0plot %*% synth$solution.w)
  gap_pc    <- actual - synthetic

  gap_df <- tibble::tibble(year = years_vec, actual = actual,
                           synthetic = synthetic, gap_pc = gap_pc) %>%
    dplyr::filter(year %in% post_years)

  pop_df <- panel %>%
    dplyr::filter(province == province_name, year %in% post_years) %>%
    dplyr::select(year, population_province)

  est_df <- gap_df %>%
    dplyr::left_join(pop_df, by = "year") %>%
    dplyr::mutate(
      province          = province_name,
      aggregate_10kUSD  = gap_pc * population_province,
      aggregate_mUSD    = aggregate_10kUSD / 100,
      aggregate_bUSD    = aggregate_10kUSD / 100000
    ) %>%
    dplyr::select(province, year, actual, synthetic, gap_pc,
                  population_province, aggregate_mUSD, aggregate_bUSD)

  cumulative_bUSD <- sum(est_df$aggregate_bUSD, na.rm = TRUE)
  cumulative_mUSD <- sum(est_df$aggregate_mUSD, na.rm = TRUE)
  avg_gap_pc      <- mean(est_df$gap_pc, na.rm = TRUE)

  cat("\n", paste(rep("=", 58), collapse = ""), "\n")
  cat("  CORRECTED ESTIMATES:", province_name, "\n")
  cat(paste(rep("=", 58), collapse = ""), "\n")
  print(est_df %>%
          dplyr::mutate(gap_pc         = round(gap_pc, 2),
                        aggregate_mUSD = round(aggregate_mUSD, 1),
                        aggregate_bUSD = round(aggregate_bUSD, 3)),
        n = Inf)
  cat("\n  Cumulative 2014-2016:\n")
  cat("    Total additional FTI:", round(cumulative_mUSD, 1),
      "million USD  (", round(cumulative_bUSD, 2), "billion USD )\n")
  cat("    Avg annual per-capita gap:", round(avg_gap_pc, 2), "USD per person\n\n")

  invisible(list(yearly = est_df, cumulative_bUSD = cumulative_bUSD,
                 cumulative_mUSD = cumulative_mUSD, avg_gap_pc = avg_gap_pc))
}

# ---- export_scm_to_gsheets ---------------------------------------------------
# NOTE: Requires active Google authentication. See 01_setup.R for auth details.

export_scm_to_gsheets <- function(res, panel, treat_year = 2014, gs_title) {
  treated_province <- res$treated_province
  dp    <- res$dataprep; synth <- res$synth
  if (missing(gs_title) || is.null(gs_title))
    gs_title <- paste0(treated_province, "_SCM_Results")

  id_name_map <- res$panel_used %>% dplyr::distinct(province_id, province)

  donor_weights_all <- res$tables$tab.w %>%
    tibble::as_tibble(rownames = "province_id") %>%
    dplyr::mutate(province_id = as.numeric(province_id)) %>%
    dplyr::left_join(id_name_map, by = "province_id") %>%
    dplyr::rename(weight = w.weights) %>%
    dplyr::select(province_id, province, weight) %>%
    dplyr::arrange(dplyr::desc(weight))

  tp       <- res$tables$tab.pred
  pred_tbl <- tibble::tibble(
    predictor   = rownames(tp),
    treated     = as.numeric(tp[, "Treated"]),
    synthetic   = as.numeric(tp[, "Synthetic"]),
    sample_mean = as.numeric(tp[, "Sample Mean"])
  ) %>%
    dplyr::mutate(abs_gap_treat_synth = abs(treated - synthetic),
                  gap_treat_synth     = treated - synthetic) %>%
    dplyr::arrange(dplyr::desc(abs_gap_treat_synth))

  years_vec        <- dp$tag$time.plot
  actual           <- as.numeric(dp$Y1plot)
  synthetic_series <- as.numeric(dp$Y0plot %*% synth$solution.w)
  gap              <- actual - synthetic_series

  gap_tbl <- tibble::tibble(
    province = treated_province, year = years_vec, actual = actual,
    synthetic = synthetic_series, gap = gap, gap_sq = gap^2,
    period = dplyr::if_else(years_vec < treat_year, "pre", "post")
  )

  mspe_pre  <- mean(gap[years_vec <  treat_year]^2, na.rm = TRUE)
  mspe_post <- mean(gap[years_vec >= treat_year]^2, na.rm = TRUE)

  metrics_tbl <- tibble::tibble(
    province = treated_province, treat_year = treat_year,
    metric = c("MSPE_Pre", "MSPE_Post"),
    value  = c(mspe_pre, mspe_post)
  )

  ss <- googlesheets4::gs4_create(gs_title)
  googlesheets4::sheet_write(pred_tbl,                                   ss = ss, sheet = "Predictor_Balance")
  googlesheets4::sheet_write(donor_weights_all %>% filter(weight > 0),   ss = ss, sheet = "Donor_Weights_Used")
  googlesheets4::sheet_write(donor_weights_all %>% filter(weight == 0),  ss = ss, sheet = "Donor_Weights_Unused")
  googlesheets4::sheet_write(metrics_tbl,                                ss = ss, sheet = "SCM_Metrics")
  googlesheets4::sheet_write(gap_tbl,                                    ss = ss, sheet = "Gap_by_Year")

  sheet_url <- tryCatch(ss$spreadsheet_url,
                        error = function(e) tryCatch(ss$browser_url,
                                                     error = function(e2) NA))
  cat("Export complete:", treated_province, "\n")
  cat("Google Sheet:", sheet_url, "\n")
  invisible(list(sheet_url = sheet_url, predictor_balance = pred_tbl,
                 metrics = metrics_tbl, gaps = gap_tbl))
}

# ---- shared_ylims ------------------------------------------------------------

shared_ylims <- function(res_list) {
  fti_vals <- unlist(lapply(res_list, function(r) {
    c(as.numeric(r$dataprep$Y1plot),
      as.numeric(r$dataprep$Y0plot %*% r$synth$solution.w))
  }))
  gap_vals <- unlist(lapply(res_list, function(r) {
    as.numeric(r$dataprep$Y1plot) -
      as.numeric(r$dataprep$Y0plot %*% r$synth$solution.w)
  }))
  list(
    fti = c(0, max(fti_vals, na.rm = TRUE) * 1.05),
    gap = c(min(gap_vals, na.rm = TRUE) * 1.15,
            max(gap_vals, na.rm = TRUE) * 1.15)
  )
}

# ---- get_outlier_provinces ---------------------------------------------------

get_outlier_provinces <- function(mspe_ratios, treated_name = "Beijing") {
  Q1      <- quantile(mspe_ratios$ratio, 0.25, na.rm = TRUE)
  Q3      <- quantile(mspe_ratios$ratio, 0.75, na.rm = TRUE)
  upper   <- Q3 + 1.5 * (Q3 - Q1)
  outlier_provs <- mspe_ratios %>%
    dplyr::filter(ratio > upper) %>%
    dplyr::pull(province) %>%
    unique()
  list(
    upper_cutoff = upper,
    outliers     = outlier_provs,
    highlighted  = unique(c(treated_name, outlier_provs))
  )
}

cat("Helper functions loaded from 03_synth_helpers.R\n")
