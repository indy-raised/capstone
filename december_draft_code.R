# SECTION 1: Setup — Packages
# Centralize installs/imports for clarity and reproducibility.
# -------------------------------

rm(list = ls())

install_if_missing <- function(pkgs) {
  to_install <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
  if (length(to_install)) install.packages(to_install)
}

pkgs <- c(
  "haven","tidyverse","stringi","googlesheets4","Synth",
  "did","fixest","ggplot2","readr","tidyr","stringr"
)
install_if_missing(pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))


# -------------------------------
# SECTION 2: IPC city data
# Read Stata → CSV, translate provinces, create pinyin city names,
# QC missing translations, mark controls, save translated IPC CSV.
# -------------------------------

ipc_dta_path <- "~/Downloads/IPC_CITY.dta"
data <- read_dta(ipc_dta_path)

write.csv(data, "IPC_CITY_converted.csv", row.names = FALSE)
cat("✅ CSV saved to:", file.path(getwd(), "IPC_CITY_converted.csv"), "\n")

province_translate <- c(
  "北京市"="Beijing","天津市"="Tianjin","上海市"="Shanghai","重庆市"="Chongqing",
  "河北省"="Hebei Province","山西省"="Shanxi Province","辽宁省"="Liaoning Province",
  "吉林省"="Jilin Province","黑龙江省"="Heilongjiang Province","江苏省"="Jiangsu Province",
  "浙江省"="Zhejiang Province","安徽省"="Anhui Province","福建省"="Fujian Province",
  "江西省"="Jiangxi Province","山东省"="Shandong Province","河南省"="Henan Province",
  "湖北省"="Hubei Province","湖南省"="Hunan Province","广东省"="Guangdong Province",
  "海南省"="Hainan Province","四川省"="Sichuan Province","贵州省"="Guizhou Province",
  "云南省"="Yunnan Province","陕西省"="Shaanxi Province","甘肃省"="Gansu Province",
  "青海省"="Qinghai Province","台湾省"="Taiwan Province","内蒙古自治区"="Inner Mongolia Autonomous Region",
  "广西壮族自治区"="Guangxi Zhuang Autonomous Region","西藏自治区"="Tibet Autonomous Region",
  "宁夏回族自治区"="Ningxia Hui Autonomous Region","新疆维吾尔自治区"="Xinjiang Uyghur Autonomous Region"
)

data <- data %>%
  mutate(
    province_english = province_translate[province_name]
  )

missing_prov <- unique(data$province_name[is.na(data$province_english)])
if (length(missing_prov) > 0) {
  cat("⚠️ Missing province translations:\n")
  print(missing_prov)
} else {
  cat("✅ All provinces translated.\n")
}

# Pinyin city labels (no tones), minor cleanup (Xi'an, etc.)
drop_suffix <- function(x) gsub("(市|州|盟|地区|自治州)$", "", x)
to_pinyin_name <- function(x) {
  x2 <- drop_suffix(x)
  p <- stringi::stri_trans_general(x2, "Han-Latin/Names; Latin-ASCII")
  p <- gsub("\\s*'\\s*", "'", p)
  tools::toTitleCase(trimws(p))
}
data <- data %>%
  mutate(
    city_english = to_pinyin_name(city_name),
    Court_Year = suppressWarnings(as.integer(Court_Year)),
    control_flag = ifelse(is.na(Court_Year), 1L, 0L)
  )

fixes <- c("Xi'An"="Xi'an","Urumqi"="Urumqi","Ordos"="Ordos")
data$city_english <- ifelse(data$city_english %in% names(fixes),
                            fixes[data$city_english], data$city_english)

weird <- unique(data$city_english[grepl("[^A-Za-z '\\-]", data$city_english)])
if (length(weird)) {
  cat("⚠️ Check these city labels (non-ASCII found):\n"); print(weird)
} else {
  cat("✅ City pinyin labels created.\n")
}

controls <- data %>%
  filter(is.na(Court_Year) | Court_Year == "") %>%
  distinct(province_name, province_english)
cat("📊 Provinces without IPC establishment (control group):\n"); print(controls)

write.csv(data, "IPC_CITY_translated_provinces_final.csv", row.names = FALSE)
cat("📁 Translated CSV saved to:", file.path(getwd(), "IPC_CITY_translated_provinces_final.csv"), "\n")

head(data[, c("province_name","province_english","city_name","Court_Year","year")])

# Quick extracts
cities_2014 <- data %>%
  filter(Court_Year == 2014) %>%
  distinct(province_english, city_english, Court_Year) %>%
  arrange(province_english, city_english)
cat("\n📅 Cities with IPC implementation in 2014:\n"); print(cities_2014, n = nrow(cities_2014))

provinces_multiple_years <- data %>%
  filter(!is.na(Court_Year)) %>%
  group_by(province_english) %>%
  summarise(
    n_distinct_years = n_distinct(Court_Year),
    years = paste(sort(unique(Court_Year)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_distinct_years > 1) %>%
  arrange(province_english)
cat("\n🔁 Provinces with multiple different IPC implementation years across cities:\n")
print(provinces_multiple_years, n = nrow(provinces_multiple_years))

provinces_2017_2020 <- data %>%
  filter(Court_Year %in% 2017:2020) %>%
  distinct(Court_Year, province_english) %>%
  arrange(Court_Year, province_english)
cat("\n📆 Provinces with IPC implementations in 2017–2020:\n")
print(provinces_2017_2020, n = nrow(provinces_2017_2020))


# -------------------------------
# SECTION 3: Covariates from Google Sheets
# Auth read-only, fetch all tabs, pivot long, join vertically, save CSV.
# -------------------------------

googlesheets4::gs4_deauth()
googlesheets4::gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")

sheet_url <- "https://docs.google.com/spreadsheets/d/1pXywZSiJvs4PrE6Vwrle2zO--lYMQLn7Yg7FtmtiK4c/edit?usp=sharing"
all_sheets <- googlesheets4::sheet_names(sheet_url)
print(all_sheets)

combined_covariates <- NULL

for (sheet in all_sheets) {
  cat("Processing sheet:", sheet, "\n")
  df_temp <- googlesheets4::read_sheet(sheet_url, sheet = sheet, skip = 3)
  var_name <- make.names(sheet)
  province_col <- if ("Region" %in% names(df_temp)) "Region" else "area"
  
  df_long <- df_temp %>%
    tidyr::pivot_longer(
      cols = -all_of(province_col),
      names_to = "year",
      values_to = var_name
    ) %>%
    dplyr::mutate(
      year = as.numeric(year),
      province = .data[[province_col]]
    ) %>%
    dplyr::select(province, year, all_of(var_name))
  
  combined_covariates <- if (is.null(combined_covariates)) df_long else
    dplyr::full_join(combined_covariates, df_long, by = c("province","year"))
}

readr::write_csv(combined_covariates, "combined_covariates.csv")
cat("✅ Combined covariate dataset saved to:", file.path(getwd(), "combined_covariates.csv"), "\n")


# -------------------------------
# SECTION 4: Merge & Panel Build (province-year)
# Merge IPC city → province labels with covariates, compute rollout timing,
# treated/ever_treated/rel_time, and save panel (CSV + RDS).
# -------------------------------

ipc_data <- readr::read_csv("IPC_CITY_translated_provinces_final.csv")
covariates <- readr::read_csv("combined_covariates.csv")

ipc_merged <- ipc_data %>%
  mutate(
    province = stringr::str_trim(province_english),
    year = as.numeric(year)
  ) %>%
  left_join(covariates, by = c("province","year"))

glimpse(ipc_merged)
readr::write_csv(ipc_merged, "ipc_with_covariates_citymerge.csv")
cat("✅ Intermediate merged (city-level join) saved.\n")

# Recompute rollout at province level
ipc_city <- readr::read_csv("IPC_CITY_translated_provinces_final.csv") %>%
  mutate(
    province = coalesce(province_english, province_name),
    Court_Year = suppressWarnings(as.integer(Court_Year))
  )

ipc_rollout_prov <- ipc_city %>%
  filter(!is.na(Court_Year)) %>%
  group_by(province) %>%
  summarise(Court_Year = min(Court_Year), .groups = "drop")

norm_name <- function(x) x %>% str_trim() %>% str_replace_all("\\s+", " ")

covariates <- covariates %>% mutate(province = norm_name(province))
ipc_rollout_prov <- ipc_rollout_prov %>% mutate(province = norm_name(province))

cat("\n🔎 Provinces in IPC but not in covariates:\n")
print(setdiff(ipc_rollout_prov$province, covariates$province))
cat("\n🔎 Provinces in covariates but not in IPC:\n")
print(setdiff(covariates$province, ipc_rollout_prov$province))

ipc_panel <- covariates %>%
  left_join(ipc_rollout_prov, by = "province") %>%
  mutate(
    treated = ifelse(!is.na(Court_Year) & year >= Court_Year, 1L, 0L),
    ever_treated = ifelse(!is.na(Court_Year), 1L, 0L),
    rel_time = ifelse(!is.na(Court_Year), year - Court_Year, NA_integer_),
    fti_per_capita = foreign_total_investment / population_province
  )

# Add this right after you create ipc_panel
ipc_panel <- ipc_panel %>%
  filter(
    !is.na(province),
    !grepl("Data Source", province, ignore.case = TRUE),
    !grepl("Note:", province, ignore.case = TRUE),
    !grepl("£º|：", province),  # Remove malformed characters
    province != "",
    nchar(province) > 2  # Province names should be reasonably long
  )

write_csv(ipc_panel, "ipc_with_covariates.csv")
saveRDS(ipc_panel, "ipc_with_covariates.rds")
cat("✅ Final merged panel saved to:", file.path(getwd(), "ipc_with_covariates.csv"), "\n")

# Quick sanity peek
ipc_panel %>%
  filter(province %in% c("Beijing","Shanghai","Guangdong Province")) %>%
  select(province, year, Court_Year, treated, rel_time) %>%
  arrange(province, year) %>%
  print(n = 30)

ipc_panel %>%
  select(year, province,
         fti_per_capita, gdp_province, population_province,
         num_priv_enterprise, student_enrollment,
         disposable_income_urban_per_cap) %>%
  pivot_longer(cols = -c(year, province), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = year, y = value, group = province)) +
  geom_line(alpha = 0.3) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Do variables vary over time?", y = "Value", x = "Year")

# -------------------------------
# SECTION 4B: Correlation Check for Covariates
# Before regression or synthetic control, assess multicollinearity
# -------------------------------

# Select numeric covariates and the outcome variable
covariate_vars <- c(
  "foreign_total_investment",
  "gdp_province",
  "population_province",
  "num_priv_enterprise",
  "registered_unemployed",
  "cpi",
  "student_enrollment",
  "disposable_income_rural_per_cap",
  "disposable_income_urban_per_cap"
)

# Keep only complete cases for these columns
corr_data <- ipc_panel %>%
  dplyr::select(all_of(covariate_vars)) %>%
  dplyr::filter(if_all(everything(), ~ !is.na(.)))

# Compute correlation matrix
corr_matrix <- round(cor(corr_data, use = "pairwise.complete.obs"), 2)

cat("📊 Correlation Matrix of Outcome and Covariates:\n")
print(corr_matrix)

# Visualize correlations as a heatmap
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
library(corrplot)

# Adjust device size and margins before plotting
par(mar = c(2, 5, 3, 5), oma = c(1, 1, 1, 1), pty = "s")

# Optional: shorter names for readability
short_names <- c(
  "FTI", "GDP", "Pop", "PrivEnt", "Unemp", "CPI",
  "Enroll", "RuralInc", "UrbanInc"
)
colnames(corr_matrix) <- short_names
rownames(corr_matrix) <- short_names

# Create plot
corrplot::corrplot(
  corr_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 25,          # reduce tilt
  tl.cex = 1.0,         # make labels readable
  addCoef.col = "black",
  number.cex = 0.7,
  cl.cex = 0.9,
  mar = c(1, 1, 3, 1),
  title = "Correlation Matrix of Key Variables",
  diag = FALSE
)

# Identify potential multicollinearity (|r| > 0.8)
high_corr <- which(abs(corr_matrix) > 0.8 & abs(corr_matrix) < 1, arr.ind = TRUE)
if (nrow(high_corr) > 0) {
  cat("\n⚠️ Highly correlated variable pairs (|r| > 0.8):\n")
  apply(high_corr, 1, function(idx) {
    cat(rownames(corr_matrix)[idx[1]], "and", colnames(corr_matrix)[idx[2]],
        ": r =", corr_matrix[idx[1], idx[2]], "\n")
  })
} else {
  cat("\n✅ No strong multicollinearity detected among covariates.\n")
}

plot_covariate <- function(panel, variable, title = NULL) {
  if (is.null(title)) title <- paste("Trend of", variable, "by Province")
  
  ggplot(panel, aes(x = year, y = .data[[variable]], group = province)) +
    geom_line(alpha = 0.4) +
    theme_minimal() +
    labs(
      title = title,
      x = "Year",
      y = variable
    )
}

plot_covariate(ipc_panel, "patent_apps", "Patent Applications Over Time")
plot_covariate(ipc_panel, "r.d_expenditure", "R&D Expenditure Over Time")
plot_covariate(ipc_panel, "r.d_workers", "R&D Workers Over Time")

# -------------------------------
# SECTION 5: OLS + Diagnostics
# Clean sample → simple vs. full models → fitted vs. actual → residual Q-Q.
# -------------------------------

needed <- c(
  "fti_per_capita","treated","gdp_province","population_province",
  "num_priv_enterprise","student_enrollment","disposable_income_urban_per_cap"
)
ipc_clean <- ipc_panel %>% drop_na(all_of(needed))

simple_regression <- lm(fti_per_capita ~ treated, data = ipc_clean)

regression_model <- lm(
  fti_per_capita ~ treated +
    gdp_province + population_province + num_priv_enterprise +
    student_enrollment + disposable_income_urban_per_cap,
  data = ipc_clean
)

print(summary(regression_model))

ipc_clean <- ipc_clean %>% mutate(predicted = predict(regression_model))

ggplot(ipc_clean, aes(x = predicted, y = foreign_total_investment)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Regression Fitted vs. Actual Foreign Investment",
    x = "Predicted Values", y = "Actual Foreign Investment"
  ) +
  theme_minimal()

ggplot(ipc_clean, aes(x = predicted, y = residuals(regression_model))) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Regression Residuals vs. Fitted Values",
    x = "Predicted Values", y = "Residuals"
  ) +
  theme_minimal()

ggplot(data.frame(resid = residuals(simple_regression)), aes(sample = resid)) +
  stat_qq() + stat_qq_line() +
  theme_minimal() +
  labs(title = "Q–Q Plot of Residuals (Simple Regression)")

ggplot(data.frame(resid = residuals(regression_model)), aes(sample = resid)) +
  stat_qq() + stat_qq_line() +
  theme_minimal() +
  labs(title = "Q–Q Plot of Residuals (Full Regression with Covariates)")

# -------------------------------
# SECTION 6: Synthetic Control helper (Updated)
# -------------------------------

run_synth_for <- function(panel, treated_province, treat_year = 2014,
                          years = 2000:2016, upload_to_gsheets = FALSE,
                          gs_title = NULL,
                          drop_donor = NULL) {   
  
  
  # Updated predictor (covariate) list — removed unemployment, CPI, rural income
  predictors <- c(
    "gdp_province", "cpi", "num_priv_enterprise","registered_unemployed",
    "student_enrollment", "disposable_income_urban_per_cap"
  )
  
  # Updated outcome variable to per capita
  outcome_var <- "fti_per_capita"
  
  df <- panel %>%
    filter(year %in% years) %>%
    mutate(province_id = as.numeric(factor(province))) %>%
    select(province_id, year, province, Court_Year, all_of(c(predictors, outcome_var)))
  
  treated_id <- df %>%
    filter(province == treated_province) %>%
    pull(province_id) %>%
    unique()
  
  # NEW: If a donor is specified to drop, remove it from the panel
  if (!is.null(drop_donor)) {
    donor_id_to_remove <- df %>% filter(province == drop_donor) %>% pull(province_id) %>% unique()
    df <- df %>% filter(province_id != donor_id_to_remove)
  }
  
  treated_2014_ids <- df %>%
    filter(Court_Year == treat_year) %>%
    pull(province_id) %>%
    unique()
  
  df <- df %>%
    filter(!province_id %in% setdiff(treated_2014_ids, treated_id)) %>%
    filter(!if_any(all_of(predictors), is.na), !is.na(.data[[outcome_var]]))
  
  counts <- df %>% count(province_id)
  complete_ids <- counts %>% filter(n == length(years)) %>% pull(province_id)
  df <- df %>% filter(province_id %in% complete_ids)
  
  ipc_df <- as.data.frame(df)
  
  # Run dataprep for Synth
  dp <- Synth::dataprep(
    foo = ipc_df,
    predictors = predictors,
    predictors.op = "mean",
    time.predictors.prior = years[years < treat_year],
    dependent = outcome_var,
    unit.variable = "province_id",
    time.variable = "year",
    treatment.identifier = treated_id,
    controls.identifier = setdiff(unique(ipc_df$province_id), treated_id),
    special.predictors = list(list(outcome_var, (treat_year-3):(treat_year-1), "mean")),
    time.optimize.ssr = years[years < treat_year],
    time.plot = years
  )
  
  s.out <- Synth::synth(dp)
  
  # Plot actual vs synthetic
  Synth::path.plot(
    synth.res = s.out, dataprep.res = dp,
    Ylab = "Foreign Total Investment per Capita (USD)",
    Xlab = "Year",
    Main = paste0("Synthetic Control: ", treated_province, " IPC Reform (", treat_year, ")"),
    Legend = c(paste0(treated_province, " (Treated)"), paste0("Synthetic ", treated_province)),
    Legend.position = "bottomright"
  )
  
  abline(v = treat_year, lty = 2, col = "black", lwd = 1.2)
  
  
  # Plot gaps
  Synth::gaps.plot(
    synth.res = s.out, dataprep.res = dp,
    Ylab = paste0("Gap (", treated_province, " - Synthetic)"),
    Xlab = "Year",
    Main = paste0("Treatment Effect on FTI per Capita: ", treated_province, " IPC Reform (", treat_year, ")"),
    abline(v = treat_year, lty = 2)
  )
  
  # RMSPE diagnostics
  Y1 <- dp$Y1plot
  Y1s <- dp$Y0plot %*% s.out$solution.w
  rmspe_df <- tibble(
    year = dp$tag$time.plot,
    actual = as.numeric(Y1),
    synthetic = as.numeric(Y1s)
  ) %>%
    mutate(gap = actual - synthetic,
           pct_error = 100 * abs(gap / actual))
  
  rmspe_pre  <- sqrt(mean(rmspe_df$pct_error[rmspe_df$year < treat_year]^2, na.rm = TRUE))
  rmspe_post <- sqrt(mean(rmspe_df$pct_error[rmspe_df$year >= treat_year]^2, na.rm = TRUE))
  
  cat("📊", treated_province, "RMSPE (Pre):", round(rmspe_pre, 3), "% | (Post):", round(rmspe_post, 3), "%\n")
  
  tables <- Synth::synth.tab(dataprep.res = dp, synth.res = s.out)
  
  if (isTRUE(upload_to_gsheets)) {
    googlesheets4::gs4_auth()
    if (is.null(gs_title)) gs_title <- paste0(treated_province, "_IPC_Synth_Results")
    ss <- gs4_create(gs_title)
    sheet_write(as_tibble(tables$tab.w), ss = ss, sheet = "Donor_Weights")
    sheet_write(as_tibble(tables$tab.pred), ss = ss, sheet = "Predictor_Balance")
    cat("✅ Results uploaded to Google Sheets at:\n", ss, "\n")
  }
  
  invisible(list(
    dataprep = dp,
    synth = s.out,
    tables = tables,
    rmspe = list(pre = rmspe_pre, post = rmspe_post),
    panel_used = ipc_df,
    
    # Store treated name INSIDE returned object
    treated_province = treated_province
  ))
  
}


# -------------------------------
# SECTION 7: Synthetic Control — Runs
# Call helper for Beijing, Shanghai, Guangdong Province.
# Toggle upload_to_gsheets = TRUE if you want Sheets outputs.
# -------------------------------

res_beijing   <- run_synth_for(ipc_panel, "Beijing",            upload_to_gsheets = FALSE)
res_shanghai  <- run_synth_for(ipc_panel, "Shanghai",           upload_to_gsheets = FALSE)
res_guangdong <- run_synth_for(ipc_panel, "Guangdong Province", upload_to_gsheets = FALSE)

# ------------------------------------
# Build donor weights table for Beijing
colnames(res_beijing$tables$tab.w)

donor_weights_beijing <- res_beijing$tables$tab.w %>%
  as_tibble(rownames = "province_id") %>%        # province_id column
  mutate(
    province_id = as.numeric(province_id)        # convert id from character
  ) %>%
  left_join(                                      # merge to get province names
    res_beijing$panel_used %>% 
      distinct(province_id, province),
    by = "province_id"
  ) %>%
  select(province_id, province, w.weights) %>%      # reorder columns
  arrange(desc(w.weights))                          # sort by importance

donor_weights_beijing

ipc_panel %>%
  select(province, year,
         gdp_province, population_province, num_priv_enterprise,
         student_enrollment, disposable_income_urban_per_cap,
         fti_per_capita) %>%
  group_by(province) %>%
  summarise(
    missing = sum(if_any(everything(), is.na)),
    .groups = "drop"
  ) %>%
  arrange(missing)
setdiff(unique(res_beijing$panel_used$province), "Beijing")

# ===============================================================
# 7X — Export Beijing Donor Weights & Predictor Balance to Google Sheets
# ===============================================================

donor_weights_beijing <- res_beijing$tables$tab.w %>%
  as_tibble(rownames = "province_id") %>%
  mutate(province_id = as.numeric(province_id)) %>%
  left_join(
    res_beijing$panel_used %>% distinct(province_id, province),
    by = "province_id"
  ) %>%
  rename(weight = w.weights) %>%
  select(province_id, province, weight) %>%
  arrange(desc(weight))

# ---------- 2. PREDICTOR BALANCE TABLE ----------

tp <- res_beijing$tables$tab.pred
predictors <- rownames(tp)

# Keep only predictors that actually exist in your panel dataset
valid_predictors <- predictors[predictors %in% names(ipc_panel)]

# Compute sample means only for valid predictors
sample_means <- ipc_panel %>%
  filter(year < 2014) %>%
  summarise(across(all_of(valid_predictors), ~ mean(.x, na.rm = TRUE))) %>%
  t() %>% 
  as.numeric()

predictor_balance_beijing <- tibble(
  Predictor   = valid_predictors,
  Treated     = round(tp[valid_predictors, 1], 3),
  Synthetic   = round(tp[valid_predictors, 2], 3),
  Sample_Mean = round(sample_means, 3)
)


# ---------- 3. WRITE TO GOOGLE SHEETS ----------

sheet_title <- "Beijing_SCM_Results"
ss <- gs4_create(sheet_title)

sheet_write(donor_weights_beijing, ss = ss, sheet = "Donor_Weights")
sheet_write(predictor_balance_beijing, ss = ss, sheet = "Predictor_Balance")

cat("✅ Synthetic control tables for Beijing uploaded.\n")
cat("🔗 Google Sheets link:\n")


export_synth_results <- function(res, panel, treat_year = 2014,
                                 gs_title = NULL) {
  
  treated_province <- res$treated_province
  
  if (is.null(gs_title)) {
    gs_title <- paste0(treated_province, "_SCM_Results")
  }
  
  # --------------------------
  # 1. Donor Weights Table
  # --------------------------
  donor_weights <- res$tables$tab.w %>%
    as_tibble(rownames = "province_id") %>%
    mutate(province_id = as.numeric(province_id)) %>%
    left_join(
      res$panel_used %>% distinct(province_id, province),
      by = "province_id"
    ) %>%
    rename(weight = w.weights) %>%
    select(province_id, province, weight) %>%
    arrange(desc(weight))
  
  # --------------------------
  # 2. Predictor Balance Table
  # --------------------------
  tp <- res$tables$tab.pred
  raw_predictors <- rownames(tp)
  
  clean1 <- raw_predictors[!grepl("^special", raw_predictors)]
  clean2 <- gsub("\\.[0-9]{4}(\\.[0-9]{4})?$", "", clean1)
  
  valid_predictors <- clean2[clean2 %in% names(panel)]
  
  sample_means <- panel %>%
    filter(year < treat_year) %>%
    summarise(across(all_of(valid_predictors), ~ mean(.x, na.rm = TRUE))) %>%
    t() %>% as.numeric()
  
  matched_rows <- clean1[match(valid_predictors, clean2)]
  
  predictor_balance <- tibble(
    Predictor   = valid_predictors,
    Treated     = round(tp[matched_rows, 1], 3),
    Synthetic   = round(tp[matched_rows, 2], 3),
    Sample_Mean = round(sample_means, 3)
  )
  
  # --------------------------
  # 3. Compute MSPE & LOSS_V
  # --------------------------
  dp <- res$dataprep
  synth <- res$synth
  
  Y1  <- dp$Y1plot
  Y0w <- dp$Y0plot %*% synth$solution.w
  gap <- Y1 - Y0w
  
  years_vec <- dp$tag$time.plot
  
  mspe_pre  <- mean(gap[years_vec < treat_year]^2, na.rm = TRUE)
  mspe_post <- mean(gap[years_vec >= treat_year]^2, na.rm = TRUE)
  
  loss_v <- sum(synth$loss.v)   # matches your earlier output format
  
  metrics_table <- tibble(
    Metric = c("MSPE_Pre", "MSPE_Post", "LOSS_V"),
    Value  = c(mspe_pre, mspe_post, loss_v)
  )
  
  # --------------------------
  # 4. Google Sheets Export
  # --------------------------
  googlesheets4::gs4_auth()
  
  ss <- googlesheets4::gs4_create(gs_title)
  
  googlesheets4::sheet_write(donor_weights,        ss = ss, sheet = "Donor_Weights")
  googlesheets4::sheet_write(predictor_balance,    ss = ss, sheet = "Predictor_Balance")
  googlesheets4::sheet_write(metrics_table,        ss = ss, sheet = "SCM_Metrics")
  
  # --------------------------
  # 5. PRINT CONFIRMATION
  # --------------------------
  sheet_url <- tryCatch(ss$browser_url,
                        error = function(e) tryCatch(ss$spreadsheet_url, error=function(e) NA))
  
  cat("\n======================================\n")
  cat("     📤 SCM Export Complete\n")
  cat("     Province:", treated_province, "\n")
  cat("     Google Sheet:", gs_title, "\n")
  cat("     🔗 Link:", sheet_url, "\n")
  cat("======================================\n\n")
  
  cat("📘 Donor Weights:\n"); print(donor_weights)
  cat("\n📘 Predictor Balance:\n"); print(predictor_balance)
  cat("\n📘 SCM Metrics:\n"); print(metrics_table)
  
  return(list(
    donor_weights = donor_weights,
    predictor_balance = predictor_balance,
    metrics = metrics_table,
    google_sheet_url = sheet_url
  ))
}

export_synth_results(res_beijing, panel = ipc_panel)

export_synth_results(res_shanghai, panel = ipc_panel)
export_synth_results(res_guangdong, panel = ipc_panel)


# ===================================================================
# 7A — Helper: Extract actual, synthetic, and gap series
# ===================================================================
extract_synth_series <- function(res) {
  dp <- res$dataprep
  s  <- res$synth
  
  tibble(
    year      = dp$tag$time.plot,
    actual    = as.numeric(dp$Y1plot),
    synthetic = as.numeric(dp$Y0plot %*% s$solution.w),
    gap       = as.numeric(dp$Y1plot - (dp$Y0plot %*% s$solution.w)),
    province  = res$treated_province
  )
}


# ===================================================================
# 7A — Raw outcome trend plot (treated vs average donor)
# ===================================================================
plot_raw_outcome <- function(panel, treated_province) {
  avg_control <- panel %>%
    filter(province != treated_province) %>%
    group_by(year) %>%
    summarise(avg_fti = mean(fti_per_capita, na.rm = TRUE))
  
  treated <- panel %>% filter(province == treated_province)
  
  ggplot() +
    geom_line(data = avg_control, aes(x = year, y = avg_fti),
              color = "gray60", size = 1.2) +
    geom_line(data = treated, aes(x = year, y = fti_per_capita),
              color = "black", size = 1.4) +
    geom_vline(xintercept = 2014, linetype = "dotted") +
    labs(
      title = paste("Raw Outcome Comparison:", treated_province),
      subtitle = "Foreign Total Investment per Capita",
      x = "Year", y = "FTI per Capita"
    ) +
    theme_minimal()
}

plot_covariate_raw <- function(panel, treated_province, variable, treat_year = 2014) {
  
  # quick safety check
  if (!variable %in% names(panel)) {
    stop(paste("Variable", variable, "not found in ipc_panel"))
  }
  
  avg_control <- panel %>%
    filter(province != treated_province) %>%
    group_by(year) %>%
    summarise(avg_value = mean(.data[[variable]], na.rm = TRUE), .groups = "drop")
  
  treated <- panel %>%
    filter(province == treated_province) %>%
    select(year, value = all_of(variable))
  
  ggplot() +
    geom_line(data = avg_control, aes(x = year, y = avg_value),
              color = "gray60", size = 1.2) +
    geom_line(data = treated, aes(x = year, y = value),
              color = "black", size = 1.4) +
    geom_vline(xintercept = treat_year, linetype = "dotted") +
    theme_minimal() +
    labs(
      title = paste("Beijing vs Untreated Average —", variable),
      subtitle = "Black = Beijing, Gray = Average of untreated provinces",
      x = "Year", y = variable
    )
}
library(zoo)   # for na.approx()

plot_covariate_imputed_pc <- function(panel, treated_province, variable) {
  
  # ---- 1. Compute per-capita variable ON THE FLY ----
  panel2 <- panel %>%
    mutate(
      value_pc = .data[[variable]] / population_province
    )
  
  # ---- 2. Extract treated series ----
  treated_df <- panel2 %>%
    filter(province == treated_province) %>%
    select(year, value_pc)
  
  # Find first & last observed non-missing points
  first_obs <- min(treated_df$year[!is.na(treated_df$value_pc)])
  last_obs  <- max(treated_df$year[!is.na(treated_df$value_pc)])
  
  # Keep only the range where ANY data exist
  treated_trim <- treated_df %>% filter(year >= first_obs, year <= last_obs)
  
  # ---- 3. Impute missing internal gaps (linear) ----
  treated_trim <- treated_trim %>%
    mutate(
      value_interp = zoo::na.approx(value_pc, year, na.rm = FALSE)
    )
  
  # Identify which values were imputed (not originally observed)
  treated_trim <- treated_trim %>%
    mutate(
      imputed = ifelse(is.na(value_pc) & !is.na(value_interp), TRUE, FALSE),
      value_final = ifelse(is.na(value_pc), value_interp, value_pc)
    )
  
  # ---- 4. Compute average untreated donor series ----
  donor_avg <- panel2 %>%
    filter(province != treated_province) %>%
    group_by(year) %>%
    summarise(avg_pc = mean(value_pc, na.rm = TRUE), .groups = "drop") %>%
    filter(year >= first_obs, year <= last_obs)
  
  # ---- 5. Plot ----
  ggplot() +
    # Donor average
    geom_line(data = donor_avg,
              aes(x = year, y = avg_pc),
              color = "gray50", size = 1.2) +
    
    # Observed Beijing values
    geom_line(data = treated_trim %>% filter(!imputed),
              aes(x = year, y = value_final),
              color = "black", size = 1.3) +
    
    # Imputed (dotted) Beijing values
    geom_line(data = treated_trim %>% filter(imputed),
              aes(x = year, y = value_final),
              color = "black", size = 1.3, linetype = "dashed") +
    
    labs(
      title = paste("Beijing vs Donor Average (Per Capita)", variable),
      x = "Year",
      y = paste(variable, "per capita")
    ) +
    theme_minimal()
}


plot_covariate_imputed_pc(ipc_panel, "Beijing", "patent_apps")
plot_covariate_imputed_pc(ipc_panel, "Beijing", "r.d_expenditure")
plot_covariate_imputed_pc(ipc_panel, "Beijing", "r.d_workers")




# ===================================================================
# 7A — Predictor Balance (Bar Plot)
# ===================================================================
plot_predictor_balance <- function(res, province_name) {
  tp <- res$tables$tab.pred
  
  predictor_data <- tibble(
    predictor = rownames(tp),
    Treated   = tp[,1],
    Synthetic = tp[,2]
  ) %>%
    pivot_longer(cols = c(Treated, Synthetic),
                 names_to = "type",
                 values_to = "value")
  
  ggplot(predictor_data, aes(x = predictor, y = value, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    labs(
      title = paste("Predictor Balance:", province_name),
      x = "Predictor", y = "Value"
    )
}

make_predictor_balance_table <- function(res, province_name, panel, treat_year = 2014) {
  
  # Extract predictor info
  tp <- res$tables$tab.pred
  all_predictors <- rownames(tp)
  
  # Keep only predictors that actually exist in the panel data
  valid_predictors <- all_predictors[all_predictors %in% names(panel)]
  
  # Compute sample means for valid predictors only
  sample_means <- panel %>%
    filter(year < treat_year) %>%
    summarise(across(all_of(valid_predictors), ~ mean(.x, na.rm = TRUE))) %>%
    t() %>%
    as.numeric()
  
  # Build predictor balance table
  balance_table <- tibble::tibble(
    Predictor    = valid_predictors,
    Treated      = round(tp[valid_predictors, 1], 3),
    Synthetic    = round(tp[valid_predictors, 2], 3),
    Sample_Mean  = round(sample_means, 3)
  )
  
  # Show results
  cat("\n============================\n")
  cat("Predictor Balance Table for:", province_name, "\n")
  cat("============================\n")
  print(balance_table, n = nrow(balance_table))
  
  return(balance_table)
}


bal_beijing <- make_predictor_balance_table(res_beijing, "Beijing", ipc_panel)
bal_shanghai <- make_predictor_balance_table(res_shanghai, "Shanghai", ipc_panel)
bal_guangdong <- make_predictor_balance_table(res_guangdong, "Guangdong Province", ipc_panel)


# ================================================================
# 7B — Unified Placebo-in-Space Spaghetti Plot (ALL provinces)
# ================================================================
placebo_space_all <- function(panel, treated_provinces,
                              treat_year = 2014, years = 2000:2016) {
  
  provinces <- unique(panel$province)
  gap_list <- list()
  
  # Run synth for every province (treated + untreated)
  for (prov in provinces) {
    message("Running placebo synth for ", prov)
    try({
      res_temp <- run_synth_for(panel, prov, treat_year = treat_year)
      df_temp  <- extract_synth_series(res_temp)
      df_temp$unit <- prov
      gap_list[[prov]] <- df_temp
    }, silent = TRUE)
  }
  
  # Combine all results
  all_gaps <- bind_rows(gap_list)
  
  # Colors for treated units
  color_map <- c(
    "Beijing" = "red"
  )
  
  ggplot(all_gaps, aes(x = year, y = gap, group = unit)) +
    geom_line(color = "gray80", alpha = 0.5, size = 0.8) +
    
    # Highlight treated provinces
    geom_line(
      data = all_gaps %>% filter(unit %in% treated_provinces),
      aes(color = unit),
      size = 1.4
    ) +
    
    scale_color_manual(values = color_map) +
    
    geom_vline(xintercept = treat_year, linetype = "dotted") +
    
    theme_minimal() +
    labs(
      title = "Placebo-in-Space Test: Unified Spaghetti Plot",
      subtitle = "Gray = placebo provinces; Colors = true treated provinces",
      x = "Year",
      y = "Treatment Effect (Gap: Actual − Synthetic)",
      color = "Treated Province"
    )
}

#---------------------------------------------------- Histogram
compute_mspe_ratio <- function(panel, treat_year = 2014, years = 2000:2016) {
  
  provinces <- unique(panel$province)
  results <- list()
  
  for (prov in provinces) {
    message("Running placebo for: ", prov)
    
    # Try synthetic control; skip failures
    out <- try(run_synth_for(panel, prov, treat_year = treat_year), silent = TRUE)
    if (inherits(out, "try-error")) next
    
    dp <- out$dataprep
    synth <- out$synth
    
    Y1  <- dp$Y1plot
    Y0w <- dp$Y0plot %*% synth$solution.w
    gap <- Y1 - Y0w
    
    years_vec <- dp$tag$time.plot
    
    mspe_pre <- mean(gap[years_vec < treat_year]^2, na.rm = TRUE)
    mspe_post <- mean(gap[years_vec >= treat_year]^2, na.rm = TRUE)
    
    results[[prov]] <- tibble(
      province = prov,
      mspe_pre = mspe_pre,
      mspe_post = mspe_post,
      ratio = mspe_post / mspe_pre
    )
  }
  
  bind_rows(results)
}

mspe_ratios <- compute_mspe_ratio(ipc_panel, treat_year = 2014)

treated_units <- c("Beijing")

mspe_ratios <- mspe_ratios %>%
  mutate(
    treated = province %in% treated_units
  )

library(ggplot2)

ggplot(mspe_ratios, aes(x = ratio)) +
  geom_histogram(
    bins = 30,
    fill = "gray75",
    color = "black",
    alpha = 0.8
  ) +
  geom_vline(
    data = mspe_ratios %>% filter(treated),
    aes(xintercept = ratio, color = province),
    size = 1.2,
    linetype = "dotted"
  ) +
  scale_color_manual(values = c(
    "Beijing" = "red"
  )) +
  theme_minimal() +
  labs(
    title = "Histogram of Post/Pre MSPE Ratios",
    subtitle = "Placebo Units vs. True Treated Units",
    x = "MSPE Ratio (Post / Pre)",
    y = "Count",
    color = "Treated Provinces"
  )

ggplot(mspe_ratios %>% filter(province != "Anhui Province"), aes(x = ratio)) +
  geom_density(fill = "gray80", alpha = 0.6) +
  geom_vline(
    data = mspe_ratios %>% filter(treated),
    aes(xintercept = ratio, color = province),
    size = 1.2
  ) +
  scale_color_manual(values = c(
    "Beijing" = "red"
  )) +
  theme_minimal() +
  labs(
    title = "Density of MSPE Ratios (Post/Pre)",
    subtitle = "Vertical lines show treated provinces' MSPE ratios",
    x = "MSPE Ratio",
    y = "Density"
  )

# ===================================================================
# outlier
# ===================================================================
treated_max <- mspe_ratios %>% 
  filter(treated) %>% 
  summarise(max_ratio = max(ratio)) %>% 
  pull(max_ratio)

mspe_ratios %>%
  filter(ratio > treated_max) %>%
  arrange(desc(ratio))

Q1 <- quantile(mspe_ratios$ratio, 0.25, na.rm = TRUE)
Q3 <- quantile(mspe_ratios$ratio, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

upper_cutoff <- Q3 + 1.5 * IQR_val

mspe_ratios %>%
  filter(ratio > upper_cutoff)

# ===================================================================
# 7B — IN-TIME PLACEBO (Fake Treatment Year)
# ===================================================================
plot_placebo_time_single <- function(panel, treated_province, fake_year = 2008) {
  
  # Run synthetic control with fake treatment year
  res_fake <- run_synth_for(
    panel,
    treated_province = treated_province,
    treat_year = fake_year,
    upload_to_gsheets = FALSE
  )
  
  df_fake <- extract_synth_series(res_fake)

  # Build the plot
  ggplot(df_fake, aes(x = year)) +
    geom_line(aes(y = actual), color = "blue", size = 1.3) +
    geom_line(aes(y = synthetic), color = "red", size = 1.2) +
    geom_vline(xintercept = fake_year, linetype = "dotted") +
    theme_minimal() +
    labs(
      title = paste("In-Time Placebo Test for", treated_province),
      subtitle = paste("Fake treatment year =", fake_year),
      x = "Year",
      y = "FTI per Capita"
    )
}


# ================================================================
# 7B — Unified Placebo-in-Space Spaghetti Plot (ALL provinces)
# ================================================================
placebo_space_all <- function(panel, treated_provinces,
                              treat_year = 2014, years = 2000:2016) {
  
  # Clean data
  panel_clean <- panel %>%
    filter(
      !is.na(province),
      !grepl("Data Source", province, ignore.case = TRUE),
      !grepl("Note:", province, ignore.case = TRUE),
      !grepl("£º|：", province),
      province != "",
      nchar(province) > 2
    ) %>%
    mutate(province_id = as.numeric(factor(province)))
  
  # Updated predictors
  predictors <- c(
    "gdp_province", "population_province", "num_priv_enterprise",
    "student_enrollment", "disposable_income_urban_per_cap"
  )
  
  # Get all provinces with complete data
  complete_provinces <- panel_clean %>%
    filter(year %in% years) %>%
    group_by(province_id, province) %>%
    summarise(
      n_years = n(),
      complete_preds = sum(!if_any(all_of(c(predictors, "fti_per_capita")), is.na)),
      .groups = "drop"
    ) %>%
    filter(n_years == length(years), complete_preds == length(years)) %>%
    pull(province_id)
  
  # Filter to complete provinces only
  panel_synth <- panel_clean %>%
    filter(province_id %in% complete_provinces, year %in% years) %>%
    select(province_id, province, year, all_of(c(predictors, "fti_per_capita"))) %>%
    as.data.frame()
  
  # Get province IDs for treated units
  treated_ids <- panel_synth %>%
    filter(province %in% treated_provinces) %>%
    pull(province_id) %>%
    unique()
  
  # Run synth for EVERY province (treated + donors)
  all_gaps <- lapply(
    X = complete_provinces,
    FUN = function(tr_id) {
      
      message("Running placebo synth for province ID: ", tr_id)
      
      # Controls = all other complete provinces
      ctrl_ids <- setdiff(complete_provinces, tr_id)
      
      # Run dataprep
      dp <- try(
        dataprep(
          foo                   = panel_synth,
          predictors            = predictors,
          predictors.op         = "mean",
          special.predictors    = list(
            list("fti_per_capita", (treat_year - 3):(treat_year - 1), "mean")
          ),
          dependent             = "fti_per_capita",
          unit.variable         = "province_id",
          time.variable         = "year",
          treatment.identifier  = tr_id,
          controls.identifier   = ctrl_ids,
          time.predictors.prior = years[years < treat_year],
          time.optimize.ssr     = years[years < treat_year],
          time.plot             = years
        ),
        silent = TRUE
      )
      
      if (inherits(dp, "try-error")) return(NULL)
      
      # Run synth
      sr <- try(synth(dp), silent = TRUE)
      if (inherits(sr, "try-error")) return(NULL)
      
      # Calculate gap
      gap <- dp$Y1plot - (dp$Y0plot %*% sr$solution.w)
      
      # Pre-treatment MSPE for filtering bad placebos
      pre_idx  <- dp$tag$time.plot < treat_year
      mspe_pre <- mean(gap[pre_idx]^2)
      
      # Get province name
      prov_name <- panel_synth %>%
        filter(province_id == tr_id) %>%
        pull(province) %>%
        unique() %>%
        first()
      
      data.frame(
        year     = dp$tag$time.plot,
        gap      = as.numeric(gap),
        unit_id  = tr_id,
        province = prov_name,
        label    = ifelse(tr_id %in% treated_ids, "Treated", "Donor"),
        mspe_pre = mspe_pre,
        stringsAsFactors = FALSE
      )
    }
  )
  
  # Combine all results
  all_gaps_df <- bind_rows(all_gaps)
  
  # Optional: filter out placebos with very poor pre-treatment fit
  # (MSPE > 2x the treated unit's MSPE)
  treated_mspe <- all_gaps_df %>%
    filter(label == "Treated") %>%
    pull(mspe_pre) %>%
    mean()
  
  all_gaps_df <- all_gaps_df %>%
    filter(mspe_pre <= 2 * treated_mspe | label == "Treated")
  
  # Colors for treated units
  color_map <- c(
    "Beijing" = "red",
    "Shanghai" = "blue",
    "Guangdong Province" = "darkgreen"
  )
  
  # Plot
  ggplot(all_gaps_df, aes(x = year, y = gap, group = unit_id)) +
    # Gray lines for donors
    geom_line(
      data = all_gaps_df %>% filter(label == "Donor"),
      color = "gray80", alpha = 0.5, size = 0.8
    ) +
    # Colored lines for treated provinces
    geom_line(
      data = all_gaps_df %>% filter(label == "Treated"),
      aes(color = province),
      size = 1.4
    ) +
    scale_color_manual(values = color_map) +
    geom_vline(xintercept = treat_year, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    theme_minimal() +
    labs(
      title = "Placebo-in-Space Test: Spaghetti Plot",
      subtitle = paste("Gray = donor provinces; Colors = treated provinces",
                       "\n(Filtered: placebos with pre-MSPE > 2× treated average)"),
      x = "Year",
      y = "Treatment Effect (Gap: Actual − Synthetic)",
      color = "Treated Province"
    )
}



# ===================================================================
# 7B — Generate all plots for Beijing, Shanghai, Guangdong
# ===================================================================
treated_list  <- c("Beijing", "Shanghai", "Guangdong Province")
results_list  <- list(res_beijing, res_shanghai, res_guangdong)
names(results_list) <- treated_list

placebo_space_single <- function(panel, treated_province, treat_year = 2014) {
  placebo_space_all(
    panel,
    treated_provinces = treated_province,
    treat_year = treat_year
  )
}

placebo_space_single(ipc_panel, "Beijing")
placebo_space_single(ipc_panel, "Shanghai")
placebo_space_single(ipc_panel, "Guangdong Province")

plot_gap_for <- function(res, treat_year = 2014) {
  df <- extract_synth_series(res)
  
  ggplot(df, aes(x = year, y = gap)) +
    geom_line(size = 1.2, color = "black") +
    geom_vline(xintercept = treat_year, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme_minimal() +
    labs(
      title = paste("Treatment Effect (Gap) for", unique(df$province)),
      subtitle = "Pre- and Post-Treatment Synthetic Control Gap",
      x = "Year",
      y = "Actual - Synthetic"
    )
}
plot_gap_for(res_beijing)
plot_gap_for(res_shanghai)
plot_gap_for(res_guangdong)

plot_placebo_time_single(ipc_panel, "Beijing", fake_year = 2008)
plot_placebo_time_single(ipc_panel, "Shanghai", fake_year = 2008)
plot_placebo_time_single(ipc_panel, "Guangdong Province", fake_year = 2008)

cat("\n\n=== Diagnostic Plots for Shanghai ===\n")

print(plot_raw_outcome(ipc_panel, "Shanghai"))
print(plot_predictor_balance(res_shanghai, "Shanghai"))
print(plot_gap_for(res_shanghai))

cat("\n\n=== Diagnostic Plots for Guangdong Province ===\n")

print(plot_raw_outcome(ipc_panel, "Guangdong Province"))
print(plot_predictor_balance(res_guangdong, "Guangdong Province"))
print(plot_gap_for(res_guangdong))

# ===========================================================
# SECTION 8: Sensitivity Analysis — Remove Tianjin from Donor Pool (Beijing SCM)
# ===========================================================
res_beijing_sensitivity <- run_synth_for(
  panel = ipc_panel,
  treated_province = "Beijing",
  drop_donor = "Tianjin",      # ← only change
  upload_to_gsheets = FALSE
)
export_synth_results(res_beijing_sensitivity, panel = ipc_panel)

  

# ============================================
# SECTION 9 — Synthetic Difference-in-Differences (SDID) for Shanghai
# ============================================
install.packages("remotes")
remotes::install_github("synth-inference/synthdid")
library(synthdid)


cat("\n==============================\n")
cat("Running Synthetic DiD for Shanghai\n")
cat("==============================\n")

# --------------------------------------------
# 1. Filter data: Only 2000–2016 window

sdid_df <- ipc_panel %>%
  filter(year >= 2000, year <= 2016) %>%
  filter(
    !(province %in% c("Beijing", "Guangdong Province"))
  ) %>%
  mutate(
    D = ifelse(province == "Shanghai" & year >= 2014, 1, 0)
  )


# --------------------------------------------
# 2. Create wide matrices required by SDID

Y_mat <- sdid_df %>%
  select(province, year, fti_per_capita) %>%
  pivot_wider(names_from = year, values_from = fti_per_capita) %>%
  column_to_rownames("province") %>%
  as.matrix()

D_mat <- sdid_df %>%
  select(province, year, D) %>%
  pivot_wider(names_from = year, values_from = D) %>%
  column_to_rownames("province") %>%
  as.matrix()

# --------------------------------------------
# 3. Run Synthetic DiD estimator
# --------------------------------------------
# --------------------------------------------
# Identify T0 automatically

treat_year <- 2014
years <- sort(unique(sdid_df$year))
T0 <- length(2000:2013)   # = 14
res_sdid <- synthdid::synthdid_estimate(Y_mat, D_mat, T0 = T0)


# Number of control units
N0 <- sum(rownames(Y_mat) != "Shanghai")

# --------------------------------------------
# Run Synthetic DiD estimator
# --------------------------------------------
res_sdid <- synthdid::synthdid_estimate(
  Y = Y_mat,
  N0 = N0,
  T0 = 14
)


cat("\n=== SDID Estimate for Shanghai ===\n")
# ATT estimate
att_shanghai <- as.numeric(res_sdid)
cat("ATT:", att_shanghai, "\n")

# Standard error
att_se <- synthdid::synthdid_se(res_sdid)
cat("Std. Error:", att_se, "\n")

# 95% CI
ci <- att_shanghai + c(-1, 1) * 1.96 * att_se
cat("95% CI:", ci, "\n")

# Plot
plot(res_sdid)
title("Synthetic DiD Estimate: Shanghai IPC Reform (2014)")

plot(res_sdid, type = "counterfactual")
title("SDID Counterfactual Path: Shanghai")

synthdid:::plot.synthdid_estimate(res_sdid)
title("SDID Estimate: Shanghai IPC Reform (2014)")
synthdid:::plot.synthdid_estimate(res_sdid, overlay = TRUE)
title("Actual vs Counterfactual Path (SDID): Shanghai")
synthdid:::plot.synthdid_estimate(res_sdid, weights = TRUE)
title("SDID Weights: Shanghai")



plot(res_sdid, main = "Synthetic DiD Estimate: Shanghai IPC Reform (2014)")



att_shanghai <- as.numeric(res_sdid)
att_se <- synthdid::synthdid_se(res_sdid)

att_shanghai
att_se


cat("ATT estimate:", att_shanghai, "\n")
cat("Std. Error:", att_se, "\n")

ci <- att_shanghai + c(-1,1)*1.96*att_se
cat("95% CI:", ci, "\n")

plot(res_sdid, main="Synthetic DiD: Shanghai")

# --------------------------------------------
# 4. Plot SDID result (Built-in)
# --------------------------------------------
plot(res_sdid, main = "Synthetic DiD Estimate for Shanghai")

# --------------------------------------------
# 5. Extract weights for table reporting
# --------------------------------------------
cat("\n=== Unit Weights (Synthetic Controls) ===\n")
print(res_sdid$weights$omega)

cat("\n=== Time Weights (Elastic DiD Weights) ===\n")
print(res_sdid$weights$lambda)

# --------------------------------------------
# 6. Optional: Store results for paper
# --------------------------------------------
sdid_results <- list(
  att = res_sdid$att,
  unit_weights = res_sdid$weights$omega,
  time_weights = res_sdid$weights$lambda
)


# -------------------------------
# SECTION 8: Staggered DID — quick cohort peek
# (You can extend this with 'did' or 'fixest' estimators.)
# -------------------------------

ipc_panel %>%
  filter(Court_Year == 2020) %>%
  distinct(province) %>%
  count() %>%
  print()
