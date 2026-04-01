# ==============================================================================
# preprocessing_transparency_only.R
#
# !! DO NOT RUN THIS SCRIPT !!
#
# This file is included for TRANSPARENCY ONLY.
#
# It documents the preprocessing pipeline applied to the restricted city-level
# dataset (IPC_CITY.dta) originally compiled by Wan et al. That raw dataset
# is NOT distributed with this replication package due to data sharing
# restrictions imposed by the original data providers.
#
# The OUTPUT of this script — a province-year panel (ipc_with_covariates.csv)
# — IS included in /data and is the starting point for all replication scripts.
#
# If you have obtained IPC_CITY.dta from the original authors and wish to
# replicate this step, place the file at the path specified below and run
# this script after installing the required packages (see 01_setup.R).
#
# Contact: [YOUR NAME / EMAIL] for questions about data access.
# ==============================================================================

# ---- Input path (restricted data — NOT included in this package) -------------
ipc_dta_path <- "~/Downloads/IPC_CITY.dta"  # adjust to your local path

# ---- Packages ----------------------------------------------------------------
library(haven)
library(tidyverse)
library(stringi)
library(googlesheets4)
library(readr)
library(tidyr)
library(stringr)

# ==============================================================================
# STEP 1: Read Wan et al. city-level Stata file & export raw CSV
# ==============================================================================

data <- read_dta(ipc_dta_path)
write.csv(data, "data/IPC_CITY_converted.csv", row.names = FALSE)
cat("CSV saved.\n")

# ==============================================================================
# STEP 2: Translate province names to English, create Pinyin city labels
# ==============================================================================

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
  mutate(province_english = province_translate[province_name])

# Check for missing translations
missing_prov <- unique(data$province_name[is.na(data$province_english)])
if (length(missing_prov) > 0) {
  cat("Missing province translations:\n"); print(missing_prov)
} else {
  cat("All provinces translated.\n")
}

# Pinyin city names
drop_suffix  <- function(x) gsub("(市|州|盟|地区|自治州)$", "", x)
to_pinyin_name <- function(x) {
  x2 <- drop_suffix(x)
  p  <- stringi::stri_trans_general(x2, "Han-Latin/Names; Latin-ASCII")
  p  <- gsub("\\s*'\\s*", "'", p)
  tools::toTitleCase(trimws(p))
}

data <- data %>%
  mutate(
    city_english = to_pinyin_name(city_name),
    Court_Year   = suppressWarnings(as.integer(Court_Year)),
    control_flag = ifelse(is.na(Court_Year), 1L, 0L)
  )

# Minor name fixes
fixes <- c("Xi'An" = "Xi'an", "Urumqi" = "Urumqi", "Ordos" = "Ordos")
data$city_english <- ifelse(data$city_english %in% names(fixes),
                            fixes[data$city_english], data$city_english)

write.csv(data, "data/IPC_CITY_translated_provinces_final.csv", row.names = FALSE)
cat("Translated CSV saved.\n")

# ==============================================================================
# STEP 3: Pull province-level covariates from Google Sheets
#
# NOTE: This requires Google OAuth authentication. The processed output
# (combined_covariates.csv) is included in /data so replicators do not
# need to re-run this step.
#
# The Google Sheet URL below is the authors' private sheet. You will need
# to supply your own credentials and sheet URL if replicating from scratch.
# ==============================================================================

# googlesheets4::gs4_auth(
#   scopes = "https://www.googleapis.com/auth/spreadsheets"
# )
#
# sheet_url <- "https://docs.google.com/spreadsheets/d/1pXywZSiJvs4PrE6Vwrle2zO--lYMQLn7Yg7FtmtiK4c/edit?usp=sharing"
# all_sheets <- googlesheets4::sheet_names(sheet_url)
#
# combined_covariates <- NULL
# for (sheet in all_sheets) {
#   cat("Processing sheet:", sheet, "\n")
#   df_temp    <- googlesheets4::read_sheet(sheet_url, sheet = sheet, skip = 3)
#   var_name   <- make.names(sheet)
#   province_col <- if ("Region" %in% names(df_temp)) "Region" else "area"
#
#   df_long <- df_temp %>%
#     tidyr::pivot_longer(cols = -all_of(province_col),
#                         names_to = "year", values_to = var_name) %>%
#     dplyr::mutate(year = as.numeric(year),
#                   province = .data[[province_col]]) %>%
#     dplyr::select(province, year, all_of(var_name))
#
#   combined_covariates <- if (is.null(combined_covariates)) df_long else
#     dplyr::full_join(combined_covariates, df_long, by = c("province","year"))
# }
# readr::write_csv(combined_covariates, "data/combined_covariates.csv")

# ==============================================================================
# STEP 4: Merge & build province-year panel
# ==============================================================================

ipc_city   <- readr::read_csv("data/IPC_CITY_translated_provinces_final.csv") %>%
  mutate(
    province   = coalesce(province_english, province_name),
    Court_Year = suppressWarnings(as.integer(Court_Year))
  )

covariates <- readr::read_csv("data/combined_covariates.csv")

norm_name <- function(x) x %>% str_trim() %>% str_replace_all("\\s+", " ")
covariates  <- covariates  %>% mutate(province = norm_name(province))

# Province-level rollout timing (earliest city implementation date)
ipc_rollout_prov <- ipc_city %>%
  filter(!is.na(Court_Year)) %>%
  group_by(province) %>%
  summarise(Court_Year = min(Court_Year), .groups = "drop") %>%
  mutate(province = norm_name(province))

# Build panel: covariates x rollout
ipc_panel <- covariates %>%
  left_join(ipc_rollout_prov, by = "province") %>%
  mutate(
    treated      = ifelse(!is.na(Court_Year) & year >= Court_Year, 1L, 0L),
    ever_treated = ifelse(!is.na(Court_Year), 1L, 0L),
    rel_time     = ifelse(!is.na(Court_Year), year - Court_Year, NA_integer_),
    fti_pc                  = foreign_total_investment / population_province,
    num_priv_enterprise_pc  = num_priv_enterprise  / population_province,
    patent_apps_pc          = patent_apps           / population_province,
    r.d_expenditure_pc      = r.d_expenditure       / population_province,
    r.d_workers_pc          = r.d_workers           / population_province,
    student_enrollment_pc   = student_enrollment    / population_province,
    export_intensity = ifelse(!is.na(export_industry) & export_industry > 0,
                              sales_value_industry / export_industry, NA_real_),
    tech_value_pc    = tech_market_value / population_province,
    gdp_log          = ifelse(!is.na(gdp_province) & gdp_province > 0,
                              log(gdp_province), NA_real_)
  ) %>%
  filter(
    !is.na(province),
    !grepl("Data Source", province, ignore.case = TRUE),
    !grepl("Note:", province, ignore.case = TRUE),
    !grepl("£º|：", province),
    province != "",
    nchar(province) > 2
  )

# Save — THIS FILE is what replicators use as their starting point
readr::write_csv(ipc_panel, "data/ipc_with_covariates.csv")
saveRDS(ipc_panel, "data/ipc_with_covariates.rds")
cat("Province-year panel saved to data/ipc_with_covariates.csv\n")
