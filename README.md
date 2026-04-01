# Replication Package: The Effect of Intellectual Property Courts on Foreign Investment in China

> **[YOUR PAPER TITLE]**  
> [YOUR NAME(S)] · [INSTITUTION] · [YEAR]  
> Contact: [YOUR EMAIL]

---

## Overview

This repository contains the replication package for the synthetic control analysis of China's Intellectual Property Courts (IPCs) and their effect on foreign total investment (FTI) at the province level.

The analysis covers 31 Chinese provinces over 2000–2016 and focuses on two treated provinces — **Beijing** and **Guangdong** — which received IPC establishment in 2014. The main method is the **Synthetic Control Method** (Abadie, Diamond & Hainmueller 2010), implemented via the `Synth` R package.

---

## Data Availability Statement

### Restricted data (NOT included)

The raw city-level dataset (`IPC_CITY.dta`) was originally compiled by Wan et al. and is subject to data-sharing restrictions imposed by the original authors. **This file is not distributed with this package.** Researchers wishing to access the raw data should contact Wan et al. directly.

### Data included in this package

The **province-year panel** (`data/ipc_with_covariates.csv`) — which is the direct input to all analysis scripts — is included. It was constructed from:

1. The Wan et al. city-level dataset (aggregated to province level)
2. Province-level covariates pulled from [describe your Google Sheet source — e.g., NBS / CEIC / manually compiled]

The preprocessing pipeline that produced this file is documented for transparency in `code/00_data_preprocessing_NOT_RUN.R`. **That script cannot be run without the restricted raw data**, but it is included so that the full pipeline is auditable.

---

## Repository Structure

```
ipc_replication/
│
├── README.md                              ← This file
├── run_all.R                              ← Master script (run this to replicate everything)
│
├── code/
│   ├── preprocessing_transparency_only.R ← Transparency only — DO NOT RUN
│   ├── 01_setup.R                        ← Package installation + global plot theme
│   ├── 02_build_panel.R                  ← REPLICATORS START HERE — loads pre-built CSVs
│   ├── 03_panel_exploration.R            ← Diagnostics, covariate trends, corrplot
│   ├── 04_synth_helpers.R                ← All SCM helper functions (sourced, not run directly)
│   ├── 05_synth_main.R                   ← Main SCM: Beijing + Guangdong
│   └── 06_robustness.R                   ← All placebo and robustness checks
│
├── data/
│   ├── ipc_with_covariates.csv           ← ✅ INCLUDED — Province-year panel (replicators use this)
│   └── combined_covariates.csv           ← ✅ INCLUDED — Covariate panel
│   [IPC_CITY.dta]                        ← ❌ NOT INCLUDED — Restricted (Wan et al.)
│
├── output/
│   ├── figures/                          ← All plots saved here (auto-created on run)
│   └── tables/                           ← All tables saved here (auto-created on run)
│
└── .gitignore                            ← Blocks restricted data files from being committed
```

---

## How to Replicate

### Requirements

- **R** ≥ 4.1 (tested on [YOUR R VERSION])
- **RStudio** (recommended) or base R
- Internet access is **not required** — all data is bundled in `data/`

### Step-by-step

1. Clone or download this repository.
2. Drop your two data files into `data/`:
   - `data/ipc_with_covariates.csv`
   - `data/combined_covariates.csv`
3. Open R and set your working directory to the repository root:
   ```r
   setwd("path/to/ipc_replication")
   ```
4. Run the master script:
   ```r
   source("run_all.R")
   ```

`run_all.R` will verify the data files are present, install missing packages, and run all scripts in order.

### Running scripts individually

The entry point for replicators is `02_build_panel.R`, which loads the two bundled CSVs and creates the `ipc_panel` object used by all downstream scripts. The dependency chain is:

```
01_setup.R
    ↓
02_build_panel.R          ← start here if running manually
    ↓
03_panel_exploration.R    (optional — diagnostics only)
    ↓
04_synth_helpers.R        (sourced automatically, not run directly)
    ↓
05_synth_main.R
    ↓
06_robustness.R
```

---

## Key Variables

| Variable | Description | Unit |
|----------|-------------|------|
| `fti_pc` | Foreign total investment per capita | USD per person |
| `foreign_total_investment` | FTI (raw) | 10,000 USD (万美元) |
| `population_province` | Population | 10,000 persons (万人) |
| `gdp_log` | Log GDP | log(GDP in original units) |
| `cpi` | Consumer price index | Index |
| `num_priv_enterprise_pc` | Private enterprises per capita | Enterprises / 10,000 persons |
| `registered_unemployed` | Registered urban unemployed | Persons |
| `student_enrollment_pc` | Tertiary enrollment per capita | Students / 10,000 persons |
| `disposable_income_urban_per_cap` | Urban disposable income | CNY per person |
| `export_intensity` | Sales / export ratio (manufacturing) | Ratio |
| `Court_Year` | Year IPC was established (NA = never treated) | Year |
| `treated` | = 1 if province has IPC and year ≥ Court_Year | Binary |
| `ever_treated` | = 1 if province ever received IPC | Binary |
| `rel_time` | Years since IPC establishment | Integer |

**Unit note:** `fti_pc = foreign_total_investment / population_province` gives USD per person (万美元 / 万人 = USD/person). Aggregate effects in `get_corrected_estimates()` convert back: gap_pc × population → 万美元, then ÷100 → million USD.

---

## Google Sheets / Authentication Note

The original covariate data was assembled from a Google Sheet. **Replicators do not need Google authentication** — the processed file `data/combined_covariates.csv` is included and loaded directly by `02_build_panel.R`. There is no Google Sheets code in any of the runnable scripts.

If you want to understand how the covariate data was originally pulled, the full (commented-out) pipeline is documented in `code/preprocessing_transparency_only.R`.

---

## Outputs

| File | Description |
|------|-------------|
| `output/figures/fig_scm_path_beijing.png` | Main SCM path plot — Beijing |
| `output/figures/fig_scm_gap_beijing.png` | Gap plot — Beijing |
| `output/figures/fig_scm_path_guangdong.png` | Main SCM path plot — Guangdong |
| `output/figures/fig_scm_gap_guangdong.png` | Gap plot — Guangdong |
| `output/figures/fig_placebo_spaghetti_*.png` | In-space placebo spaghetti plots |
| `output/figures/fig_mspe_dotplot_*.png` | MSPE ratio dot plots |
| `output/figures/fig_placebo_intime_*.png` | In-time placebo plots |
| `output/figures/fig_b1_*.png` | Appendix B1: later-treated exclusion |
| `output/figures/fig_b2_*.png` | Appendix B2: anticipation placebo |
| `output/tables/tab_estimates.csv` | Yearly aggregate FTI effect estimates |
| `output/tables/tab_robustness_full.csv` | Full robustness table (all MSPE ratios) |

---

## Software and Package Versions

| Package | Purpose |
|---------|---------|
| `Synth` | Synthetic control estimation |
| `tidyverse` | Data wrangling |
| `ggplot2` | Visualization |
| `ggthemes` | Plot theme |
| `fixest` | (Available for additional FE models) |
| `did` | (Available for DiD robustness) |
| `corrplot` | Correlation heatmap |
| `stringi` | Pinyin transliteration |
| `googlesheets4` | Google Sheets I/O (preprocessing only) |

To record your exact package versions after running:
```r
writeLines(capture.output(sessionInfo()), "session_info.txt")
```

---

## Citation

If you use this replication package, please cite:

> [YOUR FULL CITATION HERE]

---

## License

[e.g., MIT License / CC BY 4.0 — choose one and add a LICENSE file]
