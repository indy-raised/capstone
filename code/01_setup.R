# ==============================================================================
# 01_setup.R
#
# Installs all required R packages for this replication package.
# Run this script ONCE before running any analysis scripts.
#
# R version used by authors: [YOUR R VERSION, e.g. 4.3.2]
# ==============================================================================

install_if_missing <- function(pkgs) {
  to_install <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(to_install)) install.packages(to_install)
}

pkgs <- c(
  # Data I/O
  "haven", "readr", "googlesheets4",
  # Data wrangling
  "tidyverse", "tidyr", "stringr", "stringi",
  # Causal inference
  "Synth", "did", "fixest",
  # Visualization
  "ggplot2", "ggthemes", "corrplot",
  # Utilities
  "forcats", "scales", "glue"
)

install_if_missing(pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

cat("All packages installed and loaded successfully.\n")

# ==============================================================================
# GLOBAL PLOT THEME
# Defined here and sourced by all downstream scripts.
# ==============================================================================

library(ggthemes)

theme_ipc <- function(base_size = 12) {
  ggthemes::theme_few(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold"),
      plot.subtitle    = element_text(color = "gray30"),
      axis.title       = element_text(face = "bold"),
      legend.title     = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

theme_set(theme_ipc())

cat("Global plot theme (theme_ipc) set.\n")
