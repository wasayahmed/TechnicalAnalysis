# Author: Wasay Ahmed | BDA400 Assignment 2
# install_packages.R : one-time installation of the packages used in the
# TechnicalAnalysis project. Run once in RStudio: source("install_packages.R")

required <- c("quantmod",   # downloads prices from Yahoo Finance
              "TTR",        # Technical Trading Rules (installed with quantmod)
              "xts", "zoo", # time-series containers used by quantmod
              "ggplot2",    # charts
              "knitr",      # tables in R Markdown
              "rmarkdown")  # knitting the report

missing <- required[!required %in% rownames(installed.packages())]
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All packages already installed.")
}

# Confirm that each package loads and print its version
for (p in required) {
  suppressPackageStartupMessages(library(p, character.only = TRUE))
  cat(sprintf("%-10s %s\n", p, as.character(packageVersion(p))))
}
