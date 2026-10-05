# =============================================================================
# main_analysis.R : configure and run the preliminary technical analysis
# Author : Wasay Ahmed | BDA400 Assignment 2
# Usage  : open the Assignment2 folder in RStudio (Session > Set Working
#          Directory > To Source File Location) and run:
#              source("main_analysis.R")
#          or from a terminal:  Rscript main_analysis.R
# =============================================================================

source("stock_functions.R")
source("display_functions.R")

# ---- 1. Configuration --------------------------------------------------------
config <- list(
  portfolio_file = "portfolio.txt",
  from           = "2025-01-01",
  to             = "2026-09-30",
  source         = "yahoo",
  ma_short       = 20,
  ma_long        = 50,
  output_dir     = "output"
)
dir.create(config$output_dir, showWarnings = FALSE)

# ---- 2. Import data (one data frame per symbol) -------------------------------
stocks <- load_stock_data(config$portfolio_file, from = config$from, to = config$to,
                          src = config$source, assign_env = globalenv())
# Each stock is now available both as stocks$AAPL and as the object AAPL_df
print(ls(pattern = "^[A-Z]+_df$"))
str(stocks[[1]])

# ---- 3. Compute statistics ----------------------------------------------------
stats <- calculate_portfolio_statistics(stocks, ma_short = config$ma_short,
                                        ma_long = config$ma_long)
write.csv(stats$summary, file.path(config$output_dir, "portfolio_statistics.csv"),
          row.names = FALSE)

# ---- 4. Display data ----------------------------------------------------------
for (sym in names(stocks)) {
  display_quote_summary(stocks[[sym]])
  display_stock_table(stocks[[sym]], n = 5)
}
display_statistics(stats$summary)

# ---- 5. Visualisations (saved to output/) -------------------------------------
for (sym in names(stocks)) {
  d <- stats$details[[sym]]$data
  attr(d, "source") <- attr(stocks[[sym]], "source")
  ggsave(file.path(config$output_dir, paste0(sym, "_price_ma.png")),
         plot_price_ma(d), width = 9, height = 4.5, dpi = 110)
}
first <- names(stocks)[1]
ggsave(file.path(config$output_dir, paste0(first, "_volume.png")),
       plot_volume(stocks[[first]]), width = 9, height = 3.5, dpi = 110)
ggsave(file.path(config$output_dir, paste0(first, "_distribution.png")),
       plot_distribution(stocks[[first]], stats$summary[stats$summary$Symbol == first, ]),
       width = 7, height = 4, dpi = 110)
ggsave(file.path(config$output_dir, "portfolio_comparison.png"),
       plot_portfolio_comparison(stocks), width = 9, height = 4.5, dpi = 110)

cat("\nDone. Charts and statistics written to", normalizePath(config$output_dir), "\n")
