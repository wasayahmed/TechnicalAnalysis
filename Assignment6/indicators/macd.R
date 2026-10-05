# AI Assistance Declaration: I used Claude (Anthropic, Opus 5.5, via Claude Cowork,
# 2026-10-04) as a design assistant to review my translation of the provided
# pseudocode into R and to suggest edge-case tests. Prompts used: see
# AI_Appendix.md. I verified outputs against hand calculations and independent
# reference implementations (TTR, base R, lm()) in test_indicators.R. All final
# calculations are done by myself. I am responsible for the accuracy and
# originality of this work.
# Author: Wasay Ahmed | BDA400 Assignment 5 | Only base R is used below.

# Moving Average Convergence Divergence (MACD)
# Depends on ema() from ema.R
# (run from the Assignment5 folder so the relative path resolves)
if (!exists("ema", mode = "function")) source("ema.R")

macd <- function(data, short_period, long_period, signal_period) {
  if (short_period >= long_period) stop("short_period must be less than long_period")

  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema  <- ema(data, long_period)

  # Calculate the MACD line
  macd_line <- short_ema - long_ema

  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)

  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line

  # Return the MACD line, signal line, and histogram as a list
  result <- list(macd_line = macd_line,
                 signal_line = signal_line,
                 histogram = histogram)
  return(result)
}
