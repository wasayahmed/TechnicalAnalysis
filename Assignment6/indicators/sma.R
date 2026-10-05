# AI Assistance Declaration: I used Claude (Anthropic, Opus 5.5, via Claude Cowork,
# 2026-10-04) as a design assistant to review my translation of the provided
# pseudocode into R and to suggest edge-case tests. Prompts used: see
# AI_Appendix.md. I verified outputs against hand calculations and independent
# reference implementations (TTR, base R, lm()) in test_indicators.R. All final
# calculations are done by myself. I am responsible for the accuracy and
# originality of this work.
# Author: Wasay Ahmed | BDA400 Assignment 5 | Only base R is used below.

# Simple Moving Average (SMA)
# data   : numeric vector
# period : window size (integer >= 1)
# returns: numeric vector of length (length(data) - period + 1);
#          element i is the mean of data[i:(i + period - 1)]
sma <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (!is.numeric(data)) stop("data must be numeric")
  if (length(period) != 1 || period < 1 || period != as.integer(period))
    stop("period must be a single positive integer")
  if (length(data) < period)
    stop("Data length should be greater than or equal to the period")

  # Initialize a vector to store the SMA values
  n_out <- length(data) - period + 1
  sma_values <- numeric(n_out)

  # Calculate SMA for each window of 'period' data points
  for (i in seq_len(n_out)) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }

  return(sma_values)
}
