# AI Assistance Declaration: I used Claude (Anthropic, Opus 5.5, via Claude Cowork,
# 2026-10-04) as a design assistant to review my translation of the provided
# pseudocode into R and to suggest edge-case tests. Prompts used: see
# AI_Appendix.md. I verified outputs against hand calculations and independent
# reference implementations (TTR, base R, lm()) in test_indicators.R. All final
# calculations are done by myself. I am responsible for the accuracy and
# originality of this work.
# Author: Wasay Ahmed | BDA400 Assignment 5 | Only base R is used below.

# Relative Strength Index (RSI) with Wilder's smoothing
# returns: numeric vector, same length as data. The first 'period' values are
#          NA because 'period' price changes are needed for the first RSI.
#
# Implementation notes (deviations from the pseudocode, both deliberate):
#  1. Gains and losses are both filled for every change (gain = 0 when the price
#     falls, loss = 0 when it rises). In the pseudocode the "else" branch leaves
#     gains[i] empty, which would make mean() return NA.
#  2. The first RSI value (at position period + 1) uses the simple average of the
#     first 'period' gains/losses. Wilder smoothing then starts at period + 2.
#     The pseudocode smooths at period + 1 as well, which counts the
#     period-th change twice.
rsi <- function(data, period = 14) {
  if (!is.numeric(data)) stop("data must be numeric")
  if (period < 1) stop("period must be a positive integer")
  n <- length(data)

  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, n)
  if (n <= period) {
    warning("Not enough data for RSI: need more than 'period' values; returning NA")
    return(rsi_values)
  }

  # Calculate the differences between consecutive data points
  diff_values <- data[-1] - data[-n]

  # Initialize two vectors to store the gains and losses
  gains  <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # helper: convert average gain / loss into an RSI value
  to_rsi <- function(avg_gain, avg_loss) {
    if (avg_loss == 0 && avg_gain == 0) return(50)   # flat prices
    if (avg_loss == 0) return(100)                    # only gains
    rs <- avg_gain / avg_loss
    100 - (100 / (1 + rs))
  }

  # Calculate the average gains and average losses for the first 'period' changes
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  rsi_values[period + 1] <- to_rsi(avg_gain, avg_loss)

  # Calculate RSI values using the Wilder's smoothing method
  if (n >= period + 2) {
    for (i in (period + 2):n) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
      rsi_values[i] <- to_rsi(avg_gain, avg_loss)
    }
  }

  return(rsi_values)
}
