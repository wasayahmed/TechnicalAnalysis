# AI Assistance Declaration: I used Claude (Anthropic, Opus 5.5, via Claude Cowork,
# 2026-10-04) as a design assistant to review my translation of the provided
# pseudocode into R and to suggest edge-case tests. Prompts used: see
# AI_Appendix.md. I verified outputs against hand calculations and independent
# reference implementations (TTR, base R, lm()) in test_indicators.R. All final
# calculations are done by myself. I am responsible for the accuracy and
# originality of this work.
# Author: Wasay Ahmed | BDA400 Assignment 5 | Only base R is used below.

# Exponential Moving Average (EMA)
# EMA(i) = (Price(i) - EMA(i-1)) * multiplier + EMA(i-1), multiplier = 2/(period+1)
# The first EMA value is seeded with the first data point (as in the pseudocode).
# returns: numeric vector, same length as data
ema <- function(data, period) {
  if (!is.numeric(data)) stop("data must be numeric")
  if (length(period) != 1 || period < 1) stop("period must be a positive number")
  if (length(data) == 0) return(numeric(0))

  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)

  # Initialize an empty array to store EMA values
  ema_values <- numeric(length(data))

  # Loop through the data array
  for (i in seq_along(data)) {
    if (i == 1) {
      # Calculate EMA for the first data point
      ema_values[i] <- data[i]
    } else {
      # Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}
