# AI Assistance Declaration: I used Claude (Anthropic, Opus 5.5, via Claude Cowork,
# 2026-10-04) as a design assistant to review my translation of the provided
# pseudocode into R and to suggest edge-case tests. Prompts used: see
# AI_Appendix.md. I verified outputs against hand calculations and independent
# reference implementations (TTR, base R, lm()) in test_indicators.R. All final
# calculations are done by myself. I am responsible for the accuracy and
# originality of this work.
# Author: Wasay Ahmed | BDA400 Assignment 5 | Only base R is used below.

# Population Standard Deviation (divides by n, as in the given formula)
# Note: base R sd() divides by (n - 1); stdev(x) == sd(x) * sqrt((n - 1) / n)
stdev <- function(data) {
  if (!is.numeric(data)) stop("data must be numeric")
  if (length(data) == 0) stop("data must contain at least one value")

  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)

  # Calculate the differences between the data points and the mean
  diff_values <- data - mean_value

  # Calculate the squared differences
  squared_diff <- diff_values * diff_values

  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)

  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}
