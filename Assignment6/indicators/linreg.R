# AI Assistance Declaration: I used Claude (Anthropic, Opus 5.5, via Claude Cowork,
# 2026-10-04) as a design assistant to review my translation of the provided
# pseudocode into R and to suggest edge-case tests. Prompts used: see
# AI_Appendix.md. I verified outputs against hand calculations and independent
# reference implementations (TTR, base R, lm()) in test_indicators.R. All final
# calculations are done by myself. I am responsible for the accuracy and
# originality of this work.
# Author: Wasay Ahmed | BDA400 Assignment 5 | Only base R is used below.

# Linear Regression over the last 'regressionLength' points of a series
# regressionSource : numeric vector (e.g. closing prices)
# regressionLength : number of points used in the regression window
# regressionOffset : how many bars back from the end the window finishes
#                    (0 = window ends on the latest value)
# returns: list(slope, intercept, predicted_values)
#          x is the position inside the window, 1..regressionLength
#
# Implementation note: the provided pseudocode uses
#   start = n - regressionLength + regressionOffset, end = n - regressionOffset,
# which selects regressionLength + 1 points when the offset is 0 and shrinks the
# window as the offset grows. I use start = n - regressionLength + 1 - offset so
# the window always contains exactly regressionLength points and simply slides
# back by 'offset' bars (the standard definition, e.g. TradingView linreg()).
linreg <- function(regressionSource, regressionLength, regressionOffset = 0) {
  # Calculate the total number of elements in the regressionSource
  n <- length(regressionSource)

  # Check if regressionLength is greater than the number of elements in regressionSource
  if (regressionLength > n)
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  if (regressionLength < 2) stop("regressionLength must be at least 2")

  # Check if regressionOffset is greater than or equal to regressionLength
  if (regressionOffset >= regressionLength)
    stop("regressionOffset must be less than regressionLength")
  if (regressionOffset < 0) stop("regressionOffset cannot be negative")

  # Calculate the starting index for the regressionSource
  start_index <- n - regressionLength + 1 - regressionOffset
  if (start_index < 1)
    stop("Not enough data: regressionLength + regressionOffset exceeds the series length")

  # Calculate the ending index for the regressionSource
  end_index <- n - regressionOffset

  # Extract the relevant portion of regressionSource
  source_subset <- regressionSource[start_index:end_index]

  # Calculate the index values for the regression points
  index_values <- seq_along(source_subset)

  # Calculate the sum of index values and the sum of source_subset
  sum_index  <- sum(index_values)
  sum_source <- sum(source_subset)

  # Calculate the mean of index values and the mean of source_subset
  mean_index  <- sum_index / length(index_values)
  mean_source <- sum_source / length(source_subset)

  # Calculate the numerator and denominator for the linear regression formula
  numerator   <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)

  # Calculate the slope and intercept of the linear regression line
  slope     <- numerator / denominator
  intercept <- mean_source - slope * mean_index

  # Calculate the predicted values for the regressionSource
  predicted_values <- slope * index_values + intercept

  # Return the slope, intercept, and predicted values as a list
  result <- list(slope = slope,
                 intercept = intercept,
                 predicted_values = predicted_values)
  return(result)
}
