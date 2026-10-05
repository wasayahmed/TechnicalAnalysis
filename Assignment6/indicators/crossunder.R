# AI Assistance Declaration: I used Claude (Anthropic, Opus 5.5, via Claude Cowork,
# 2026-10-04) as a design assistant to review my translation of the provided
# pseudocode into R and to suggest edge-case tests. Prompts used: see
# AI_Appendix.md. I verified outputs against hand calculations and independent
# reference implementations (TTR, base R, lm()) in test_indicators.R. All final
# calculations are done by myself. I am responsible for the accuracy and
# originality of this work.
# Author: Wasay Ahmed | BDA400 Assignment 5 | Only base R is used below.

# Crossunder: TRUE at index i when arr1 moves from >= arr2 (at i-1) to < arr2 (at i)
# returns: logical vector, same length as the inputs; the first element is FALSE.
crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) stop("Both arrays should have the same length")

  # Initialize a vector to store the crossunder signals
  crossunder_signals <- rep(FALSE, length(arr1))

  # Check for crossunder signals at each data point
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      hit <- arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]
      crossunder_signals[i] <- isTRUE(hit)
    }
  }

  return(crossunder_signals)
}
