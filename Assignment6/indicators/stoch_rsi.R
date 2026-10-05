# AI Assistance Declaration: I used Claude (Anthropic, Opus 5.5, via Claude Cowork,
# 2026-10-04) as a design assistant to review my translation of the provided
# pseudocode into R and to suggest edge-case tests. Prompts used: see
# AI_Appendix.md. I verified outputs against hand calculations and independent
# reference implementations (TTR, base R, lm()) in test_indicators.R. All final
# calculations are done by myself. I am responsible for the accuracy and
# originality of this work.
# Author: Wasay Ahmed | BDA400 Assignment 5 | Only base R is used below.

# Stochastic RSI (StochRSI)
# Depends on rsi() and sma().  (run from the Assignment5 folder)
if (!exists("rsi", mode = "function")) source("rsi.R")
if (!exists("sma", mode = "function")) source("sma.R")

# data     : numeric vector of prices
# period   : RSI period, also used as the StochRSI look-back window
# k_period : smoothing period for %K (SMA of raw StochRSI)
# d_period : smoothing period for %D (SMA of %K, usually 3)
# returns  : list(k_line, d_line), both the same length as data, padded with NA
#
# Implementation note: the formula text defines RSI_lowest / RSI_highest over a
# look-back period (usually 14), so a ROLLING min/max is used. The pseudocode's
# single global min/max would let one extreme RSI value months ago flatten every
# later reading. Raw values are on a 0..1 scale, matching the description.
stoch_rsi <- function(data, period = 14, k_period = 3, d_period = 3) {
  n <- length(data)

  # helper: apply sma() to the non-NA tail of x and pad the front with NA
  sma_padded <- function(x, p) {
    out <- rep(NA_real_, length(x))
    ok  <- which(!is.na(x))
    if (length(ok) < p) return(out)
    first <- ok[1]
    vals  <- sma(x[first:length(x)], p)
    out[(first + p - 1):length(x)] <- vals
    out
  }

  # Calculate the RSI
  rsi_values <- suppressWarnings(rsi(data, period))

  # Calculate the StochRSI (rolling min / max of RSI over 'period' values)
  stoch <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (i - period + 1 < 1) next
    window <- rsi_values[(i - period + 1):i]
    if (anyNA(window)) next
    lo <- min(window); hi <- max(window)
    stoch[i] <- if (hi == lo) 0.5 else (rsi_values[i] - lo) / (hi - lo)
  }

  # Calculate the %K line (StochRSI smoothed)
  k_line <- sma_padded(stoch, k_period)

  # Calculate the %D line (simple moving average of %K)
  d_line <- sma_padded(k_line, d_period)

  if (all(is.na(k_line)))
    warning("Not enough data for StochRSI with these periods; returning NA")

  # Return the %K and %D lines as a list
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}
