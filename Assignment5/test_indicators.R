# AI Assistance Declaration: I used Claude (Anthropic, Opus 5.5, via Claude Cowork,
# 2026-10-04) as a design assistant to review my translation of the provided
# pseudocode into R and to suggest edge-case tests. Prompts used: see
# AI_Appendix.md. I verified outputs against hand calculations and independent
# reference implementations (TTR, base R, lm()) in test_indicators.R. All final
# calculations are done by myself. I am responsible for the accuracy and
# originality of this work.
# Author: Wasay Ahmed | BDA400 Assignment 5 | Only base R is used below.

# test_indicators.R
# Run from the Assignment5 folder:  Rscript test_indicators.R
# 1) runs every example from the assignment brief,
# 2) checks against hand-calculated values,
# 3) cross-checks against independent reference implementations
#    (base R, lm(), and the TTR package, used ONLY here for verification).

files <- c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
           "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R")
for (f in files) source(f)

passed <- 0; failed <- 0
check <- function(label, ok) {
  if (isTRUE(ok)) { passed <<- passed + 1; cat(sprintf("[PASS] %s\n", label)) }
  else            { failed <<- failed + 1; cat(sprintf("[FAIL] %s\n", label)) }
}
near <- function(a, b, tol = 1e-8) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol))
has_ttr <- requireNamespace("TTR", quietly = TRUE)

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
set.seed(42)
prices <- cumprod(c(100, 1 + rnorm(299, 0.0005, 0.015)))   # synthetic price path

cat("\n=== Examples from the assignment brief ===\n")
cat("sma(data, 3):\n");          print(round(sma(data, period = 3), 4))
cat("ema(data, 3):\n");          print(round(ema(data, period = 3), 4))
cat("macd(100..130, 3, 5, 2):\n"); print(lapply(macd(c(100,105,110,115,120,125,130), 3, 5, 2), round, 4))
cat("stdev(data):\n");           print(round(stdev(data), 4))
cat("rsi(c(45,...,62), 5):\n");  print(round(rsi(c(45,50,48,55,52,49,58,60,65,62), period = 5), 2))
cat("stoch_rsi(10 points, 14, 3, 3): (not enough data -> NA, with a warning)\n")
print(suppressWarnings(stoch_rsi(c(45,50,48,55,52,49,58,60,65,62), period = 14, k_period = 3, d_period = 3)))
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
cat("crossover(arr1, arr2):\n");  print(crossover(arr1, arr2))
cat("crossunder(arr1, arr2):\n"); print(crossunder(arr1, arr2))

cat("\n=== SMA ===\n")
check("SMA hand values (period 3)", near(sma(data, 3), c(37, 47, 53, 60, 65, 71, 70) / 3))
check("SMA length = n - period + 1", length(sma(prices, 20)) == 281)
check("SMA period 1 returns the data", near(sma(data, 1), data))
check("SMA errors when data shorter than period", inherits(try(sma(1:3, 5), silent = TRUE), "try-error"))
if (has_ttr) check("SMA matches TTR::SMA", near(sma(prices, 20), na.omit(TTR::SMA(prices, 20))))

cat("\n=== EMA ===\n")
# hand: k = 0.5 -> 10, 11, 13, 16.5, 17.25, ...
check("EMA hand values (period 3)", near(ema(data, 3)[1:5], c(10, 11, 13, 16.5, 17.25)))
alpha <- 2 / 21
ref_ema <- stats::filter(alpha * prices, 1 - alpha, method = "recursive", init = prices[1])
check("EMA matches stats::filter recursion", near(ema(prices, 20), ref_ema))
check("EMA of a constant is constant", near(ema(rep(5, 10), 4), rep(5, 10)))

cat("\n=== MACD ===\n")
m <- macd(prices, 12, 26, 9)
check("MACD returns 3 named components", identical(names(m), c("macd_line", "signal_line", "histogram")))
check("MACD line = EMA12 - EMA26", near(m$macd_line, ema(prices, 12) - ema(prices, 26)))
check("Histogram = MACD - signal", near(m$histogram, m$macd_line - m$signal_line))
check("MACD errors if short >= long", inherits(try(macd(prices, 26, 12, 9), silent = TRUE), "try-error"))

cat("\n=== Standard deviation ===\n")
check("stdev hand value (data) = 4.9466", abs(stdev(data) - sqrt(sum((data - mean(data))^2) / 9)) < 1e-12 && abs(stdev(data) - 4.9466) < 1e-4)
n <- length(prices)
check("stdev == sd() * sqrt((n-1)/n)", near(stdev(prices), sd(prices) * sqrt((n - 1) / n)))
check("stdev of constant is 0", stdev(rep(3, 7)) == 0)

cat("\n=== Linear regression ===\n")
lr <- linreg(prices, 50, 0)
fit <- lm(y ~ x, data.frame(x = 1:50, y = tail(prices, 50)))
check("linreg slope matches lm()", near(lr$slope, coef(fit)[2]))
check("linreg intercept matches lm()", near(lr$intercept, coef(fit)[1]))
check("linreg predictions match fitted()", near(lr$predicted_values, fitted(fit)))
lr5 <- linreg(prices, 50, 5)
check("linreg offset 5 uses prices[(n-54):(n-5)]",
      near(lr5$slope, coef(lm(prices[(n-54):(n-5)] ~ seq_len(50)))[2]))
check("perfect line recovered", near(unlist(linreg(2 * (1:10) + 3, 10, 0)[1:2]), c(2, 3)))
check("linreg errors if length > n", inherits(try(linreg(1:5, 6, 0), silent = TRUE), "try-error"))
check("linreg errors if offset >= length", inherits(try(linreg(1:20, 5, 5), silent = TRUE), "try-error"))

cat("\n=== RSI ===\n")
r <- rsi(prices, 14)
check("RSI first 14 values NA", all(is.na(r[1:14])) && !is.na(r[15]))
check("RSI within [0, 100]", all(r[!is.na(r)] >= 0 & r[!is.na(r)] <= 100))
check("RSI = 100 on a rising series", near(tail(rsi(1:30, 14), 1), 100))
# hand check on the brief's example (period 5): changes 5,-2,7,-3,-3 -> AG=2.4, AL=1.6
check("RSI hand value at position 6 = 60", near(rsi(c(45,50,48,55,52,49,58,60,65,62), 5)[6], 60))
if (has_ttr) check("RSI matches TTR::RSI (Wilder)", near(r, TTR::RSI(prices, 14), 1e-6))

cat("\n=== Stochastic RSI ===\n")
s <- stoch_rsi(prices, 14, 3, 3)
k <- s$k_line[!is.na(s$k_line)]
check("StochRSI %K within [0, 1]", all(k >= 0 & k <= 1))
check("StochRSI lines same length as data", length(s$k_line) == length(prices) && length(s$d_line) == length(prices))
if (has_ttr) {
  rr <- TTR::RSI(prices, 14)
  raw <- (rr - TTR::runMin(rr, 14)) / (TTR::runMax(rr, 14) - TTR::runMin(rr, 14))
  kk  <- TTR::SMA(raw, 3); dd <- TTR::SMA(kk, 3)
  check("StochRSI %K matches TTR-based reference", near(s$k_line, kk, 1e-6))
  check("StochRSI %D matches TTR-based reference", near(s$d_line, dd, 1e-6))
}

cat("\n=== Crossover / Crossunder ===\n")
check("crossover example: only index 4 TRUE", identical(which(crossover(arr1, arr2)), 4L))
check("crossunder example: no TRUE values", !any(crossunder(arr1, arr2)))
check("crossunder detects drop", identical(which(crossunder(c(5, 6, 4, 3), c(5, 5, 5, 5))), 3L))
check("crossover handles NA as FALSE", identical(crossover(c(NA, 1, 3), c(2, 2, 2)), c(FALSE, FALSE, TRUE)))
check("length mismatch errors", inherits(try(crossover(1:3, 1:4), silent = TRUE), "try-error"))

cat(sprintf("\n==== %d passed, %d failed ====\n", passed, failed))
