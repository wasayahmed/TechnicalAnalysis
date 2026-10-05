# AI Assistance Declaration: I used Claude (Anthropic, Opus 5.5, via Claude Cowork,
# 2026-10-04) as a design assistant to review my filter and trading-rule logic.
# Prompts used: see AI_Appendix.md. I verified outputs by spot-checking signal
# dates against the indicator values printed below. All final calculations are
# done by myself. I am responsible for the accuracy and originality of this work.
# Author: Wasay Ahmed | BDA400 Assignment 5
#
# run_portfolio_analysis.R
# Builds a data frame per stock, applies the custom indicators, filters the data
# (time period, time frame, data source), screens stocks on indicator criteria,
# and generates Buy / Sell / Hold signals from trading rules.
# Run from the Assignment5 folder:  Rscript run_portfolio_analysis.R
# Data import re-uses load_stock_data() from Assignment 2 (quantmod / Yahoo,
# with a labelled offline fallback).

for (f in c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R", "rsi.R",
            "stoch_rsi.R", "crossover.R", "crossunder.R")) source(f)
source(file.path("..", "Assignment2", "stock_functions.R"))

# ---- 1. Build one data frame per symbol ---------------------------------------
stocks <- load_stock_data(file.path("..", "Assignment2", "portfolio.txt"),
                          from = "2025-01-01", to = "2026-09-30")

# ---- 2. Filters ------------------------------------------------------------------
# 2a. time period
filter_period <- function(df, start, end) {
  out <- df[df$Date >= as.Date(start) & df$Date <= as.Date(end), ]
  attributes(out)[c("symbol", "source")] <- attributes(df)[c("symbol", "source")]
  out
}

# 2b. time frame: aggregate daily bars to weekly or monthly OHLCV (base R only)
filter_timeframe <- function(df, timeframe = c("daily", "weekly", "monthly")) {
  timeframe <- match.arg(timeframe)
  if (timeframe == "daily") return(df)
  key <- if (timeframe == "weekly") format(df$Date, "%G-%V") else format(df$Date, "%Y-%m")
  groups <- split(df, factor(key, levels = unique(key)))
  out <- do.call(rbind, lapply(groups, function(g) data.frame(
    Date = max(g$Date), Open = g$Open[1], High = max(g$High), Low = min(g$Low),
    Close = g$Close[nrow(g)], Volume = sum(g$Volume), Adjusted = g$Adjusted[nrow(g)])))
  rownames(out) <- NULL
  attributes(out)[c("symbol", "source")] <- attributes(df)[c("symbol", "source")]
  out
}

# 2c. data source: which price column the indicators use
#     ("Close", "Open", "High", "Low", "Adjusted", or "HL2" = (High + Low) / 2)
price_source <- function(df, source = "Close") {
  if (source == "HL2") return((df$High + df$Low) / 2)
  df[[source]]
}

# ---- 3. Indicators added as columns ----------------------------------------------
pad_front <- function(x, n) c(rep(NA_real_, n - length(x)), x)

add_indicators <- function(df, src = "Close", fast = 20, slow = 50) {
  p <- price_source(df, src)
  n <- length(p)
  df$SMA_fast <- if (n >= fast) pad_front(sma(p, fast), n) else NA
  df$SMA_slow <- if (n >= slow) pad_front(sma(p, slow), n) else NA
  df$EMA_fast <- ema(p, fast)
  m <- macd(p, 12, 26, 9)
  df$MACD <- m$macd_line; df$MACD_signal <- m$signal_line; df$MACD_hist <- m$histogram
  df$RSI  <- suppressWarnings(rsi(p, 14))
  s <- suppressWarnings(stoch_rsi(p, 14, 3, 3))
  df$StochK <- s$k_line; df$StochD <- s$d_line
  df
}

# ---- 4. Trading rules ---------------------------------------------------------------
# Rule 1 (trend):   SMA_fast crosses over SMA_slow  -> Buy
#                   SMA_fast crosses under SMA_slow -> Sell
# Rule 2 (momentum): MACD crosses over its signal while RSI < 70 -> Buy
#                    MACD crosses under its signal while RSI > 30 -> Sell
# Otherwise Hold. If rules disagree on the same day, Hold.
apply_trading_rules <- function(df) {
  ma_up   <- crossover(df$SMA_fast, df$SMA_slow)
  ma_down <- crossunder(df$SMA_fast, df$SMA_slow)
  mo_up   <- crossover(df$MACD, df$MACD_signal) & !is.na(df$RSI) & df$RSI < 70
  mo_down <- crossunder(df$MACD, df$MACD_signal) & !is.na(df$RSI) & df$RSI > 30
  buy  <- ma_up | mo_up
  sell <- ma_down | mo_down
  df$Signal <- ifelse(buy & !sell, "Buy", ifelse(sell & !buy, "Sell", "Hold"))
  df$Rule   <- ifelse(ma_up | ma_down, "MA cross", ifelse(mo_up | mo_down, "MACD+RSI", ""))
  df
}

# ---- 5. Run for every stock ------------------------------------------------------------
period_start <- "2025-06-01"; period_end <- "2026-09-30"
results <- lapply(stocks, function(df) {
  d <- filter_period(df, "2025-01-01", period_end)   # keep history for warm-up
  d <- add_indicators(d, src = "Close")
  d <- apply_trading_rules(d)
  d[d$Date >= as.Date(period_start), ]
})

# ---- 6. Screen stocks on indicator criteria (latest bar) ----------------------------
latest <- do.call(rbind, lapply(names(results), function(s) {
  d <- results[[s]]; r <- d[nrow(d), ]
  lr <- linreg(d$Close, 20, 0)
  data.frame(Symbol = s, Date = r$Date, Close = round(r$Close, 2),
             RSI = round(r$RSI, 1), StochK = round(r$StochK, 2),
             MACD_hist = round(r$MACD_hist, 2),
             Trend_20d_slope = round(lr$slope, 3),
             Volatility_20d = round(stdev(tail(d$Close, 20)), 2),
             Above_SMA50 = r$Close > r$SMA_slow,
             Last_Signal = tail(d$Signal[d$Signal != "Hold"], 1) %||% "None")
}))
cat("\n===== Latest indicator snapshot =====\n"); print(latest, row.names = FALSE)

cat("\n===== Screen A: uptrend (Close above SMA50 and positive 20-day regression slope) =====\n")
print(subset(latest, Above_SMA50 & Trend_20d_slope > 0)$Symbol)
cat("\n===== Screen B: oversold (RSI < 35 or StochRSI %K < 0.2) =====\n")
print(subset(latest, RSI < 35 | StochK < 0.2)$Symbol)

cat("\n===== Trading signals since", period_start, "=====\n")
sig <- do.call(rbind, lapply(names(results), function(s) {
  d <- results[[s]]; d <- d[d$Signal != "Hold", c("Date", "Close", "RSI", "Signal", "Rule")]
  if (nrow(d)) cbind(Symbol = s, d) else NULL
}))
sig$Close <- round(sig$Close, 2); sig$RSI <- round(sig$RSI, 1)
print(sig, row.names = FALSE)
write.csv(sig, "trading_signals.csv", row.names = FALSE)

cat("\n===== Weekly time-frame example (first stock, last 6 weeks) =====\n")
print(tail(filter_timeframe(stocks[[1]], "weekly"), 6), row.names = FALSE)
