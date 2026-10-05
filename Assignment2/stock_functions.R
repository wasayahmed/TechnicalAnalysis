# =============================================================================
# stock_functions.R
# Author : Wasay Ahmed | BDA400 Data Science Tools and Techniques | Assignment 2
# Purpose: Utility functions to (1) read the portfolio file, (2) import stock
#          data for every symbol with quantmod, and (3) compute basic
#          statistics (moving average, mean, mode, median, standard deviation).
# =============================================================================

suppressPackageStartupMessages({
  library(quantmod)   # getSymbols(), OHLC helpers
  library(xts)
})

# -----------------------------------------------------------------------------
# read_portfolio(): read one ticker symbol per line, ignore blanks and comments
# -----------------------------------------------------------------------------
read_portfolio <- function(portfolio_file = "portfolio.txt") {
  if (!file.exists(portfolio_file)) stop("Portfolio file not found: ", portfolio_file)
  symbols <- trimws(readLines(portfolio_file, warn = FALSE))
  symbols <- toupper(symbols[symbols != "" & !startsWith(symbols, "#")])
  unique(symbols)
}

# -----------------------------------------------------------------------------
# xts_to_df(): convert a quantmod xts object into a tidy data frame
# -----------------------------------------------------------------------------
xts_to_df <- function(x, symbol) {
  df <- data.frame(
    Date     = as.Date(index(x)),
    Open     = as.numeric(Op(x)),
    High     = as.numeric(Hi(x)),
    Low      = as.numeric(Lo(x)),
    Close    = as.numeric(Cl(x)),
    Volume   = as.numeric(Vo(x)),
    Adjusted = if (has.Ad(x)) as.numeric(Ad(x)) else as.numeric(Cl(x))
  )
  df <- df[!is.na(df$Close), ]          # drop holidays / missing rows
  rownames(df) <- NULL
  attr(df, "symbol") <- symbol
  df
}

# -----------------------------------------------------------------------------
# simulate_stock_data(): OFFLINE FALLBACK ONLY.
# If Yahoo Finance cannot be reached (no internet, firewall, rate limit) this
# creates a clearly-labelled synthetic OHLCV series so the rest of the code can
# still be demonstrated. Data source is stored in attr(df, "source").
# -----------------------------------------------------------------------------
simulate_stock_data <- function(symbol, from, to) {
  start_prices <- c(AAPL = 185, MSFT = 375, GOOGL = 140, AMZN = 150, NVDA = 48)
  s0 <- if (symbol %in% names(start_prices)) start_prices[[symbol]] else 100
  set.seed(sum(utf8ToInt(symbol)))                       # reproducible per symbol
  dates <- seq(as.Date(from), as.Date(to), by = "day")
  dates <- dates[!format(dates, "%u") %in% c("6", "7")]  # weekdays only
  n     <- length(dates)
  ret   <- rnorm(n, mean = 0.0006, sd = 0.017)
  close <- s0 * cumprod(1 + ret)
  open  <- c(s0, head(close, -1)) * (1 + rnorm(n, 0, 0.004))
  high  <- pmax(open, close) * (1 + abs(rnorm(n, 0, 0.007)))
  low   <- pmin(open, close) * (1 - abs(rnorm(n, 0, 0.007)))
  df <- data.frame(Date = dates, Open = round(open, 2), High = round(high, 2),
                   Low = round(low, 2), Close = round(close, 2),
                   Volume = round(rlnorm(n, log(4e7), 0.35)),
                   Adjusted = round(close, 2))
  attr(df, "symbol") <- symbol
  df
}

# -----------------------------------------------------------------------------
# load_stock_data(): read portfolio.txt and import data for every symbol.
#   returns  : a named list of data frames, one per symbol (list$AAPL, ...)
#   assign_env: if an environment is given, each data frame is also stored
#               there as a separate object, e.g. AAPL_df, MSFT_df.
# -----------------------------------------------------------------------------
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = "2025-01-01",
                            to   = Sys.Date(),
                            src  = "yahoo",
                            use_fallback = TRUE,
                            assign_env = NULL) {
  symbols <- read_portfolio(portfolio_file)
  message("Symbols in portfolio: ", paste(symbols, collapse = ", "))

  stock_list <- list()
  for (sym in symbols) {
    df <- tryCatch({
      x <- getSymbols(sym, src = src, from = from, to = to, auto.assign = FALSE)
      out <- xts_to_df(x, sym)
      attr(out, "source") <- paste("quantmod /", src)
      out
    }, error = function(e) {
      if (!use_fallback) stop("Download failed for ", sym, ": ", conditionMessage(e))
      warning("Download failed for ", sym, " (", conditionMessage(e),
              "). Using SIMULATED data instead.", call. = FALSE)
      out <- simulate_stock_data(sym, from, to)
      attr(out, "source") <- "SIMULATED (offline fallback)"
      out
    })
    stock_list[[sym]] <- df
    if (!is.null(assign_env)) assign(paste0(sym, "_df"), df, envir = assign_env)
    message(sprintf("  %-6s %4d rows  %s to %s  [%s]", sym, nrow(df),
                    min(df$Date), max(df$Date), attr(df, "source")))
  }
  stock_list
}

# -----------------------------------------------------------------------------
# moving_average(): trailing simple moving average, NA for the first n-1 rows
# -----------------------------------------------------------------------------
moving_average <- function(x, n = 20) {
  if (length(x) < n) return(rep(NA_real_, length(x)))
  ma <- stats::filter(x, rep(1 / n, n), sides = 1)
  as.numeric(ma)
}

# -----------------------------------------------------------------------------
# stat_mode(): base R has no mode() for data values, so write one.
# Prices are continuous, so they are rounded (default to whole dollars) before
# counting; ties return the smallest most frequent value.
# -----------------------------------------------------------------------------
stat_mode <- function(x, digits = 0) {
  x <- round(x[!is.na(x)], digits)
  if (length(x) == 0) return(NA_real_)
  counts <- table(x)
  as.numeric(names(counts)[which.max(counts)])
}

# -----------------------------------------------------------------------------
# calculate_statistics(): statistics for ONE stock data frame
#   returns a list with
#     $summary : one-row data frame (mean, mode, median, sd, min, max, ...)
#     $data    : the input data frame with MA columns added
# -----------------------------------------------------------------------------
calculate_statistics <- function(stock_df, ma_short = 20, ma_long = 50,
                                 column = "Close") {
  if (!column %in% names(stock_df)) stop("Column not found: ", column)
  x <- stock_df[[column]]

  stock_df[[paste0("MA", ma_short)]] <- moving_average(x, ma_short)
  stock_df[[paste0("MA", ma_long)]]  <- moving_average(x, ma_long)
  stock_df$Daily_Return <- c(NA, diff(x) / head(x, -1))

  summary_df <- data.frame(
    Symbol          = attr(stock_df, "symbol") %||% NA,
    Observations    = length(x),
    First_Date      = min(stock_df$Date),
    Last_Date       = max(stock_df$Date),
    Last_Close      = tail(x, 1),
    Mean            = mean(x, na.rm = TRUE),
    Median          = median(x, na.rm = TRUE),
    Mode            = stat_mode(x),
    Std_Dev         = sd(x, na.rm = TRUE),
    Min             = min(x, na.rm = TRUE),
    Max             = max(x, na.rm = TRUE),
    MA_Short_Latest = tail(stock_df[[paste0("MA", ma_short)]], 1),
    MA_Long_Latest  = tail(stock_df[[paste0("MA", ma_long)]], 1),
    Avg_Daily_Return_pct = 100 * mean(stock_df$Daily_Return, na.rm = TRUE),
    Volatility_pct  = 100 * sd(stock_df$Daily_Return, na.rm = TRUE),
    Avg_Volume      = mean(stock_df$Volume, na.rm = TRUE)
  )
  list(summary = summary_df, data = stock_df)
}

# small helper used above (base R only has %||% from R 4.4)
`%||%` <- function(a, b) if (is.null(a)) b else a

# -----------------------------------------------------------------------------
# calculate_portfolio_statistics(): apply calculate_statistics() to every stock
#   returns list(summary = combined data frame, details = list per symbol)
# -----------------------------------------------------------------------------
calculate_portfolio_statistics <- function(stock_list, ...) {
  details <- lapply(stock_list, calculate_statistics, ...)
  summary <- do.call(rbind, lapply(details, `[[`, "summary"))
  rownames(summary) <- NULL
  list(summary = summary, details = details)
}
