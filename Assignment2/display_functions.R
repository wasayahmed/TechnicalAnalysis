# =============================================================================
# display_functions.R
# Author : Wasay Ahmed | BDA400 Data Science Tools and Techniques | Assignment 2
# Purpose: Custom functions to display the imported data frames and the
#          calculated statistics, in several ways inspired by how Yahoo
#          Finance presents market data (quote summary, historical table,
#          price chart with moving averages, volume bars, comparison chart).
# =============================================================================

suppressPackageStartupMessages(library(ggplot2))

fmt_num <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = ",")

# 1. Historical data table (Yahoo "Historical Data" tab style) ----------------
display_stock_table <- function(stock_df, n = 10, latest_first = TRUE) {
  sym <- attr(stock_df, "symbol")
  cat(sprintf("\n===== %s : historical prices (%s) =====\n", sym, attr(stock_df, "source")))
  df <- stock_df[, c("Date", "Open", "High", "Low", "Close", "Adjusted", "Volume")]
  if (latest_first) df <- df[order(df$Date, decreasing = TRUE), ]
  out <- head(df, n)
  out$Volume <- fmt_num(out$Volume, 0)
  print(out, row.names = FALSE)
  invisible(out)
}

# 2. Quote summary (Yahoo "Summary" panel style) -------------------------------
display_quote_summary <- function(stock_df) {
  n    <- nrow(stock_df)
  last <- stock_df[n, ]
  prev <- stock_df[n - 1, ]
  yr   <- stock_df[stock_df$Date > max(stock_df$Date) - 365, ]
  chg  <- last$Close - prev$Close
  cat(sprintf("\n%s  %s   %s (%+.2f%%)   as of %s\n",
              attr(stock_df, "symbol"), fmt_num(last$Close),
              sprintf("%+.2f", chg), 100 * chg / prev$Close, last$Date))
  info <- data.frame(
    Field = c("Previous Close", "Open", "Day's Range", "52 Week Range",
              "Volume", "Avg. Volume (3M)"),
    Value = c(fmt_num(prev$Close), fmt_num(last$Open),
              paste(fmt_num(last$Low), "-", fmt_num(last$High)),
              paste(fmt_num(min(yr$Low)), "-", fmt_num(max(yr$High))),
              fmt_num(last$Volume, 0),
              fmt_num(mean(tail(stock_df$Volume, 63)), 0)))
  print(info, row.names = FALSE, right = FALSE)
  invisible(info)
}

# 3. Statistics table for the whole portfolio ---------------------------------
display_statistics <- function(summary_df) {
  cat("\n===== Portfolio statistics (Close price) =====\n")
  show <- summary_df[, c("Symbol", "Observations", "Last_Close", "Mean", "Median",
                         "Mode", "Std_Dev", "MA_Short_Latest", "MA_Long_Latest",
                         "Volatility_pct")]
  num <- sapply(show, is.numeric) & names(show) != "Observations"
  show[num] <- lapply(show[num], round, 2)
  print(show, row.names = FALSE)
  invisible(show)
}

# 4. Price chart with moving averages ------------------------------------------
plot_price_ma <- function(detail_df, ma_cols = c("MA20", "MA50")) {
  sym  <- attr(detail_df, "symbol")
  long <- rbind(
    data.frame(Date = detail_df$Date, Value = detail_df$Close, Series = "Close"),
    do.call(rbind, lapply(ma_cols, function(m)
      data.frame(Date = detail_df$Date, Value = detail_df[[m]], Series = m))))
  ggplot(long, aes(Date, Value, colour = Series)) +
    geom_line(linewidth = 0.6, na.rm = TRUE) +
    scale_colour_manual(values = c(Close = "grey30", MA20 = "#1f77b4", MA50 = "#d62728")) +
    labs(title = paste(sym, "closing price with 20- and 50-day moving averages"),
         subtitle = paste("Source:", attr(detail_df, "source")),
         x = NULL, y = "Price (USD)", colour = NULL) +
    theme_minimal(base_size = 11) + theme(legend.position = "top")
}

# 5. Volume bars ----------------------------------------------------------------
plot_volume <- function(stock_df) {
  df <- stock_df
  df$Direction <- ifelse(df$Close >= df$Open, "Up day", "Down day")
  ggplot(df, aes(Date, Volume / 1e6, fill = Direction)) +
    geom_col(width = 1) +
    scale_fill_manual(values = c("Up day" = "#2ca02c", "Down day" = "#d62728")) +
    labs(title = paste(attr(stock_df, "symbol"), "daily volume"),
         x = NULL, y = "Volume (millions)", fill = NULL) +
    theme_minimal(base_size = 11) + theme(legend.position = "top")
}

# 6. Portfolio comparison: growth of $100 ---------------------------------------
plot_portfolio_comparison <- function(stock_list) {
  df <- do.call(rbind, lapply(names(stock_list), function(s) {
    d <- stock_list[[s]]
    data.frame(Date = d$Date, Symbol = s, Index = 100 * d$Close / d$Close[1])
  }))
  ggplot(df, aes(Date, Index, colour = Symbol)) +
    geom_line(linewidth = 0.6) +
    geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
    labs(title = "Portfolio comparison: growth of $100 invested on the first day",
         x = NULL, y = "Value of $100", colour = NULL) +
    theme_minimal(base_size = 11) + theme(legend.position = "top")
}

# 7. Distribution of closing prices with mean / median / mode markers ----------
plot_distribution <- function(stock_df, stats_row) {
  marks <- data.frame(Stat = c("Mean", "Median", "Mode"),
                      Value = c(stats_row$Mean, stats_row$Median, stats_row$Mode))
  ggplot(stock_df, aes(Close)) +
    geom_histogram(bins = 30, fill = "grey75", colour = "white") +
    geom_vline(data = marks, aes(xintercept = Value, colour = Stat), linewidth = 0.9) +
    labs(title = paste(attr(stock_df, "symbol"), "distribution of closing prices"),
         x = "Close (USD)", y = "Days", colour = NULL) +
    theme_minimal(base_size = 11) + theme(legend.position = "top")
}
