# =============================================================================
# Portfolio Technical Analysis Dashboard (R Shiny)
# Author : Wasay Ahmed | BDA400 Data Science Tools and Techniques | Assignment 6
# Stage 3 of the TechnicalAnalysis project: visualisation, indicator overlays,
# trading rules and chart annotations.
#
# How to run (RStudio): open this file and click "Run App", or in the console
#     shiny::runApp("Assignment6")      # from the repository root
# The indicator functions in indicators/ are my own base-R implementations
# from Assignment 5 (sma, ema, macd, stdev, linreg, rsi, stoch_rsi,
# crossover, crossunder).
# =============================================================================

# -----------------------------------------------------------------------------
# STEP 1: DATA COLLECTION AND SETUP
# -----------------------------------------------------------------------------
# Packages (install once):
#   install.packages(c("shiny", "ggplot2", "quantmod", "patchwork", "DT"))
library(shiny)
library(ggplot2)
suppressPackageStartupMessages(library(quantmod))
library(patchwork)   # stacks the price chart and indicator panels
library(DT)          # interactive tables

# Assignment 5 indicators (base R implementations)
for (f in c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R", "rsi.R",
            "stoch_rsi.R", "crossover.R", "crossunder.R")) {
  source(file.path("indicators", f), local = TRUE)
}

# Portfolio symbols from Assignment 2
portfolio <- if (file.exists("portfolio.txt")) {
  s <- trimws(readLines("portfolio.txt", warn = FALSE)); s[s != ""]
} else c("AAPL", "MSFT", "GOOGL", "AMZN", "NVDA")

# Default values (Step 1 of the brief)
stock_symbol <- portfolio[1]
start_date   <- Sys.Date() - 365
end_date     <- Sys.Date()

# --- Data source 1: Yahoo Finance through quantmod ----------------------------
fetch_yahoo <- function(symbol, from, to) {
  getSymbols(symbol, src = "yahoo", from = from, to = to, auto.assign = FALSE)
}

# --- Data source 2: user-supplied CSV (Date, Open, High, Low, Close, Volume) --
read_csv_upload <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- tools::toTitleCase(tolower(trimws(names(df))))
  need <- c("Date", "Open", "High", "Low", "Close")
  miss <- setdiff(need, names(df))
  if (length(miss)) stop("CSV is missing column(s): ", paste(miss, collapse = ", "))
  if (!"Volume" %in% names(df)) df$Volume <- NA_real_
  df <- df[!is.na(as.Date(df$Date)), ]
  x <- xts(df[, c("Open", "High", "Low", "Close", "Volume")], order.by = as.Date(df$Date))
  colnames(x) <- paste0("CSV.", c("Open", "High", "Low", "Close", "Volume"))
  x
}

# --- Data source 3: offline demo data (used if the internet is unavailable) ---
simulate_ohlc <- function(symbol, from, to) {
  set.seed(sum(utf8ToInt(symbol)))
  d <- seq(as.Date(from), as.Date(to), by = "day")
  d <- d[!format(d, "%u") %in% c("6", "7")]
  n <- length(d)
  cl <- 150 * cumprod(1 + rnorm(n, 0.0005, 0.017))
  op <- c(150, head(cl, -1)) * (1 + rnorm(n, 0, 0.004))
  hi <- pmax(op, cl) * (1 + abs(rnorm(n, 0, 0.007)))
  lo <- pmin(op, cl) * (1 - abs(rnorm(n, 0, 0.007)))
  x <- xts(cbind(op, hi, lo, cl, round(rlnorm(n, log(4e7), 0.35))), order.by = d)
  colnames(x) <- paste0("DEMO.", c("Open", "High", "Low", "Close", "Volume"))
  x
}

# Convert xts to a plain data frame and apply the chosen time frame
to_frame <- function(x, time_frame = "Daily") {
  x <- x[, 1:5]
  colnames(x) <- c("Open", "High", "Low", "Close", "Volume")
  x <- x[!is.na(x$Close), ]
  if (time_frame != "Daily") {
    on   <- if (time_frame == "Weekly") "weeks" else "months"
    last <- index(x)[endpoints(x, on)]                     # last trading day of each period
    x    <- to.period(x, period = on, OHLC = TRUE, name = NULL)
    index(x) <- last                                       # date bars by their last trading day
  }
  colnames(x) <- c("Open", "High", "Low", "Close", "Volume")[seq_len(ncol(x))]
  data.frame(Date = as.Date(index(x)), coredata(x), row.names = NULL)
}

# -----------------------------------------------------------------------------
# Indicator and trading-rule helpers (built on the Assignment 5 functions)
# -----------------------------------------------------------------------------
pad_front <- function(v, n) c(rep(NA_real_, n - length(v)), v)
safe_sma  <- function(x, p) if (length(x) >= p) pad_front(sma(x, p), length(x)) else rep(NA_real_, length(x))

rolling_stdev <- function(x, p) {
  out <- rep(NA_real_, length(x))
  if (length(x) >= p) for (i in p:length(x)) out[i] <- stdev(x[(i - p + 1):i])
  out
}

add_indicators <- function(df, p) {
  cl <- df$Close
  df$MA_short <- safe_sma(cl, p$ma_short)
  df$MA_long  <- safe_sma(cl, p$ma_long)
  df$EMA      <- ema(cl, p$ema_period)
  sd_bb       <- rolling_stdev(cl, p$bb_period)
  df$BB_mid   <- safe_sma(cl, p$bb_period)
  df$BB_up    <- df$BB_mid + p$bb_k * sd_bb
  df$BB_low   <- df$BB_mid - p$bb_k * sd_bb
  df$RSI      <- suppressWarnings(rsi(cl, p$rsi_period))
  m <- macd(cl, p$macd_fast, p$macd_slow, p$macd_signal)
  df$MACD <- m$macd_line; df$MACD_signal <- m$signal_line; df$MACD_hist <- m$histogram
  s <- suppressWarnings(stoch_rsi(cl, p$rsi_period, 3, 3))
  df$StochK <- s$k_line; df$StochD <- s$d_line
  df
}

# STEP 4: trading rules. Each returns a character vector of Buy / Sell / Hold.
generate_signals <- function(df, rule, p) {
  n <- nrow(df)
  buy <- sell <- rep(FALSE, n)
  if (rule %in% c("MA Crossover", "Combined (MA + RSI filter)")) {
    buy  <- crossover(df$MA_short, df$MA_long)
    sell <- crossunder(df$MA_short, df$MA_long)
    if (rule == "Combined (MA + RSI filter)") {
      # only buy if not overbought, only sell if not oversold
      buy  <- buy  & !is.na(df$RSI) & df$RSI < p$rsi_high
      sell <- sell & !is.na(df$RSI) & df$RSI > p$rsi_low
    }
  } else if (rule == "RSI Reversal") {
    # RSI climbs back above the oversold line -> Buy; drops below overbought -> Sell
    buy  <- crossover(df$RSI, rep(p$rsi_low, n))
    sell <- crossunder(df$RSI, rep(p$rsi_high, n))
  } else if (rule == "MACD Crossover") {
    buy  <- crossover(df$MACD, df$MACD_signal)
    sell <- crossunder(df$MACD, df$MACD_signal)
  }
  ifelse(buy, "Buy", ifelse(sell, "Sell", "Hold"))
}

# Simple long-only back-test: enter on Buy at the close, exit on Sell
backtest <- function(df) {
  pos <- 0; entry <- NA; trades <- list()
  for (i in seq_len(nrow(df))) {
    if (df$Signal[i] == "Buy" && pos == 0) { pos <- 1; entry <- i }
    else if (df$Signal[i] == "Sell" && pos == 1) {
      trades[[length(trades) + 1]] <- data.frame(
        Entry = df$Date[entry], Entry_Price = df$Close[entry],
        Exit = df$Date[i], Exit_Price = df$Close[i])
      pos <- 0
    }
  }
  if (pos == 1) trades[[length(trades) + 1]] <- data.frame(
    Entry = df$Date[entry], Entry_Price = df$Close[entry],
    Exit = df$Date[nrow(df)], Exit_Price = df$Close[nrow(df)])
  if (!length(trades)) return(NULL)
  t <- do.call(rbind, trades)
  t$Return_pct <- round(100 * (t$Exit_Price / t$Entry_Price - 1), 2)
  t
}

# -----------------------------------------------------------------------------
# Chart builders (ggplot2)
# -----------------------------------------------------------------------------
col_up <- "#1a9850"; col_dn <- "#d73027"

price_chart <- function(df, chart_type, ind, show_signals, show_hold, show_regime, symbol) {
  rng <- range(c(df$Low, df$High), na.rm = TRUE)
  p <- ggplot(df, aes(x = Date))

  # background shading: short MA above long MA (bullish regime)
  if (show_regime && "Moving Averages" %in% ind) {
    reg <- df[!is.na(df$MA_short) & !is.na(df$MA_long), ]
    if (nrow(reg)) {
      # group consecutive bullish bars into continuous shaded blocks
      runs <- rle(reg$MA_short > reg$MA_long)
      ends <- cumsum(runs$lengths); starts <- ends - runs$lengths + 1
      blocks <- data.frame(xmin = reg$Date[starts], xmax = reg$Date[ends])[runs$values, ]
      if (nrow(blocks)) p <- p + geom_rect(data = blocks, inherit.aes = FALSE,
                         aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                         fill = col_up, alpha = 0.07)
    }
  }

  if ("Bollinger Bands" %in% ind)
    p <- p + geom_ribbon(aes(ymin = BB_low, ymax = BB_up), fill = "#9ecae1", alpha = 0.35, na.rm = TRUE)

  # STEP 2: chart types
  wid <- max(0.6, 0.7 * as.numeric(median(diff(df$Date))))
  if (chart_type == "Line") {
    p <- p + geom_line(aes(y = Close), colour = "grey20", linewidth = 0.6)
  } else if (chart_type == "Area") {
    p <- p + geom_area(aes(y = Close), fill = "#6baed6", alpha = 0.45) +
      geom_line(aes(y = Close), colour = "#2171b5", linewidth = 0.5)
  } else if (chart_type == "Candlestick") {
    df$Dir <- ifelse(df$Close >= df$Open, "Up", "Down")
    p <- p + geom_segment(data = df, aes(xend = Date, y = Low, yend = High, colour = Dir), linewidth = 0.35) +
      geom_rect(data = df, aes(xmin = Date - wid / 2, xmax = Date + wid / 2,
                               ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = Dir),
                colour = NA) +
      scale_fill_manual(values = c(Up = col_up, Down = col_dn), guide = "none") +
      scale_colour_manual(values = c(Up = col_up, Down = col_dn), guide = "none")
  } else if (chart_type == "OHLC Bars") {
    df$Dir <- ifelse(df$Close >= df$Open, "Up", "Down")
    p <- p + geom_segment(data = df, aes(xend = Date, y = Low, yend = High, colour = Dir)) +
      geom_segment(data = df, aes(x = Date - wid / 2, xend = Date, y = Open, yend = Open, colour = Dir)) +
      geom_segment(data = df, aes(x = Date, xend = Date + wid / 2, y = Close, yend = Close, colour = Dir)) +
      scale_colour_manual(values = c(Up = col_up, Down = col_dn), guide = "none")
  }

  # STEP 3: overlays on the price chart
  lines <- NULL
  if ("Moving Averages" %in% ind) lines <- rbind(lines,
    data.frame(Date = df$Date, Value = df$MA_short, Series = "MA short"),
    data.frame(Date = df$Date, Value = df$MA_long,  Series = "MA long"))
  if ("EMA" %in% ind) lines <- rbind(lines, data.frame(Date = df$Date, Value = df$EMA, Series = "EMA"))
  if ("Linear Regression" %in% ind && nrow(df) >= 20) {
    lr <- linreg(df$Close, min(nrow(df), 60), 0)
    lines <- rbind(lines, data.frame(Date = tail(df$Date, length(lr$predicted_values)),
                                     Value = lr$predicted_values, Series = "Regression"))
  }
  if (!is.null(lines)) {
    p <- p + ggnewscale_free_lines(lines)
  }

  # STEP 4: annotate bars with trading signals
  if (show_signals) {
    # separate layers for Buy and Sell so the candle colour/fill scales are untouched
    for (lab in c("Buy", "Sell")) {
      sig <- df[df$Signal == lab, ]
      if (!nrow(sig)) next
      is_buy <- lab == "Buy"
      sig$y  <- if (is_buy) sig$Low * 0.985 else sig$High * 1.015
      clr    <- if (is_buy) col_up else col_dn
      p <- p +
        geom_point(data = sig, aes(y = y), shape = if (is_buy) 24 else 25,
                   size = 3.4, fill = clr, colour = "black", stroke = 0.3) +
        geom_label(data = sig, aes(y = y, label = Signal), vjust = if (is_buy) 1.7 else -0.7,
                   size = 3, label.size = 0, fill = "white", alpha = 0.85,
                   colour = clr, fontface = "bold")
    }
    if (show_hold) {
      hold <- df[df$Signal == "Hold", ]
      p <- p + geom_text(data = hold, aes(y = Close, label = "H"), size = 2.2, colour = "grey55", vjust = -0.6)
    }
  }

  # text legend for the overlay lines (their colours are fixed per series)
  key <- c("MA short" = "MA short: blue", "MA long" = "MA long: orange",
           "EMA" = "EMA: purple", "Regression" = "Regression: red dashed")
  sub <- if (is.null(lines)) NULL else paste(key[unique(lines$Series)], collapse = "   |   ")
  if ("Bollinger Bands" %in% ind) sub <- paste(c(sub, "Bollinger Bands: shaded blue"), collapse = "   |   ")
  p + labs(title = paste(symbol, "price chart"), subtitle = sub, x = NULL, y = "Price") +
    scale_y_continuous(expand = expansion(mult = c(0.07, 0.07))) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top", panel.grid.minor = element_blank())
}

# Overlay lines need their own colour scale (candles already use colour);
# implemented with linetype + a manual colour per series drawn as separate layers.
ggnewscale_free_lines <- function(lines) {
  pal <- c("MA short" = "#1f78b4", "MA long" = "#ff7f00", "EMA" = "#6a3d9a", "Regression" = "#e31a1c")
  lapply(unique(lines$Series), function(s)
    geom_line(data = lines[lines$Series == s, ], aes(x = Date, y = Value),
              colour = pal[[s]], linewidth = 0.8, na.rm = TRUE,
              linetype = if (s == "Regression") "dashed" else "solid"))
}

volume_panel <- function(df) {
  df$Dir <- ifelse(df$Close >= df$Open, "Up", "Down")
  ggplot(df, aes(Date, Volume / 1e6, fill = Dir)) + geom_col(width = 0.8) +
    scale_fill_manual(values = c(Up = col_up, Down = col_dn), guide = "none") +
    labs(x = NULL, y = "Vol (M)") + theme_minimal(base_size = 11)
}

rsi_panel <- function(df, lo, hi) {
  ggplot(df, aes(Date, RSI)) +
    annotate("rect", xmin = min(df$Date), xmax = max(df$Date), ymin = lo, ymax = hi, fill = "#f0f0f0") +
    geom_hline(yintercept = c(lo, hi), linetype = "dashed", colour = "grey50") +
    geom_line(colour = "#6a3d9a", na.rm = TRUE) +
    scale_y_continuous(limits = c(0, 100), breaks = c(lo, 50, hi)) +
    labs(x = NULL, y = "RSI") + theme_minimal(base_size = 11)
}

stoch_panel <- function(df) {
  d <- rbind(data.frame(Date = df$Date, v = df$StochK, s = "%K"),
             data.frame(Date = df$Date, v = df$StochD, s = "%D"))
  ggplot(d, aes(Date, v, colour = s)) +
    geom_hline(yintercept = c(0.2, 0.8), linetype = "dashed", colour = "grey50") +
    geom_line(na.rm = TRUE) +
    scale_colour_manual(values = c("%K" = "#1f78b4", "%D" = "#ff7f00"), name = NULL) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = NULL, y = "StochRSI") + theme_minimal(base_size = 11) + theme(legend.position = "right")
}

macd_panel <- function(df) {
  ggplot(df, aes(Date)) +
    geom_col(aes(y = MACD_hist, fill = MACD_hist >= 0), width = 0.8, na.rm = TRUE) +
    geom_line(aes(y = MACD, colour = "MACD")) +
    geom_line(aes(y = MACD_signal, colour = "Signal")) +
    scale_fill_manual(values = c(`TRUE` = col_up, `FALSE` = col_dn), guide = "none") +
    scale_colour_manual(values = c(MACD = "#1f78b4", Signal = "#ff7f00"), name = NULL) +
    labs(x = NULL, y = "MACD") + theme_minimal(base_size = 11) + theme(legend.position = "right")
}

# text for the About tab
about_text <- function() {
  HTML("<p>Data: Yahoo Finance via <code>quantmod::getSymbols()</code>, a user CSV, or offline
        demo data if the internet is unavailable. Indicators are my own base-R implementations
        from Assignment 5. Signals: Buy when the fast series crosses over the slow series,
        Sell when it crosses under, otherwise Hold. Educational project, not investment advice.</p>")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a

# -----------------------------------------------------------------------------
# STEP 2: SHINY APP SKELETON (UI)
# -----------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#f7f8fa; }
    .well { background:#ffffff; border:1px solid #e3e6ea; }
    .kpi { background:#fff; border:1px solid #e3e6ea; border-radius:6px; padding:8px 12px; margin-bottom:10px; }
    .kpi h4 { margin:0; font-size:13px; color:#6b7280; } .kpi p { margin:0; font-size:20px; font-weight:600; }
  "))),
  titlePanel("Portfolio Technical Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(width = 3,
      h4("1. Data"),
      radioButtons("data_source", "Data source:",
                   c("Yahoo Finance", "Upload CSV", "Offline demo data"), inline = FALSE),
      conditionalPanel("input.data_source != 'Upload CSV'",
        selectizeInput("symbol", "Stock symbol (pick or type):", choices = portfolio,
                       selected = stock_symbol, options = list(create = TRUE))),
      conditionalPanel("input.data_source == 'Upload CSV'",
        fileInput("csv_file", "CSV with Date, Open, High, Low, Close, Volume", accept = ".csv")),
      dateRangeInput("date_range", "Select Date Range:", start = start_date, end = end_date,
                     max = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:", choices = c("Daily", "Weekly", "Monthly")),
      actionButton("load", "Load / refresh data", class = "btn-primary", width = "100%"),
      hr(),
      h4("2. Chart"),
      selectInput("chart_type", "Chart type:", c("Candlestick", "Line", "Area", "OHLC Bars")),
      checkboxGroupInput("technical_indicators", "Technical indicators (toggle on/off):",
                         choices = c("Moving Averages", "EMA", "Bollinger Bands", "Linear Regression",
                                     "Volume", "RSI", "Stochastic RSI", "MACD"),
                         selected = c("Moving Averages", "RSI", "MACD")),
      hr(),
      h4("3. Trading rule"),
      selectInput("rule", "Signal rule:",
                  c("MA Crossover", "Combined (MA + RSI filter)", "RSI Reversal", "MACD Crossover")),
      checkboxInput("show_signals", "Annotate Buy / Sell bars", TRUE),
      checkboxInput("show_hold", "Also mark Hold bars (H)", FALSE),
      checkboxInput("show_regime", "Shade bullish regime (short MA > long MA)", TRUE),
      hr(),
      h4("4. Parameters"),
      sliderInput("ma_short", "Short MA period", 5, 50, 20),
      sliderInput("ma_long", "Long MA period", 20, 200, 50),
      sliderInput("ema_period", "EMA period", 5, 100, 21),
      sliderInput("rsi_period", "RSI period", 5, 30, 14),
      sliderInput("rsi_band", "RSI oversold / overbought", 10, 90, c(30, 70)),
      fluidRow(column(4, numericInput("macd_fast", "MACD fast", 12, 2, 50)),
               column(4, numericInput("macd_slow", "slow", 26, 5, 100)),
               column(4, numericInput("macd_signal", "signal", 9, 2, 50))),
      fluidRow(column(6, numericInput("bb_period", "BB period", 20, 5, 100)),
               column(6, numericInput("bb_k", "BB width (sd)", 2, 0.5, 4, 0.5)))
    ),
    mainPanel(width = 9,
      uiOutput("status"),
      fluidRow(column(3, uiOutput("kpi_last")), column(3, uiOutput("kpi_change")),
               column(3, uiOutput("kpi_rsi")),  column(3, uiOutput("kpi_signal"))),
      tabsetPanel(
        tabPanel("Chart", plotOutput("stock_chart", height = "860px")),
        tabPanel("Signals & back-test",
                 br(), uiOutput("bt_summary"), h4("Signal bars"), DTOutput("signal_table"),
                 h4("Trades (long only, enter on Buy, exit on Sell)"), DTOutput("trade_table")),
        tabPanel("Data", br(), DTOutput("data_table")),
        tabPanel("About", br(), about_text())
      )
    )
  )
)

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- STEP 1: fetch data (with exception handling and fallback) -------------
  raw <- eventReactive(input$load, ignoreNULL = FALSE, {
    from <- as.Date(input$date_range[1]) - 400     # extra history for indicator warm-up
    to   <- as.Date(input$date_range[2])
    validate(need(from < to, "Start date must be before end date."))
    sym  <- toupper(trimws(input$symbol %||% stock_symbol))

    if (input$data_source == "Upload CSV") {
      validate(need(!is.null(input$csv_file), "Please upload a CSV file."))
      x <- tryCatch(read_csv_upload(input$csv_file$datapath),
                    error = function(e) validate(need(FALSE, paste("CSV error:", conditionMessage(e)))))
      return(list(x = x, symbol = sub("\\.csv$", "", input$csv_file$name), source = "Uploaded CSV"))
    }
    if (input$data_source == "Offline demo data")
      return(list(x = simulate_ohlc(sym, from, to), symbol = sym, source = "Offline demo data (simulated)"))

    withProgress(message = paste("Downloading", sym, "from Yahoo Finance"), {
      x <- tryCatch(fetch_yahoo(sym, from, to), error = function(e) e)
    })
    if (inherits(x, "error")) {
      showNotification(paste0("Yahoo Finance failed for ", sym, ": ", conditionMessage(x),
                              ". Showing offline demo data instead."), type = "warning", duration = 8)
      return(list(x = simulate_ohlc(sym, from, to), symbol = sym,
                  source = "Offline demo data (Yahoo unavailable)"))
    }
    list(x = x, symbol = sym, source = "Yahoo Finance")
  })

  params <- reactive({
    validate(need(input$ma_short < input$ma_long, "Short MA period must be less than long MA period."),
             need(input$macd_fast < input$macd_slow, "MACD fast period must be less than slow period."))
    list(ma_short = input$ma_short, ma_long = input$ma_long, ema_period = input$ema_period,
         rsi_period = input$rsi_period, rsi_low = input$rsi_band[1], rsi_high = input$rsi_band[2],
         macd_fast = input$macd_fast, macd_slow = input$macd_slow, macd_signal = input$macd_signal,
         bb_period = input$bb_period, bb_k = input$bb_k)
  })

  # ---- filtering, indicators and signals (time frame + date range) -----------
  filtered_data <- reactive({
    r <- raw(); p <- params()
    df <- to_frame(r$x, input$time_frame)
    validate(need(nrow(df) > 30, "Not enough data for the selected range / time frame."))
    df <- add_indicators(df, p)                       # computed on full history
    df$Signal <- generate_signals(df, input$rule, p)  # Step 4
    df <- df[df$Date >= as.Date(input$date_range[1]) & df$Date <= as.Date(input$date_range[2]), ]
    validate(need(nrow(df) > 2, "No data in the selected date range."))
    df
  })

  output$status <- renderUI({
    r <- raw()
    tags$p(style = "color:#6b7280;", sprintf("Source: %s  |  %s  |  %s bars shown (%s)",
           r$source, r$symbol, nrow(filtered_data()), input$time_frame))
  })

  kpi <- function(title, value) div(class = "kpi", h4(title), p(value))
  output$kpi_last   <- renderUI({ d <- filtered_data(); kpi("Last close", sprintf("%.2f", tail(d$Close, 1))) })
  output$kpi_change <- renderUI({ d <- filtered_data()
    kpi("Change over range", sprintf("%+.1f%%", 100 * (tail(d$Close, 1) / d$Close[1] - 1))) })
  output$kpi_rsi    <- renderUI({ d <- filtered_data(); kpi("RSI", sprintf("%.1f", tail(d$RSI, 1))) })
  output$kpi_signal <- renderUI({ d <- filtered_data(); s <- d[d$Signal != "Hold", ]
    kpi("Latest signal", if (nrow(s)) paste(tail(s$Signal, 1), format(tail(s$Date, 1), "%b %d")) else "None") })

  # ---- STEP 2 + 3 + 4: the chart ----------------------------------------------
  output$stock_chart <- renderPlot({
    df  <- filtered_data(); ind <- input$technical_indicators
    p <- price_chart(df, input$chart_type, ind, input$show_signals, input$show_hold,
                     input$show_regime, raw()$symbol)
    panels <- list(p); heights <- 3.2
    if ("Volume" %in% ind && any(!is.na(df$Volume))) { panels <- c(panels, list(volume_panel(df))); heights <- c(heights, 0.8) }
    if ("RSI" %in% ind)            { panels <- c(panels, list(rsi_panel(df, input$rsi_band[1], input$rsi_band[2]))); heights <- c(heights, 1) }
    if ("Stochastic RSI" %in% ind) { panels <- c(panels, list(stoch_panel(df))); heights <- c(heights, 1) }
    if ("MACD" %in% ind)           { panels <- c(panels, list(macd_panel(df)));  heights <- c(heights, 1.1) }
    lims <- range(df$Date)
    panels <- lapply(panels, function(g) g + coord_cartesian(xlim = lims) )
    if (input$chart_type == "Area") panels[[1]] <- panels[[1]] +
      coord_cartesian(xlim = lims, ylim = range(c(df$Low, df$High), na.rm = TRUE))
    wrap_plots(panels, ncol = 1, heights = heights)
  }, res = 96)

  # ---- signals table and back-test ---------------------------------------------
  output$signal_table <- renderDT({
    d <- filtered_data(); d <- d[d$Signal != "Hold", c("Date", "Close", "MA_short", "MA_long", "RSI", "MACD", "Signal")]
    datatable(transform(d, Close = round(Close, 2), MA_short = round(MA_short, 2), MA_long = round(MA_long, 2),
                        RSI = round(RSI, 1), MACD = round(MACD, 3)),
              rownames = FALSE, options = list(pageLength = 10, order = list(0, "desc")))
  })
  trades <- reactive(backtest(filtered_data()))
  output$trade_table <- renderDT({
    t <- trades(); validate(need(!is.null(t), "No completed Buy signal in this range."))
    datatable(transform(t, Entry_Price = round(Entry_Price, 2), Exit_Price = round(Exit_Price, 2)),
              rownames = FALSE, options = list(pageLength = 10))
  })
  output$bt_summary <- renderUI({
    d <- filtered_data(); t <- trades()
    bh <- 100 * (tail(d$Close, 1) / d$Close[1] - 1)
    st <- if (is.null(t)) 0 else 100 * (prod(1 + t$Return_pct / 100) - 1)
    wr <- if (is.null(t)) NA else 100 * mean(t$Return_pct > 0)
    tags$p(sprintf("Rule: %s  |  Trades: %d  |  Win rate: %s  |  Strategy return: %+.1f%%  |  Buy & hold: %+.1f%%",
                   input$rule, if (is.null(t)) 0L else nrow(t),
                   if (is.na(wr)) "n/a" else sprintf("%.0f%%", wr), st, bh))
  })

  output$data_table <- renderDT({
    d <- filtered_data()
    num <- vapply(d, is.numeric, logical(1)); d[num] <- lapply(d[num], round, 3)
    datatable(d, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, order = list(0, "desc")))
  })
}

shinyApp(ui, server)
