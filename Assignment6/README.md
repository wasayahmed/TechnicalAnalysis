# Assignment 6: Portfolio Dashboard (R Shiny)

**Wasay Ahmed** | BDA400 Data Science Tools and Techniques | Technical Analysis using R, Visualization Phase

Interactive dashboard that fetches stock data from Yahoo Finance (quantmod), an uploaded CSV, or offline demo data, and:

- draws **candlestick, OHLC bar, line or area** charts for any date range and a **daily / weekly / monthly** time frame;
- overlays my Assignment 5 indicators, each toggled on or off: **moving averages, EMA, Bollinger Bands (stdev), linear regression, volume, RSI, Stochastic RSI, MACD**;
- applies a selectable trading rule (**MA crossover, MA + RSI filter, RSI reversal, MACD crossover**) with adjustable parameters;
- **annotates Buy / Sell bars** on the chart (optionally Hold bars), shades bullish regimes, and back-tests the rule (long only).

## Run

```r
install.packages(c("shiny", "ggplot2", "quantmod", "patchwork", "DT"))
shiny::runApp("Assignment6")    # from the repository root, or click "Run App" in RStudio
```

## Files

| File | Purpose |
|------|---------|
| `app.R` | Whole app, organised in sections matching Steps 1 to 4 of the brief |
| `indicators/` | Assignment 5 indicator functions (base R) |
| `portfolio.txt` | Symbols offered in the symbol picker |
| `screenshots/` | App screenshots (captured in offline demo mode) |

Repository: https://github.com/YOUR-GITHUB-USERNAME/TechnicalAnalysis

*Educational project. Not investment advice.*
