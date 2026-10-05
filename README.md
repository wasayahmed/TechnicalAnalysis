# TechnicalAnalysis

Stock technical analysis project in R for **BDA400 Data Science Tools and Techniques** (CDI College), by **Wasay Ahmed**.
The project is built in three stages, one folder per assignment.

| Folder | Assignment | Contents |
|--------|------------|----------|
| [`Assignment2/`](Assignment2) | A2: Preliminary stage | Setup, `portfolio.txt`, data import with quantmod, basic statistics, display functions |
| [`Assignment5/`](Assignment5) | A5: Development phase | SMA, EMA, MACD, stdev, linear regression, RSI, StochRSI, crossover, crossunder in base R, tests, filters and trading rules |
| [`Assignment6/`](Assignment6) | A6: Visualization phase | R Shiny portfolio dashboard with indicator overlays and annotated trading signals |

## Quick start

```r
# Assignment 2
setwd("Assignment2"); source("install_packages.R"); source("main_analysis.R")

# Assignment 5
setwd("../Assignment5"); source("test_indicators.R"); source("run_portfolio_analysis.R")

# Assignment 6
shiny::runApp("../Assignment6")
```

Portfolio: AAPL, MSFT, GOOGL, AMZN, NVDA (edit `Assignment2/portfolio.txt`). Data: Yahoo Finance via `quantmod::getSymbols()`.
If Yahoo Finance cannot be reached, the code falls back to clearly labelled simulated data so it can still be demonstrated.

*Educational project. Not investment advice.*
