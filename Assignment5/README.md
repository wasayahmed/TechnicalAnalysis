# Assignment 5: Technical Analysis using R, Development Phase

Base-R implementations of nine technical indicators (no TTR or quantmod inside the functions), following the templates and names in the brief.

| File | Function |
|------|----------|
| `sma.R` | `sma(data, period)` |
| `ema.R` | `ema(data, period)` |
| `macd.R` | `macd(data, short_period, long_period, signal_period)` |
| `stdev.R` | `stdev(data)` |
| `linreg.R` | `linreg(regressionSource, regressionLength, regressionOffset)` |
| `rsi.R` | `rsi(data, period)` |
| `stoch_rsi.R` | `stoch_rsi(data, period, k_period, d_period)` |
| `crossover.R` | `crossover(arr1, arr2)` |
| `crossunder.R` | `crossunder(arr1, arr2)` |

## Run

```r
# working directory = Assignment5
source("test_indicators.R")         # brief examples + 36 checks -> "36 passed, 0 failed"
source("run_portfolio_analysis.R")  # filters, screens and Buy/Sell/Hold rules on the A2 portfolio
```

`run_portfolio_analysis.R` reads `../Assignment2/portfolio.txt` and reuses `load_stock_data()` from Assignment 2.
Saved outputs: `test_results.txt`, `portfolio_analysis_output.txt`, `trading_signals.csv` (the saved sample run used the offline fallback data).
Implementation notes and deliberate corrections to the pseudocode are explained in `WasayAhmed_BDA400_A05_Explanation.docx`.
