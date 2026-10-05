# AI Appendix: Assignment 5 (Technical Analysis, Development Phase)

**AI Assistance Declaration:** I used Claude (Anthropic, Opus 5.5, via Claude Cowork, 2026-10-04) for reviewing my translation of the provided pseudocode into R and suggesting edge-case tests. Prompts used: listed below. I verified outputs using hand calculations of the brief's examples and `test_indicators.R`, which compares every function with base R, `lm()` or the TTR package (36 checks, all passing). All final calculations are done by myself. I am responsible for the accuracy and originality of this work.

| Tool | Model | Interface | Date |
|------|-------|-----------|------|
| Claude (Anthropic) | Opus 5.5 | Claude Cowork | 2026-10-04 |

## Prompts and key responses

1. **"Review this R implementation of rsi() against this pseudocode and list any logic errors."**
   Key response: the pseudocode's else-branch leaves `gains[i]` empty on down days (mean becomes NA), and smoothing at `period + 1` counts the `period`-th change twice. Suggested seeding with the simple average and starting Wilder smoothing one bar later.
   My check: confirmed by hand on the brief's example (RSI at position 6 = 60.00) and against `TTR::RSI()`.

2. **"Review this linreg() against the pseudocode. How many points does the window contain when regressionOffset = 0?"**
   Key response: `start = n - L + offset`, `end = n - offset` gives L + 1 points at offset 0.
   My check: changed to `start = n - L + 1 - offset`; verified slope and intercept against `lm()`.

3. **"The StochRSI description says lowest/highest RSI over a period, but the pseudocode uses min/max of all RSI values. Which is standard?"**
   Key response: a rolling look-back window is the standard definition.
   My check: rebuilt the same formula with `TTR::runMin/runMax` and compared (exact match).

4. **"Suggest edge cases to test SMA, EMA, crossover and crossunder."**
   Key response: data shorter than period, period = 1, constant series, NA warm-up values in crossover inputs, unequal lengths.
   My check: all included in `test_indicators.R`.

5. **"How can I verify my EMA when TTR seeds it differently?"**
   Key response: compute the same recursion independently with `stats::filter(alpha * x, 1 - alpha, method = "recursive", init = x[1])`.
