# BDA400 Assignment 5 — Technical Analysis using R

Mahmoud Bakar | CDI College | Development Phase

Open this folder in RStudio and set it as the working directory. Run:
```r
source("run_tests.R")
source("analyze_portfolio.R")
```
Or run each script with Rscript from this directory. No packages or internet are required.
The script reads the five Assignment 2 Yahoo Finance snapshots in market_data. The reference period is 2025; these are historical snapshots. The script writes one daily indicator CSV per symbol, a latest-date summary and a screen of stocks above SMA20 with RSI in [50, 70]. It uses daily Close and a 365-calendar-day filter relative to each file's last date. Buy is an upward EMA12/26 crossing with RSI below 70; Sell is a downward crossing. Signals are classroom examples.

## Indicator conventions
- SMA returns complete trailing windows. The analysis pads the leading dates with NA.
- EMA starts at the first price. MACD returns the fast-minus-slow EMA, its signal EMA, and their difference.
- Standard deviation divides by n (population).
- Regression uses the handout's inclusive start/end indices, then fits local positions by least squares. At offset zero, regressionLength=20 spans 21 points where available; the analysis labels this parameter as 20.
- RSI seeds Wilder averages from the first period price changes and emits the seed at period+1. Smoothing starts with the next change so the seed observation is counted once. A flat sequence scores 50.
- Stochastic RSI follows the handout's full-series normalization and two SMA smoothers, with leading NA for alignment. A zero range maps to 0.5. The full-series range uses future observations relative to earlier rows, so neither stochastic line is used in the trading rules.
- Crossover returns None/Up/Down. Crossunder returns None initially and True/False afterward, per the pseudocode. Missing comparisons do not fire signals.

ChatGPT (Codex) assisted with code adaptation, testing and documentation. Test and analysis logs record the assistant's R run.
