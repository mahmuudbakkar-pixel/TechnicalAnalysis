# BDA400 Assignment 5 - Technical Analysis using R

**Student:** Douha Suliman  
**Course:** BDA400  
**Assignment:** A05, Development Phase

Nine independent, base R indicator scripts use exactly the function names and argument names in the handout. There are no third-party packages in these implementations.

## Run and check

From this directory:

```sh
Rscript run_tests.R
```

The tests cover expected values, edge cases, argument errors, and all nine functions. The saved `test_results.txt` records the run.

To load individual functions in RStudio, source `sma.R`, `ema.R`, `rsi.R` first, followed by the other files. `macd()` calls `ema()` and `stoch_rsi()` calls `rsi()`.

## Apply to the earlier portfolio

The symbols in `portfolio.txt` match Assignment 2: AAPL, MSFT, GOOGL, AMZN, TSLA. Put actual historical OHLCV CSV exports in `market_data/` named `AAPL.csv`, etc. Each file needs `Date,Open,High,Low,Close,Volume`. Then run:

```sh
Rscript analyze_portfolio.R
```

The application builds one data frame per symbol, filters the latest 365 calendar days, adds indicators and illustrative Buy/Sell/Hold rules, and writes outputs to `output/`. **Actual source CSVs were not provided with Assignment 5, so no market observations or real-stock results are included in this package.**

## Conventions

- `sma()` returns one value for each complete window; `ema()` retains input length and uses the first price as its seed.
- `stdev()` divides by `n` as the handout's population formula specifies.
- `linreg()` uses an exact window ending `regressionOffset` observations before the latest; its pseudocode start index is off by one, so the function corrects that boundary.
- `rsi()` uses Wilder smoothing and has leading `NA`s. `stoch_rsi()` uses a rolling RSI range with aligned `%K` and `%D` values; a flat range is assigned 0.5.
- The handout mixes Boolean descriptions and string pseudocode for crossings. `crossover()` returns `None`, `Up`, `Down`; `crossunder()` returns `None`, `True`, `False` strings to match the pseudocode.

The cover sheet's repository URL is the expected URL from Assignment 2 and must be checked after the Assignment 5 folder is uploaded.
