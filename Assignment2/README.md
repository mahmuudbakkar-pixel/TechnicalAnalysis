# Technical Analysis using R Preliminary Stage

**Mahmoud Bakar | BDA400 | Assignment 2 | 10 November 2026**

This project imports five Yahoo Finance daily OHLCV series, stores each as a separate data frame in a named list, calculates descriptive statistics on daily **Close**, and displays price, moving-average and volume charts. The bundled data cover the 2025 calendar year: 250 observations per stock, from 2 January to 31 December.

## Run

1. Install R from https://cran.r-project.org/ and RStudio Desktop from https://posit.co/download/rstudio-desktop/.
2. Open `TechnicalAnalysis.Rproj`, or set your working directory to this `Assignment2` folder.
3. Run `source("setup.R")`, then `source("display_analysis.R")`.
4. Inspect `summary_table`, `stocks$AAPL` (or another symbol), and the files in `output/`.
5. Run `source("verify.R")` for independent numerical checks.

The default run uses the included **real Yahoo historical snapshots**, so results can be reproduced without contacting the provider. To download again:

```r
Sys.setenv(TA_REFRESH = "true")
source("display_analysis.R")
```

Fresh mode calls `quantmod::getSymbols()` for each symbol. If that fails, an explicitly logged Yahoo query1 endpoint is attempted. Both paths use real market observations; no synthetic fallback is substituted. Package installation and fresh downloads need internet access. An unavailable provider raises an error instead of inventing data.

## Files

- `MahmoudBakar_BDA400_A02.docx`: report and cover page with repository link.
- `portfolio.txt`: AAPL, MSFT, GOOGL, AMZN, TSLA, one per line.
- `setup.R`: package installation and environment record.
- `stock_functions.R`: input validation, loading and statistical functions.
- `display_analysis.R`: complete run, tables and five price/volume charts.
- `verify.R`: independent formula checks and edge cases.
- `data/`: unrounded historical snapshots and source notes.
- `output/`: full-precision statistics, moving averages, charts and logs.
- `evidence/`: actual screenshots of the assistant-operated Linux RStudio environment.

## Statistical choices

All statistics use unadjusted daily Close in USD. Adjusted Close is preserved separately. The arithmetic mean, median and sample standard deviation use the complete annual series. Standard deviation uses the n-1 denominator and describes price levels, not return volatility. A 20-observation trailing simple moving average is aligned to trading dates, with 19 leading NA values. Short series return unavailable SMA values rather than shortened alignment.

The mode returns **all repeated prices tied for highest frequency**, using the input precision. If all prices are unique, it says `No repeated value`. R's built-in `mode()` is not a statistical mode. Rounding is only for report presentation.

## Evidence and attribution

Prepared with ChatGPT (GPT-6 Codex), 10 November 2026: implementation, documentation, execution, checks and packaging. Calculations were executed in R and independently checked. The report distinguishes the assistant-operated Linux environment from installation on the student's own Windows PC. No Windows installer screenshots or student actions are claimed. Repository account already existed and was reused.

Repository: https://github.com/mahmuudbakkar-pixel/TechnicalAnalysis/tree/main/Assignment2
