# BDA400 Assignment 6 - Portfolio visualization dashboard

**Student:** Douha Suliman  
**Course:** BDA400  
**Stage:** Assignment 6 - Visualization Phase  
**Repository:** https://github.com/mahmuudbakkar-pixel/TechnicalAnalysis/tree/main/Assignment6

## Run

Use R 4.3 or newer and an internet connection. In the R console, run:

```r
install.packages(c("shiny", "ggplot2", "quantmod"))
shiny::runApp("Assignment6")
```

Or open `app.R` in RStudio and select **Run App**. The script contains the complete dashboard implementation. `quantmod` retrieves Yahoo Finance OHLCV bars; `shiny` supplies the controls and reactive plots; `ggplot2` renders them. The four indicator calculations in `app.R` carry forward the base R logic from Assignment 5.

Choose one of the five portfolio symbols (AAPL, MSFT, GOOGL, AMZN, TSLA), visible dates, daily/weekly/monthly bars, line/candlestick/area chart, SMA/RSI/MACD switches, and indicator periods. Click **Fetch Yahoo Finance data** after changing symbol or dates. The selected source and dates appear above the price chart. Changing display options updates charts without repeating the download.

The app downloads older observations before the visible start date to warm up the 50-bar moving average, including when monthly bars are selected. It aggregates OHLCV before calculating indicators and then crops to the visible range. Short/long SMA crossing upward creates a Buy label, crossing downward creates a Sell label, and all other bars show Hold in the table. RSI and MACD have separate synchronized date panels because their units differ from USD prices; each panel can be turned off independently. The signal labels appear on the price bars.

Yahoo access can fail due to a provider outage or network restriction. In that case, upload a genuine CSV using the optional local fallback and click **Fetch**. The CSV needs `Date,Open,High,Low,Close,Volume` columns; the app names it as a local source. No fabricated historical market observations are bundled.

## Offline verification

From inside `Assignment6`, run `Rscript run_tests.R`. This uses explicitly synthetic fixture values to check aggregation, indicator alignment, crossover events and plot construction. `test_results.txt` records the run. A live Yahoo call and browser controls still require network access when the evaluator runs the app.

## Submission

Upload this `Assignment6` folder to the public `TechnicalAnalysis` repository and submit the cover PDF `Suliman_Douha_CA_BDA400_A06.pdf` to the LMS. The cover PDF references this folder's URL. The GitHub folder link is only valid once the folder has been committed.
