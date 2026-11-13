# Technical Analysis using R | Visualization Phase
Mahmoud Bakar | CDI College | BDA400 Assignment 6

Open app.R in RStudio and click Run App. Install dependencies first:
```r
install.packages(c("shiny", "ggplot2", "quantmod"))
shiny::runApp()
```

Run from this folder. The default source loads the bundled 2025 Yahoo Finance snapshots for AAPL, MSFT, GOOGL, AMZN and TSLA. For another date range, select Fetch from Yahoo Finance and press Load data. This uses quantmod and requests historical bars before the visible range to warm up indicators. You can also upload a matching local OHLCV CSV. Live requests require internet and provider availability.

Choose daily, weekly or monthly OHLCV bars and line, candlestick or area charts. Weekly and monthly bars use first open, highest high, lowest low, last close and summed volume. SMA lines overlay price; RSI and MACD use separate date panels to preserve their scales. Assignment 5 calculations are embedded in app.R. Short SMA crossings above or below long SMA produce Buy or Sell annotations; the table reports Hold otherwise. Insufficient history leaves indicators unavailable until enough bars are present. Signals are for coursework.

Run `source("run_tests.R")` from this folder. The checks cover OHLCV aggregation, indicator warm-up, signals, three chart types, server output on a synthetic CSV and server-rendered charts on historical Yahoo data. A live Yahoo request was not verified. The assistant adapted the code and ran the documented checks.
