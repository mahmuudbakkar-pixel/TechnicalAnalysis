# Optional application to CSV exports of real historical stock data.
# Base R only. Save SYMBOL.csv files in market_data/ with
# Date, Open, High, Low, Close, Volume columns in ascending or any date order.
for (file in c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
               "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R")) source(file)

portfolio <- trimws(readLines("portfolio.txt"))
portfolio <- portfolio[nzchar(portfolio)]
stock_frames <- list()
results <- list()
dir.create("output", showWarnings = FALSE)

for (symbol in portfolio) {
  path <- file.path("market_data", paste0(symbol, ".csv"))
  if (!file.exists(path)) {
    message("No source CSV for ", symbol, "; add ", path, " to run real-data analysis.")
    next
  }
  stock <- read.csv(path, stringsAsFactors = FALSE)
  expected <- c("Date", "Open", "High", "Low", "Close", "Volume")
  if (!all(expected %in% names(stock))) stop(path, " must contain: ", paste(expected, collapse = ", "))
  stock <- stock[order(as.Date(stock$Date)), expected]
  stock$Date <- as.Date(stock$Date)
  if (anyNA(stock) || any(stock$High < stock$Low) || any(stock$Volume < 0)) stop(path, " has missing or invalid OHLCV data")
  # Example time-period filter: keep the most recent 365 calendar days.
  stock <- stock[stock$Date >= max(stock$Date) - 365, ]
  if (nrow(stock) < 30L) { message(symbol, ": fewer than 30 recent observations; skipped."); next }
  stock_frames[[symbol]] <- stock
  n <- nrow(stock); close <- stock$Close
  avg20 <- c(rep(NA_real_, 19), sma(close, 20))
  fast <- ema(close, 12); slow <- ema(close, 26)
  strength <- rsi(close, 14)
  crosses <- crossover(fast, slow)
  under <- crossunder(fast, slow)
  # Illustrative rules only: signals are not investment recommendations.
  signal <- rep("Hold", n)
  signal[crosses == "Up" & !is.na(strength) & strength < 70] <- "Buy"
  signal[under == "True"] <- "Sell"
  stock$SMA20 <- avg20
  stock$EMA12 <- fast
  stock$EMA26 <- slow
  stock$RSI14 <- strength
  stock$Signal <- signal
  write.csv(stock, file.path("output", paste0(symbol, "_indicators.csv")), row.names = FALSE)
  latest <- stock[n, ]
  results[[symbol]] <- data.frame(Symbol = symbol, LatestDate = latest$Date,
                                  Close = latest$Close, SMA20 = latest$SMA20,
                                  RSI14 = latest$RSI14, Signal = latest$Signal,
                                  AboveSMA20 = latest$Close > latest$SMA20)
}
if (length(results) > 0L) {
  summary <- do.call(rbind, results)
  write.csv(summary, "output/portfolio_summary.csv", row.names = FALSE)
  print(summary)
  cat("Filtered: close above SMA20 and RSI between 50 and 70\n")
  print(subset(summary, AboveSMA20 & RSI14 >= 50 & RSI14 <= 70))
} else {
  cat("No market dataset was provided. Indicator functions and tests are ready; no real-stock results were invented.\n")
}
