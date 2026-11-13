# Offline checks for the dashboard logic. No market observations are fabricated.
source("app.R", local = TRUE)
dates <- seq(as.Date("2025-01-01"), by = "day", length.out = 180)
close <- 100 + seq_along(dates) * 0.15 + 3 * sin(seq_along(dates) / 7)
demo <- data.frame(Date = dates, Open = close - 0.4, High = close + 1,
                   Low = close - 1, Close = close, Volume = rep(1000, length(dates)))
stopifnot(nrow(clean_ohlcv(demo)) == 180L)
week <- to_timeframe(demo, "Weekly")
month <- to_timeframe(demo, "Monthly")
stopifnot(nrow(week) < nrow(demo), nrow(month) == 6L,
          month$Open[1] == demo$Open[1], month$Close[1] == demo$Close[31],
          month$Volume[1] == 31000)
analyzed <- add_indicators(demo, 5L, 20L, 14L)
stopifnot(all(analyzed$Signal %in% c("Buy", "Sell", "Hold")),
          all(is.na(analyzed$LongSMA[1:19])), is.finite(analyzed$LongSMA[20]),
          is.na(analyzed$RSI[14]), is.finite(analyzed$RSI[15]),
          any(analyzed$Signal == "Buy"), any(analyzed$Signal == "Sell"))
# A crossover is an event, so adjacent bars after a crossover remain Hold.
for (i in which(analyzed$Signal == "Buy")) {
  stopifnot(analyzed$ShortSMA[i] > analyzed$LongSMA[i],
            analyzed$ShortSMA[i - 1] <= analyzed$LongSMA[i - 1])
}
for (type in c("Line", "Candlestick", "Area")) {
  p <- price_plot(analyzed, "TEST", type, c("SMA", "RSI", "MACD"), "Daily")
  stopifnot(inherits(ggplot2::ggplot_build(p), "ggplot_built"))
}
stopifnot(inherits(ggplot2::ggplot_build(price_plot(analyzed, "TEST", "Line", character(0), "Daily")),
                   "ggplot_built"))
stopifnot(inherits(ggplot2::ggplot_build(rsi_plot(analyzed)), "ggplot_built"),
          inherits(ggplot2::ggplot_build(macd_plot(analyzed)), "ggplot_built"))
fixture <- tempfile(fileext = ".csv")
utils::write.csv(demo, fixture, row.names = FALSE)
shiny::testServer(server, {
  session$setInputs(symbol = "AAPL", date_range = as.Date(c("2025-02-01", "2025-06-01")),
                    time_frame = "Daily", chart_type = "Line", indicators = c("SMA", "RSI", "MACD"),
                    short_ma = 5, long_ma = 20, rsi_period = 14,
                    csv = data.frame(name = "synthetic_fixture.csv", datapath = fixture), fetch = 1)
  stopifnot(grepl("Local CSV: synthetic_fixture.csv", output$status, fixed = TRUE),
            grepl("AAPL", output$status, fixed = TRUE))
})
# Verify the default data path and rendering with actual historical data.
shiny::testServer(server, {
  session$setInputs(symbol = "AAPL", date_range = as.Date(c("2025-01-01", "2025-12-31")),
                    time_frame = "Daily", chart_type = "Candlestick",
                    indicators = c("SMA", "RSI", "MACD"),
                    short_ma = 20, long_ma = 50, rsi_period = 14,
                    source_mode = "snapshot", fetch = 1)
  stopifnot(grepl("250 bars", output$status, fixed = TRUE),
            length(output$price_chart$src) == 1L)
  session$setInputs(time_frame = "Monthly", chart_type = "Area")
  stopifnot(grepl("12 bars", output$status, fixed = TRUE),
            grepl("Insufficient bars", output$status, fixed = TRUE),
            length(output$price_chart$src) == 1L)
})
cat("PASS: aggregation, indicators, signals, three chart types, all panels, CSV and saved Yahoo data.\n")
cat("Shiny server rendered daily candlesticks and monthly area charts.\n")
cat(R.version.string, "\n")
