# Mahmoud Bakar | BDA400 Assignment 2
# Run from Assignment2: source("display_analysis.R")
# Bundled snapshot by default. Set TA_REFRESH=true to request fresh Yahoo retrieval.
source("stock_functions.R")
source("setup.R")
refresh <- identical(tolower(Sys.getenv("TA_REFRESH", "false")), "true")
stocks <- load_stock_data(use_cache = !refresh)
statistics <- Map(function(d, s) calculate_statistics(d, s, 20L), stocks, names(stocks))
summary_table <- do.call(rbind, lapply(statistics, `[[`, "summary"))
rownames(summary_table) <- NULL
write.csv(summary_table, "output/statistics.csv", row.names = FALSE)
write.csv(attr(stocks, "provenance"), "output/run_sources.csv", row.names = FALSE)
print(summary_table, row.names = FALSE, digits = 6)

display_stock_data <- function(stocks, n = 6L) {
  for (s in names(stocks)) {
    cat("\n", s, "- first", n, "observations\n")
    print(head(stocks[[s]], n), row.names = FALSE)
    cat("Last observation:\n"); print(tail(stocks[[s]], 1), row.names = FALSE)
  }
}
plot_stock <- function(d, result, symbol, file) {
  png(file, width = 1500, height = 1000, res = 150)
  on.exit(dev.off())
  par(mfrow = c(2, 1), mar = c(3, 4, 3, 1))
  plot(d$Date, d$Close, type = "l", col = "#174A75", lwd = 1.5,
       main = paste(symbol, "daily Close and 20 trading day SMA"), xlab = "", ylab = "USD")
  lines(result$series$Date, result$series$SMA, col = "#C55A11", lwd = 2)
  legend("topleft", c("Close", "SMA 20"), col = c("#174A75", "#C55A11"), lty = 1, bty = "n")
  plot(d$Date, d$Volume / 1e6, type = "h", col = "#467B87",
       main = "Daily trading volume", xlab = "Date", ylab = "Millions of shares")
}
display_stock_data(stocks)
for (s in names(stocks)) {
  write.csv(statistics[[s]]$series, file.path("output", paste0(s, "_moving_average.csv")), row.names = FALSE)
  plot_stock(stocks[[s]], statistics[[s]], s, file.path("output", paste0(s, "_chart.png")))
}
writeLines(capture.output(display_stock_data(stocks)), "output/data_preview.txt")
cat("\nSUCCESS: five data frames, statistics table and five price/volume charts saved.\n")
