# Mahmoud Bakar | BDA400 Assignment 2
# AI-assisted implementation; calculations are performed by R.

read_portfolio <- function(path = "portfolio.txt") {
  if (!file.exists(path)) stop("Portfolio file not found: ", path)
  symbols <- toupper(trimws(readLines(path, warn = FALSE)))
  symbols <- unique(symbols[nzchar(symbols) & !startsWith(symbols, "#")])
  if (!length(symbols)) stop("Portfolio contains no symbols.")
  if (any(!grepl("^[A-Z0-9.^=-]+$", symbols))) stop("Invalid symbol in portfolio.")
  symbols
}

validate_stock <- function(d) {
  cols <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  if (!is.data.frame(d) || !all(cols %in% names(d))) stop("Missing OHLCV columns.")
  d <- d[, cols]
  d$Date <- as.Date(d$Date)
  if (!nrow(d) || anyNA(d$Date) || anyDuplicated(d$Date)) stop("Empty or invalid dates.")
  if (!all(vapply(d[-1], is.numeric, logical(1)))) stop("Prices and volume must be numeric.")
  d <- d[order(d$Date), ]
  if (any(!is.finite(as.matrix(d[-1])))) stop("Incomplete OHLCV observations; inspect source.")
  if (any(d$Close <= 0 | d$Adjusted <= 0 | d$Volume < 0)) stop("Invalid price or volume.")
  if (any(d$High < pmax(d$Open, d$Close, d$Low) |
          d$Low > pmin(d$Open, d$Close, d$High))) stop("Inconsistent OHLC range.")
  rownames(d) <- NULL
  d
}

# Yahoo query1 fallback is explicit and never substitutes invented prices.
yahoo_query1 <- function(symbol, from, to) {
  epoch <- function(d) as.numeric(as.POSIXct(as.Date(d), tz = "UTC"))
  url <- sprintf(paste0("https://query1.finance.yahoo.com/v8/finance/chart/%s",
                        "?period1=%.0f&period2=%.0f&interval=1d"),
                 utils::URLencode(symbol, reserved = TRUE), epoch(from), epoch(to))
  h <- curl::new_handle(useragent = "Mozilla/5.0", timeout = 45)
  response <- curl::curl_fetch_memory(url, handle = h)
  if (response$status_code != 200L) stop("Yahoo query1 HTTP ", response$status_code)
  result <- jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE)$chart$result
  if (!length(result)) stop("Yahoo returned no series.")
  z <- result[[1]]; q <- z$indicators$quote[[1]]
  nums <- function(v) vapply(v, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1))
  data.frame(Date = as.Date(as.POSIXct(nums(z$timestamp), origin = "1970-01-01", tz = "UTC")),
             Open = nums(q$open), High = nums(q$high), Low = nums(q$low),
             Close = nums(q$close), Volume = nums(q$volume),
             Adjusted = nums(z$indicators$adjclose[[1]]$adjclose))
}

load_stock_data <- function(portfolio_file = "portfolio.txt", from = "2025-01-01",
                            to = "2026-01-01", use_cache = TRUE, cache_dir = "data") {
  if (as.Date(from) >= as.Date(to)) stop("from must precede to; to is exclusive.")
  symbols <- read_portfolio(portfolio_file)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  stocks <- setNames(vector("list", length(symbols)), symbols)
  provenance <- vector("list", length(symbols))
  for (i in seq_along(symbols)) {
    symbol <- symbols[i]
    cache <- file.path(cache_dir, paste0(symbol, ".csv"))
    if (use_cache && file.exists(cache)) {
      d <- read.csv(cache, stringsAsFactors = FALSE)
      source_name <- "Bundled Yahoo historical snapshot"
    } else {
      message("Downloading ", symbol, " using quantmod")
      d <- tryCatch({
        x <- quantmod::getSymbols(symbol, src = "yahoo", from = from, to = to,
                                  auto.assign = FALSE, warnings = FALSE)
        data.frame(Date = as.Date(zoo::index(x)), Open = as.numeric(quantmod::Op(x)),
                   High = as.numeric(quantmod::Hi(x)), Low = as.numeric(quantmod::Lo(x)),
                   Close = as.numeric(quantmod::Cl(x)), Volume = as.numeric(quantmod::Vo(x)),
                   Adjusted = as.numeric(quantmod::Ad(x)))
      }, error = function(e) {
        message("quantmod provider call failed: ", conditionMessage(e),
                "\nTrying Yahoo query1 chart endpoint.")
        NULL
      })
      source_name <- "Yahoo Finance via quantmod"
      if (is.null(d)) {
        d <- yahoo_query1(symbol, from, to)
        source_name <- "Yahoo Finance query1 fallback"
      }
      d <- validate_stock(d)
      write.csv(d, cache, row.names = FALSE)
    }
    d <- validate_stock(d)
    d <- d[d$Date >= as.Date(from) & d$Date < as.Date(to), ]
    if (!nrow(d)) stop("No observations in selected period for ", symbol)
    stocks[[symbol]] <- d
    provenance[[i]] <- data.frame(Symbol = symbol, Source = source_name,
                                  FirstDate = min(d$Date), LastDate = max(d$Date), Rows = nrow(d))
  }
  attr(stocks, "provenance") <- do.call(rbind, provenance)
  stocks
}

# Statistical mode, NOT base R mode(), which reports an object's storage type.
statistical_mode <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(numeric(0))
  values <- sort(unique(x)); counts <- tabulate(match(x, values))
  if (max(counts) == 1L) return(numeric(0)) # No repeated value
  values[counts == max(counts)]             # Preserve all ties
}

calculate_statistics <- function(stock_data, symbol = "Stock", ma_period = 20L) {
  d <- validate_stock(stock_data)
  if (length(ma_period) != 1L || !is.finite(ma_period) || ma_period < 1 ||
      ma_period != floor(ma_period)) stop("ma_period must be a positive integer.")
  # Use unadjusted daily Close consistently; input prices are not rounded.
  x <- d$Close
  modes <- statistical_mode(x)
  sma <- if (length(x) >= ma_period) as.numeric(TTR::SMA(x, n = ma_period)) else rep(NA_real_, length(x))
  summary <- data.frame(Symbol = symbol, N = length(x), Mean = mean(x),
                        Mode = if (length(modes)) paste(format(modes, digits = 12), collapse = "; ") else "No repeated value",
                        Median = median(x), SD = if (length(x) > 1L) sd(x) else NA_real_,
                        SMA_Period = ma_period, Latest_SMA = tail(sma, 1))
  list(summary = summary, series = data.frame(Date = d$Date, Close = x, SMA = sma))
}
