# BDA400 Assignment 6 - Portfolio visualization dashboard
# Student: Mahmoud Bakar
# Run with shiny::runApp("Assignment6_MahmoudBakar") after installing shiny, ggplot2, quantmod.

required_packages <- c("shiny", "ggplot2", "quantmod")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install required packages first: install.packages(c(",
       paste(sprintf('"%s"', missing_packages), collapse = ", "), "))")
}

# Assignment 5 implementations, used again here instead of substituting TTR.
# Simple moving average; returns one result for each complete window.
sma <- function(data, period) {
  if (!is.numeric(data) || anyNA(data) || any(!is.finite(data))) stop("data must be finite numeric values")
  if (length(period) != 1L || is.na(period) || period < 1 || period != as.integer(period)) stop("period must be a positive integer")
  n <- length(data)
  if (n < period) stop("Data length should be greater than or equal to the period")
  sma_values <- numeric(n - period + 1L)
  for (i in seq_along(sma_values)) {
    sma_values[i] <- sum(data[i:(i + period - 1L)]) / period
  }
  return(sma_values)
}

# Exponential moving average, seeded with the first observation as specified.
ema <- function(data, period) {
  if (!is.numeric(data) || length(data) == 0L || anyNA(data) || any(!is.finite(data))) stop("data must be nonempty finite numeric values")
  if (length(period) != 1L || is.na(period) || period < 1 || period != as.integer(period)) stop("period must be a positive integer")
  multiplier <- 2 / (period + 1)
  ema_values <- numeric(length(data))
  ema_values[1L] <- data[1L]
  if (length(data) > 1L) {
    for (i in 2:length(data)) {
      ema_values[i] <- (data[i] - ema_values[i - 1L]) * multiplier + ema_values[i - 1L]
    }
  }
  return(ema_values)
}

# Source ema.R first. The result retains one value per input observation.
macd <- function(data, short_period, long_period, signal_period) {
  if (!exists("ema", mode = "function")) stop("Source ema.R before macd.R")
  if (length(short_period) != 1L || length(long_period) != 1L ||
      is.na(short_period) || is.na(long_period) || short_period >= long_period) {
    stop("short_period must be less than long_period")
  }
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  macd_line <- short_ema - long_ema
  signal_line <- ema(macd_line, signal_period)
  histogram <- macd_line - signal_line
  result <- list(macd_line = macd_line, signal_line = signal_line, histogram = histogram)
  return(result)
}

# Wilder RSI: first result follows period price changes, at position period+1.
rsi <- function(data, period) {
  if (!is.numeric(data) || anyNA(data) || any(!is.finite(data))) stop("data must be finite numeric values")
  if (length(period) != 1L || is.na(period) || period < 1 || period != as.integer(period)) stop("period must be a positive integer")
  if (length(data) <= period) stop("RSI needs at least period + 1 prices")
  diff_values <- diff(data)
  gains <- pmax(diff_values, 0)
  losses <- pmax(-diff_values, 0)
  avg_gain <- sum(gains[seq_len(period)]) / period
  avg_loss <- sum(losses[seq_len(period)]) / period
  rsi_values <- rep(NA_real_, length(data))
  score <- function(gain, loss) {
    if (gain == 0 && loss == 0) return(50)
    if (loss == 0) return(100)
    if (gain == 0) return(0)
    return(100 - 100 / (1 + gain / loss))
  }
  rsi_values[period + 1L] <- score(avg_gain, avg_loss)
  if (length(data) > period + 1L) {
    for (i in (period + 2L):length(data)) {
      avg_gain <- (avg_gain * (period - 1L) + gains[i - 1L]) / period
      avg_loss <- (avg_loss * (period - 1L) + losses[i - 1L]) / period
      rsi_values[i] <- score(avg_gain, avg_loss)
    }
  }
  return(rsi_values)
}

# Three labels follow the handout's pseudocode: None, Up, Down.
crossover <- function(arr1, arr2) {
  if (!is.numeric(arr1) || !is.numeric(arr2) || length(arr1) != length(arr2)) stop("Both arrays must be numeric and have the same length")
  n <- length(arr1)
  crossover_signals <- rep("None", n)
  if (n > 1L) {
    for (i in 2:n) {
      if (anyNA(c(arr1[i - 1L], arr1[i], arr2[i - 1L], arr2[i]))) next
      if (arr1[i] > arr2[i] && arr1[i - 1L] <= arr2[i - 1L]) crossover_signals[i] <- "Up"
      else if (arr1[i] < arr2[i] && arr1[i - 1L] >= arr2[i - 1L]) crossover_signals[i] <- "Down"
    }
  }
  return(crossover_signals)
}

# Text labels follow the supplied pseudocode: first None, then True/False.
crossunder <- function(arr1, arr2) {
  if (!is.numeric(arr1) || !is.numeric(arr2) || length(arr1) != length(arr2)) stop("Both arrays must be numeric and have the same length")
  n <- length(arr1)
  crossunder_signals <- rep("False", n)
  if (n > 0L) crossunder_signals[1L] <- "None"
  if (n > 1L) {
    for (i in 2:n) {
      if (anyNA(c(arr1[i - 1L], arr1[i], arr2[i - 1L], arr2[i]))) {
        crossunder_signals[i] <- "False"
      } else if (arr1[i] < arr2[i] && arr1[i - 1L] >= arr2[i - 1L]) {
        crossunder_signals[i] <- "True"
      }
    }
  }
  return(crossunder_signals)
}

# Convert Yahoo's xts OHLCV result to an ordinary data frame and check it.
as_ohlcv <- function(stock) {
  out <- data.frame(Date = as.Date(zoo::index(stock)),
                    Open = as.numeric(quantmod::Op(stock)),
                    High = as.numeric(quantmod::Hi(stock)),
                    Low = as.numeric(quantmod::Lo(stock)),
                    Close = as.numeric(quantmod::Cl(stock)),
                    Volume = as.numeric(quantmod::Vo(stock)))
  clean_ohlcv(out)
}
clean_ohlcv <- function(out) {
  expected <- c("Date", "Open", "High", "Low", "Close", "Volume")
  if (!all(expected %in% names(out))) stop("Data must have Date, Open, High, Low, Close, Volume columns")
  out <- out[, expected]; out$Date <- as.Date(out$Date)
  for (name in expected[-1]) out[[name]] <- as.numeric(out[[name]])
  out <- out[stats::complete.cases(out), ]; out <- out[order(out$Date), ]
  if (!nrow(out) || anyDuplicated(out$Date) || any(out$High < pmax(out$Open, out$Close, out$Low)) ||
      any(out$Low > pmin(out$Open, out$Close)) || any(out$Volume < 0) ||
      any(!is.finite(as.matrix(out[, expected[-1]])))) stop("No usable OHLCV observations or invalid prices")
  rownames(out) <- NULL; out
}

# Aggregate trading days to one OHLCV bar per week or calendar month.
to_timeframe <- function(d, timeframe) {
  if (timeframe == "Daily") return(d)
  if (!timeframe %in% c("Weekly", "Monthly")) stop("Unknown time frame")
  key <- if (timeframe == "Weekly") as.character(cut(d$Date, "week")) else format(d$Date, "%Y-%m")
  groups <- split(seq_len(nrow(d)), factor(key, levels = unique(key)))
  bars <- lapply(groups, function(i) data.frame(Date = max(d$Date[i]), Open = d$Open[i[1]],
                 High = max(d$High[i]), Low = min(d$Low[i]), Close = d$Close[tail(i, 1)],
                 Volume = sum(d$Volume[i])))
  result <- do.call(rbind, bars); rownames(result) <- NULL; result
}

# Compute indicators over the full fetched history before cropping the visible dates.
# Buy/Sell only occur when short SMA crosses long SMA; otherwise Hold.
add_indicators <- function(d, short_period, long_period, rsi_period) {
  if (short_period >= long_period) stop("Short SMA must be less than long SMA")
  align_sma <- function(n) {
    if (nrow(d) < n) return(rep(NA_real_, nrow(d)))
    c(rep(NA_real_, n - 1L), sma(d$Close, n))
  }
  d$ShortSMA <- align_sma(short_period)
  d$LongSMA <- align_sma(long_period)
  d$RSI <- if (nrow(d) <= rsi_period) rep(NA_real_, nrow(d)) else rsi(d$Close, rsi_period)
  m <- macd(d$Close, 12, 26, 9)
  d$MACD <- m$macd_line; d$MACDSignal <- m$signal_line; d$MACDHistogram <- m$histogram
  up <- crossover(d$ShortSMA, d$LongSMA)
  down <- crossunder(d$ShortSMA, d$LongSMA)
  d$Signal <- ifelse(up == "Up", "Buy", ifelse(down == "True", "Sell", "Hold"))
  d
}

palette <- c(Buy = "#087f5b", Sell = "#d94841", Hold = "#64748b")
base_theme <- function() ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.title = ggplot2::element_text(face = "bold"))

price_plot <- function(d, symbol, chart_type, indicators, timeframe) {
  gg <- ggplot2::ggplot(d, ggplot2::aes(x = Date))
  if (chart_type == "Candlestick") {
    width <- if (nrow(d) > 1L) 0.7 * stats::median(diff(as.numeric(d$Date))) else 1
    d$Direction <- ifelse(d$Close >= d$Open, "Up", "Down")
    gg <- gg + ggplot2::geom_segment(ggplot2::aes(y = Low, yend = High, xend = Date), colour = "#475569") +
      ggplot2::geom_rect(data = d, ggplot2::aes(xmin = Date - width / 2, xmax = Date + width / 2,
                           ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = Direction),
                         inherit.aes = FALSE, colour = "#334155", linewidth = 0.2) +
      ggplot2::scale_fill_manual(values = c(Up = "#2a9d8f", Down = "#e76f51"))
  } else if (chart_type == "Area") {
    gg <- gg + ggplot2::geom_area(ggplot2::aes(y = Close), fill = "#a5d8ff", alpha = 0.65) +
      ggplot2::geom_line(ggplot2::aes(y = Close), colour = "#1864ab", linewidth = 0.7)
  } else {
    gg <- gg + ggplot2::geom_line(ggplot2::aes(y = Close), colour = "#1864ab", linewidth = 0.8)
  }
  if ("SMA" %in% indicators) {
    gg <- gg + ggplot2::geom_line(ggplot2::aes(y = ShortSMA, colour = "Short SMA"), linewidth = 0.65, na.rm = TRUE) +
      ggplot2::geom_line(ggplot2::aes(y = LongSMA, colour = "Long SMA"), linewidth = 0.65, na.rm = TRUE)
  }
  marks <- d[d$Signal != "Hold", ]
  if (nrow(marks)) gg <- gg +
    ggplot2::geom_point(data = marks, ggplot2::aes(y = Close, colour = Signal), size = 2.5,
                        show.legend = FALSE) +
    ggplot2::geom_text(data = marks, ggplot2::aes(y = Close, label = Signal, colour = Signal),
                       vjust = -0.9, size = 3.2, check_overlap = TRUE, show.legend = FALSE)
  gg + ggplot2::scale_colour_manual(values = c("Short SMA" = "#e67700", "Long SMA" = "#7048e8",
                                              Buy = unname(palette["Buy"]), Sell = unname(palette["Sell"]))) +
    ggplot2::labs(title = paste(symbol, timeframe, "price and crossover signals"),
                     subtitle = "Buy/Sell labels mark actual SMA crossings; other bars are Hold.",
                     x = NULL, y = "Price (USD)", colour = NULL, fill = NULL) + base_theme()
}

rsi_plot <- function(d) {
  ggplot2::ggplot(d, ggplot2::aes(Date, RSI)) + ggplot2::geom_line(colour = "#7b2cbf", linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_hline(yintercept = c(30, 70), linetype = "dashed", colour = "#94a3b8") +
    ggplot2::coord_cartesian(ylim = c(0, 100)) +
    ggplot2::labs(title = "Relative Strength Index (RSI)", x = NULL, y = "RSI (0-100)") + base_theme()
}
macd_plot <- function(d) {
  ggplot2::ggplot(d, ggplot2::aes(Date)) +
    ggplot2::geom_col(ggplot2::aes(y = MACDHistogram), fill = "#94a3b8", width = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = MACD, colour = "MACD"), linewidth = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = MACDSignal, colour = "Signal"), linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = c(MACD = "#1864ab", Signal = "#e67700")) +
    ggplot2::labs(title = "MACD (12, 26, 9)", x = NULL, y = "MACD", colour = NULL) + base_theme()
}

portfolio <- c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA")
ui <- shiny::fluidPage(
  shiny::tags$head(shiny::tags$style(shiny::HTML("body {background:#f6f8fb;color:#17283b;} .well {background:#fff;border-radius:12px;} .tab-content {padding-top:14px;}"))),
  shiny::titlePanel("Portfolio Technical Analysis | Mahmoud Bakar"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shiny::selectInput("symbol", "Portfolio symbol", choices = portfolio),
      shiny::dateRangeInput("date_range", "Visible date range", start = "2025-01-01", end = "2025-12-31"),
      shiny::selectInput("time_frame", "Time frame", choices = c("Daily", "Weekly", "Monthly")),
      shiny::selectInput("chart_type", "Price chart", choices = c("Line", "Candlestick", "Area")),
      shiny::checkboxGroupInput("indicators", "Indicators", choices = c("Moving averages" = "SMA", "RSI", "MACD"),
                                selected = c("SMA", "RSI", "MACD")),
      shiny::numericInput("short_ma", "Short SMA bars", value = 20, min = 2, max = 100, step = 1),
      shiny::numericInput("long_ma", "Long SMA bars", value = 50, min = 3, max = 150, step = 1),
      shiny::numericInput("rsi_period", "RSI bars", value = 14, min = 2, max = 100, step = 1),
      shiny::selectInput("source_mode", "Data source",
                         choices = c("Saved Yahoo Finance data (2025)" = "snapshot", "Fetch from Yahoo Finance" = "live")),
      shiny::actionButton("fetch", "Load data", class = "btn-primary"),
      shiny::hr(),
      shiny::helpText("The saved data works offline. Live fetch requests earlier bars for indicator warm-up."),
      shiny::fileInput("csv", "Optional local OHLCV CSV fallback", accept = ".csv"),
      shiny::helpText("CSV columns: Date, Open, High, Low, Close, Volume. A local file represents the selected symbol.")),
    shiny::mainPanel(width = 9,
      shiny::verbatimTextOutput("status"),
      shiny::plotOutput("price_chart", height = "410px"),
      shiny::conditionalPanel("input.indicators && input.indicators.indexOf('RSI') >= 0", shiny::plotOutput("rsi_chart", height = "220px")),
      shiny::conditionalPanel("input.indicators && input.indicators.indexOf('MACD') >= 0", shiny::plotOutput("macd_chart", height = "220px")),
      shiny::h4("Trading signals"), shiny::tableOutput("signal_table"),
      shiny::helpText("Illustrative crossover rules only; this dashboard is for coursework, not investment advice."))))

server <- function(input, output, session) {
  # Load on startup or when requested. Display settings stay reactive.
  downloaded <- shiny::eventReactive(input$fetch, {
    shiny::req(input$symbol, input$date_range)
    symbol <- input$symbol
    if (!symbol %in% portfolio) stop("Select a portfolio symbol")
    start <- as.Date(input$date_range[1]); end <- as.Date(input$date_range[2])
    if (is.na(start) || is.na(end) || start > end || end > Sys.Date() + 1) stop("Select a valid date range")
    if (!is.null(input$csv)) {
      local <- tryCatch(clean_ohlcv(utils::read.csv(input$csv$datapath, stringsAsFactors = FALSE)), error = identity)
      return(list(data = local, source = paste("Local CSV:", input$csv$name), symbol = symbol))
    }
    if (!identical(input$source_mode, "live")) {
      path <- file.path("market_data", paste0(symbol, ".csv"))
      local <- tryCatch(clean_ohlcv(utils::read.csv(path)), error = identity)
      return(list(data = local, source = "Yahoo Finance snapshot (2025)", symbol = symbol))
    }
    # Six years warm up the default 50-bar monthly SMA when available.
    from <- start - 365 * 6
    fetched <- tryCatch(quantmod::getSymbols(symbol, src = "yahoo", from = from,
                           to = end + 1, auto.assign = FALSE, warnings = FALSE), error = identity)
    if (!inherits(fetched, "error")) fetched <- tryCatch(as_ohlcv(fetched), error = identity)
    list(data = fetched, source = "Yahoo Finance", symbol = symbol)
  }, ignoreNULL = FALSE)

  prepared <- shiny::reactive({
    raw <- downloaded()
    shiny::validate(shiny::need(!inherits(raw$data, "error"),
      if (inherits(raw$data, "error")) paste("Data could not be loaded:", conditionMessage(raw$data)) else ""))
    shiny::req(input$time_frame, input$date_range, input$short_ma, input$long_ma, input$rsi_period)
    shiny::validate(shiny::need(input$short_ma < input$long_ma, "Short SMA must be less than long SMA"))
    # Limit the input before aggregation so a partial bar cannot include future dates.
    daily <- raw$data[raw$data$Date <= as.Date(input$date_range[2]), ]
    shiny::validate(shiny::need(nrow(daily) > 0L, "No data on or before the end date"))
    d <- to_timeframe(daily, input$time_frame)
    d <- add_indicators(d, as.integer(input$short_ma), as.integer(input$long_ma), as.integer(input$rsi_period))
    d <- d[d$Date >= as.Date(input$date_range[1]) & d$Date <= as.Date(input$date_range[2]), ]
    shiny::validate(shiny::need(nrow(d) > 0L, "No observations in the visible date range"))
    list(data = d, source = raw$source, symbol = raw$symbol)
  })

  output$status <- shiny::renderText({
    z <- prepared(); paste(z$symbol, "|", z$source, "|", nrow(z$data), "bars |",
                           format(min(z$data$Date)), "to", format(max(z$data$Date)),
                           "| latest signal:", tail(z$data$Signal, 1),
                           if (all(is.na(z$data$LongSMA))) "| Insufficient bars for long SMA" else "")
  })
  output$price_chart <- shiny::renderPlot({
    z <- prepared(); price_plot(z$data, z$symbol, input$chart_type, input$indicators, input$time_frame)
  })
  output$rsi_chart <- shiny::renderPlot({ rsi_plot(prepared()$data) })
  output$macd_chart <- shiny::renderPlot({ macd_plot(prepared()$data) })
  output$signal_table <- shiny::renderTable({
    z <- prepared()$data
    tail(z[, c("Date", "Close", "ShortSMA", "LongSMA", "RSI", "Signal")], 12)
  }, digits = 2, rownames = FALSE)
}

shiny::shinyApp(ui, server)
