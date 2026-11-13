# Source rsi.R and sma.R first. Smooth normalized RSI to obtain %K and %D.
# Both series remain aligned with the original price observations.
stoch_rsi <- function(data, period, k_period, d_period) {
  if (!exists("rsi", mode = "function")) stop("Source rsi.R before stoch_rsi.R")
  for (p in list(k_period, d_period)) {
    if (length(p) != 1L || is.na(p) || p < 1 || p != as.integer(p)) stop("k_period and d_period must be positive integers")
  }
  rsi_values <- rsi(data, period)
  valid <- rsi_values[!is.na(rsi_values)]
  span <- max(valid) - min(valid)
  normalized <- if (span == 0) rep(0.5, length(valid)) else (valid - min(valid)) / span
  # The course pseudocode uses the entire series range, then two SMAs.
  # This retrospective indicator is not used in time-causal trading rules.
  smooth <- function(x, width) {
    if (length(x) < width) return(numeric())
    sma(x, width)
  }
  k <- smooth(normalized, k_period)
  d <- smooth(k, d_period)
  align <- function(x) c(rep(NA_real_, length(data) - length(x)), x)
  list(k_line = align(k), d_line = align(d))
}
