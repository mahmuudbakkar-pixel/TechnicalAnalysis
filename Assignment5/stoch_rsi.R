# Source rsi.R first. %K is rolling-normalised RSI; %D smooths %K.
# Both series remain aligned with the original price observations.
stoch_rsi <- function(data, period, k_period, d_period) {
  if (!exists("rsi", mode = "function")) stop("Source rsi.R before stoch_rsi.R")
  for (p in list(k_period, d_period)) {
    if (length(p) != 1L || is.na(p) || p < 1 || p != as.integer(p)) stop("k_period and d_period must be positive integers")
  }
  rsi_values <- rsi(data, period)
  n <- length(rsi_values)
  k_line <- rep(NA_real_, n)
  d_line <- rep(NA_real_, n)
  first_k <- period + k_period
  if (first_k <= n) {
    for (i in first_k:n) {
      window <- rsi_values[(i - k_period + 1L):i]
      lowest <- min(window)
      highest <- max(window)
      k_line[i] <- if (highest == lowest) 0.5 else (rsi_values[i] - lowest) / (highest - lowest)
    }
  }
  first_d <- first_k + d_period - 1L
  if (first_d <= n) {
    for (i in first_d:n) d_line[i] <- sum(k_line[(i - d_period + 1L):i]) / d_period
  }
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}
