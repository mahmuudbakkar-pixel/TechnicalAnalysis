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
