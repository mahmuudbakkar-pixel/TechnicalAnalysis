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
