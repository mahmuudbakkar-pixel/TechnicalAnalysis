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
