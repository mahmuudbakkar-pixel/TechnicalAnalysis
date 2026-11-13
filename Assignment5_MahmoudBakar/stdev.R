# Population standard deviation: denominator n, following the handout.
stdev <- function(data) {
  if (!is.numeric(data) || length(data) == 0L || anyNA(data) || any(!is.finite(data))) stop("data must be nonempty finite numeric values")
  mean_value <- sum(data) / length(data)
  diff_values <- data - mean_value
  squared_diff <- diff_values * diff_values
  variance <- sum(squared_diff) / length(squared_diff)
  standard_deviation <- sqrt(variance)
  return(standard_deviation)
}
