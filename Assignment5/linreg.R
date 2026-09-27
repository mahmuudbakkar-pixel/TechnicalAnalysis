# Regress the selected window on positions 1, ..., regressionLength.
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  if (!is.numeric(regressionSource) || anyNA(regressionSource) || any(!is.finite(regressionSource))) stop("regressionSource must be finite numeric values")
  n <- length(regressionSource)
  if (length(regressionLength) != 1L || is.na(regressionLength) || regressionLength < 2 || regressionLength != as.integer(regressionLength)) stop("regressionLength must be an integer at least 2")
  if (regressionLength > n) stop("regressionLength cannot be greater than the number of elements in regressionSource")
  if (length(regressionOffset) != 1L || is.na(regressionOffset) || regressionOffset < 0 || regressionOffset != as.integer(regressionOffset)) stop("regressionOffset must be a nonnegative integer")
  if (regressionOffset >= regressionLength) stop("regressionOffset must be less than regressionLength")
  if (regressionLength + regressionOffset > n) stop("Not enough observations before the requested offset")
  end_index <- n - regressionOffset
  start_index <- end_index - regressionLength + 1L
  source_subset <- regressionSource[start_index:end_index]
  index_values <- seq_along(source_subset)
  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  predicted_values <- slope * index_values + intercept
  result <- list(slope = slope, intercept = intercept, predicted_values = predicted_values)
  return(result)
}
