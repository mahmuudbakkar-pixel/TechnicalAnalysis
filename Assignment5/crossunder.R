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
