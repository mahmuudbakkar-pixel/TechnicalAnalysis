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
