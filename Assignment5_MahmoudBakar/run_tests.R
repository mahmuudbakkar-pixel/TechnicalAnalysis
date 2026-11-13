# BDA400 Assignment 5 - reproducible base R checks
# Run from inside Assignment5: Rscript run_tests.R
files <- c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
           "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R")
for (file in files) source(file)
near <- function(actual, expected, tol = 1e-10) {
  stopifnot(length(actual) == length(expected),
            identical(is.na(actual), is.na(expected)),
            all(abs(actual[!is.na(actual)] - expected[!is.na(expected)]) < tol))
}
expect_error <- function(expr) {
  result <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  stopifnot(result)
}

near(sma(c(10, 12, 15, 20, 18), 3), c(37/3, 47/3, 53/3))
expect_error(sma(1:2, 3))
near(ema(c(10, 12, 14), 3), c(10, 11, 12.5))
near(ema(5, 1), 5)
macd_result <- macd(c(10, 12, 14, 13), 2, 3, 2)
near(macd_result$histogram, macd_result$macd_line - macd_result$signal_line)
near(macd_result$macd_line, c(0, 1/3, 11/18, 31/108))
near(macd_result$signal_line, c(0, 2/9, 13/27, 19/54))
near(stdev(c(2, 4, 6)), sqrt(8/3))
near(stdev(7), 0)
fit <- linreg(c(1, 3, 5, 7, 9), 3, 1)
near(fit$slope, 2); near(fit$intercept, 3); near(fit$predicted_values, c(5, 7))
expect_error(linreg(1:3, 3, 2))
near(stoch_rsi(rep(5, 8), 2, 3, 2)$d_line, c(rep(NA, 5), rep(0.5, 3)))
near(rsi(1:6, 3), c(NA, NA, NA, 100, 100, 100))
near(rsi(6:1, 3), c(NA, NA, NA, 0, 0, 0))
near(rsi(rep(5, 6), 3), c(NA, NA, NA, 50, 50, 50))
stoch <- stoch_rsi(c(10, 12, 11, 13, 12, 14, 13, 15, 14, 16), 3, 3, 2)
stopifnot(length(stoch$k_line) == 10L, length(stoch$d_line) == 10L,
          all(stoch$k_line[!is.na(stoch$k_line)] >= 0 & stoch$k_line[!is.na(stoch$k_line)] <= 1),
          all(is.na(stoch$d_line[1:6])))
stopifnot(identical(crossover(c(1, 3, 1), c(2, 2, 2)), c("None", "Up", "Down")))
stopifnot(identical(crossunder(c(1, 3, 1), c(2, 2, 2)), c("None", "False", "True")))
expect_error(crossover(1:3, 1:2))
expect_error(crossunder(1:3, 1:2))

cat("All nine indicators passed normal-case and edge-case checks in base R.\n")
cat("R version:", R.version.string, "\n")
