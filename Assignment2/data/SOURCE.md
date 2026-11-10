# Data provenance

Downloaded on 10 November 2026 through quantmod 0.4.27, with `src="yahoo"`, `from="2025-01-01"`, `to="2026-01-01"` and `auto.assign=FALSE`. All five original provider calls succeeded; the query1 fallback was not used. The end date is exclusive.

Symbols: AAPL, MSFT, GOOGL, AMZN and TSLA. Each CSV has 250 observations, 2 January through 31 December 2025, and columns Date, Open, High, Low, Close, Volume, Adjusted. Prices are USD; volume is shares. Original floating-point precision is preserved. The frozen snapshots support reproduction because provider data may later be revised.

The analysis uses Close, not Adjusted. Dates are market observation dates, not every calendar day. Missing dates are not interpolated. All OHLCV observations passed finite-value, uniqueness, chronological-order and price-range checks. No stock prices were invented. Synthetic numbers occur only in the clearly labeled function tests in verify.R.

The initial download and run are recorded in output/execution_log.txt. Later cached runs are identified as such in output/run_sources.csv.
