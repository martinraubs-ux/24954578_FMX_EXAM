library(dplyr)
library(zoo)

calc_rolling_sd <- function(data, n = 21, annualize = FALSE) {

  # Basic input validation
  if (!all(c("date", "Tickers", "Returns") %in% names(data))) {
    stop("Dataframe must contain 'date', 'Tickers', and 'Returns' columns.")
  }
  if (!is.numeric(n) || n < 2) {
    stop("Lookback window 'n' must be a number greater than or equal to 2.")
  }

  result <- data %>%
    mutate(date = as.Date(date)) %>%
    arrange(Tickers, date) %>%
    group_by(Tickers) %>%
    mutate(
      # Convert simple decimal returns to log returns
      log_return = log(1 + Returns),

      # Calculate right-aligned rolling SD
      rolling_sd = rollapply(
        data = log_return,
        width = n,
        FUN = sd,
        fill = NA,
        align = "right"
      )
    ) %>%
    slice(-n) %>%
    ungroup()

  # Optionally add annualized volatility column
  if (annualize) {
    result <- result %>%
      mutate(annualized_vol = rolling_sd * sqrt(252)* 100)
  }

  return(result)
}