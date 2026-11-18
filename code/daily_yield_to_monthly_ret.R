library(tidyverse)
library(lubridate)
library(tbl2xts)

# C. Daily 10Y Bond Yields (YTM) -> Monthly Bond Total Returns
daily_yield_to_monthly_ret <- function(df, maturity = 10, freq = 2) {
  # If TRI was already constructed on Bond_Yields, convert TRI directly to monthly returns
  if ("TRI" %in% names(df)) {
    return(daily_tri_to_monthly(df))
  }
  
  # Otherwise identify the yield column (e.g., Yield, YTM, or 3rd column)
  df_std <- df %>%
    rename_with(~ "Tickers", any_of(c("Ticker", "Tickers"))) %>%
    rename_with(~ "Yield", any_of(c("Yield", "YTM", "yield", "Value")))
  
  n_periods <- maturity * freq
  scale_div <- if (max(abs(df_std$Yield), na.rm = TRUE) > 1) 100 else 1
  
  df_std %>%
    transmute(
      date    = as.Date(date),
      ym      = floor_date(date, "month"),
      Tickers = str_trim(as.character(Tickers)),
      Yield   = as.numeric(Yield)
    ) %>%
    filter(!is.na(ym), !is.na(Tickers), !is.na(Yield)) %>%
    arrange(Tickers, date) %>%
    # Snap to month-end yield first so carry (y_lag / 12) matches monthly frequency
    group_by(Tickers, ym) %>%
    summarise(Yield = dplyr::last(Yield), .groups = "drop") %>%
    group_by(Tickers) %>%
    arrange(ym, .by_group = TRUE) %>%
    mutate(
      y = Yield / scale_div,
      y_lag     = lag(y),
      dy        = y - y_lag,
      disc      = (1 + y_lag / freq)^(-n_periods),
      mod_dur   = if_else(y_lag > 1e-5, (1 / y_lag) * (1 - disc), maturity / (1 + y_lag / freq)),
      convexity = if_else(
        y_lag > 1e-5,
        (2 / (y_lag^2)) * (1 - disc) - (n_periods / (y_lag * (1 + y_lag / freq)^(n_periods + 1))),
        maturity^2
      ),
      date    = ceiling_date(ym, "month") - days(1),
      Returns = (y_lag / 12) - (mod_dur * dy) + (0.5 * convexity * (dy^2))
    ) %>%
    ungroup() %>%
    filter(!is.na(Returns)) %>%
    select(date, Tickers, Returns)
}