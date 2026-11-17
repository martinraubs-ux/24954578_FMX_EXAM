library(tidyverse)
library(slider)       # Functional rolling windows (slide_dbl, slide_dfr)
library(lubridate)
library(PerformanceAnalytics)

#PREPARE & JOIN MACRO VOLATILITY SERIES

prepare_macro_factors <- function(uncert_df, cncy_df, start_date = "2020-01-01") {
  
  # Standardize column names (Ticker vs Tickers) across both macro datasets
  clean_macro <- function(df) {
    df %>%
      rename_with(~ "Ticker", any_of(c("Tickers", "Ticker"))) %>%
      filter(date >= as.Date(start_date)) %>%
      select(date, Name, TRI) %>%
      distinct(date, Name, .keep_all = TRUE)
  }
  
  bind_rows(
    clean_macro(uncert_df),
    clean_macro(cncy_df)
  ) %>%
    pivot_wider(names_from = Name, values_from = TRI) %>%
    arrange(date) %>%
    fill(everything(), .direction = "down") # Carry forward holiday gaps
}