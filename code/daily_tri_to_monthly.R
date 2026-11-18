library(tidyverse)
library(lubridate)
library(tbl2xts)

# A. Daily TRI -> Monthly Returns (for MSCI & Commods)
daily_tri_to_monthly <- function(df) {
  df %>%
    rename_with(~ "Tickers", any_of(c("Ticker", "Tickers"))) %>%
    transmute(
      date    = as.Date(date),
      ym      = floor_date(date, "month"),
      Tickers = str_trim(as.character(Tickers)),
      TRI     = as.numeric(TRI)
    ) %>%
    filter(!is.na(ym), !is.na(Tickers), !is.na(TRI)) %>%
    arrange(Tickers, date) %>%
    group_by(Tickers, ym) %>%
    summarise(TRI = dplyr::last(TRI), .groups = "drop") %>%
    group_by(Tickers) %>%
    arrange(ym, .by_group = TRUE) %>%
    mutate(
      date    = ceiling_date(ym, "month") - days(1),
      Returns = (TRI / lag(TRI)) - 1
    ) %>%
    ungroup() %>%
    filter(!is.na(Returns)) %>%
    select(date, Tickers, Returns)
}