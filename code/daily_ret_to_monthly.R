library(tidyverse)
library(lubridate)
library(tbl2xts)

# B. Daily Returns -> Monthly Compounded Returns (for Idx_Daily)
daily_ret_to_monthly <- function(df, keep_tickers = NULL) {
  df %>%
    rename_with(~ "Tickers", any_of(c("Ticker", "Tickers"))) %>%
    transmute(
      date    = as.Date(date),
      ym      = floor_date(date, "month"),
      Tickers = str_trim(as.character(Tickers)),
      Returns = as.numeric(Returns)
    ) %>%
    filter(!is.na(ym), !is.na(Tickers), !is.na(Returns)) %>%
    { if (!is.null(keep_tickers)) filter(., Tickers %in% keep_tickers) else . } %>%
    # Guard against duplicate daily rows
    group_by(Tickers, date, ym) %>%
    summarise(Returns = mean(Returns, na.rm = TRUE), .groups = "drop") %>%
    group_by(Tickers, ym) %>%
    summarise(
      date    = ceiling_date(dplyr::first(ym), "month") - days(1),
      Returns = prod(1 + Returns, na.rm = TRUE) - 1,
      .groups = "drop"
    ) %>%
    select(date, Tickers, Returns)
}