library(tidyverse)
library(lubridate)
library(tbl2xts)

# D. Already-Monthly LCL Series -> Standardize Month-End Date Only
prep_monthly_lcl <- function(df) {
  df %>%
    rename_with(~ "Tickers", any_of(c("Ticker", "Tickers"))) %>%
    transmute(
      date    = ceiling_date(floor_date(as.Date(date), "month"), "month") - days(1),
      Tickers = str_trim(as.character(Tickers)),
      Returns = as.numeric(Returns)
    ) %>%
    filter(!is.na(date), !is.na(Tickers), !is.na(Returns)) %>%
    distinct(date, Tickers, .keep_all = TRUE)
}
