library(tidyverse)
library(slider)       # Functional rolling windows (slide_dbl, slide_dfr)
library(lubridate)
library(PerformanceAnalytics)

# ==============================================================================
#  CROSS-SECTIONAL VOLATILITY (CSV) & SPREAD METRICS
# ==============================================================================
calc_cross_sectional_metrics <- function(const_df, window = 60, start_date = "2020-01-01") {
  const_df %>%
    mutate(
      date   = as.Date(date),
      Return = as.numeric(Return),
      Weight = as.numeric(weight)
    ) %>%
    filter(date >= as.Date(start_date), !is.na(Return), !is.na(Weight), Weight > 0) %>%
    # Drop extreme corporate-action data errors (> 40% single-day move on JSE)
    filter(abs(Return) <= 0.40) %>%
    group_by(date) %>%
    # Require at least 30 active stocks on a given day to avoid partial-day artifacts
    filter(n() >= 30) %>%
    mutate(w_norm = Weight / sum(Weight, na.rm = TRUE)) %>%
    summarise(
      n_stocks       = n(),
      R_bar_weighted = sum(w_norm * Return, na.rm = TRUE),
      R_bar_equal    = mean(Return, na.rm = TRUE),
      CSV_weighted   = sqrt(sum(w_norm * (Return - R_bar_weighted)^2, na.rm = TRUE)),
      CSV_equal      = sqrt(mean((Return - R_bar_equal)^2, na.rm = TRUE)),
      .groups        = "drop"
    ) %>%
    arrange(date) %>%
    mutate(
      # Match the 60-day window of your correlation and idiosyncratic vol metrics
      CSV_weighted_60d = slide_dbl(CSV_weighted, mean, .before = window - 1, .complete = TRUE),
      CSV_equal_60d    = slide_dbl(CSV_equal, mean, .before = window - 1, .complete = TRUE)
    )
}