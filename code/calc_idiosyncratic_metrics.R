library(tidyverse)
library(slider)       
library(lubridate)
library(PerformanceAnalytics)

# 3. PURE FUNCTION: IDIOSYNCRATIC VOLATILITY & BETA DISPERSION (USING Idx_daily)
calc_idiosyncratic_metrics <- function(const_df, idx_df, benchmark_ticker = "J433", window = 60) {
  
  bench <- idx_df %>%
    filter(Tickers == benchmark_ticker) %>%
    mutate(date = as.Date(date)) %>%
    select(date, Bench_Ret = Returns) %>%
    distinct(date, .keep_all = TRUE)
  
  const_df %>%
    mutate(
      date   = as.Date(date),
      Return = as.numeric(Return)
    ) %>%
    filter(date >= as.Date("2019-10-01"), !is.na(Return)) %>%
    group_by(date, Tickers) %>%
    summarise(Return = mean(Return, na.rm = TRUE), .groups = "drop") %>%
    inner_join(bench, by = "date") %>%
    group_by(Tickers) %>%
    arrange(date, .by_group = TRUE) %>%
    # Filter out stocks with fewer observations than the window
    filter(n() >= window) %>%
    mutate(
      # Vectorized/C-level rolling moments via slider
      cov_im  = slide2_dbl(Return, Bench_Ret, cov, .before = window - 1, .complete = TRUE),
      var_m   = slide_dbl(Bench_Ret, var, .before = window - 1, .complete = TRUE),
      var_i   = slide_dbl(Return, var, .before = window - 1, .complete = TRUE),
      
      # Closed-form OLS Beta & Annualized Residual Volatility
      beta     = cov_im / var_m,
      res_var  = pmax(0, (window - 1) / (window - 2) * (var_i - (beta^2) * var_m)),
      idio_vol = sqrt(res_var) * sqrt(252)
    ) %>%
    ungroup() %>%
    filter(date >= as.Date("2020-01-01"), !is.na(beta)) %>%
    group_by(date) %>%
    summarise(
      mean_idio_vol   = mean(idio_vol, na.rm = TRUE),
      beta_dispersion = sd(beta, na.rm = TRUE),
      .groups         = "drop"
    )
}