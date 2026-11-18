construct_bond_tri <- function(yield_df, maturity = 10, freq = 2) {
  # Expects tidy long format: (date, Ticker, Yield) — pivot_longer first if wide
  n_periods <- maturity * freq
  
  yield_df %>%
    mutate(
      date = as.Date(date),
      # Automatically convert percentage points (e.g. 4.25) to decimals (0.0425)
      y = if_else(abs(TRI) > 0.5, TRI / 100, TRI)
    ) %>%
    arrange(Ticker, date) %>%
    group_by(Ticker) %>%
    mutate(
      y_lag = lag(y),
      dy    = y - y_lag,
      
      # Modified Duration & Convexity of a par bond at t-1
      disc  = (1 + y_lag / freq)^(-n_periods),
      mod_dur = if_else(y_lag > 1e-5, (1 / y_lag) * (1 - disc), maturity / (1 + y_lag / freq)),
      convexity = if_else(
        y_lag > 1e-5,
        (2 / (y_lag^2)) * (1 - disc) - (n_periods / (y_lag * (1 + y_lag / freq)^(n_periods + 1))),
        maturity^2
      ),
      
      # Monthly Total Return = Carry + Duration Price Change + Convexity Adjustment
      Bond_Ret = (y_lag / 12) - (mod_dur * dy) + (0.5 * convexity * (dy^2)),
      Bond_Ret = coalesce(Bond_Ret, 0),
      
      # Compound into Total Return Index (Base = 100)
      TRI = 100 * cumprod(1 + Bond_Ret)
    ) %>%
    ungroup() %>%
    select(date, Ticker, mod_dur, Bond_Ret, TRI)
}
