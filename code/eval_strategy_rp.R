library(tidyverse)
library(PerformanceAnalytics)
library(tbl2xts)
library(xts)

#  Functional wrapper around PerformanceAnalytics::Return.portfolio (or rmsfuns::Safe_Return.portfolio)
eval_strategy_rp <- function(strat_name, reb_weights_df, R_xts, tc_bps = 0.0015) {
  
  # Build xts of target weights stamped at each quarterly rebalance date
  W_xts <- reb_weights_df %>%
    select(date = reb_date, Tickers, Weight = !!sym(strat_name)) %>%
    pivot_wider(names_from = Tickers, values_from = Weight, values_fill = 0) %>%
    arrange(date) %>%
    select(date, all_of(colnames(R_xts))) %>%
    tbl_xts()
  
  # Run portfolio return engine with full weight drift & contribution attribution
  rp_out <- rmsfuns::Safe_Return.portfolio(
    R       = R_xts,
    weights = W_xts,
    verbose = TRUE
  )
  
  # Compute exact quarterly turnover from BOP.Weight vs. prior month's EOP.Weight
  bop_mat <- as.matrix(rp_out$BOP.Weight)
  eop_mat <- as.matrix(rp_out$EOP.Weight)
  
  turnover_vec <- c(
    0,
    0.5 * rowSums(abs(bop_mat[-1, , drop = FALSE] - eop_mat[-nrow(eop_mat), , drop = FALSE]))
  )
  
  tibble(
    date      = zoo::index(rp_out$returns),
    Strategy  = strat_name,
    Gross_Ret = as.numeric(rp_out$returns),
    Turnover  = turnover_vec,
    Net_Ret   = Gross_Ret - (Turnover * tc_bps)
  )
}