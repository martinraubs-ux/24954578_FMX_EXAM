# Execute Rolling Quarterly Rebalance Backtest (120-month initial lookback)
run_quarterly_backtest <- function(return_mat_raw, Amat, b_eq, b_lb, b_grp,
                                   lookback_min = 120, tc_bps = 0.0015) {
  
  dates_vec <- return_mat_raw$date
  mat_raw   <- return_mat_raw %>% select(-date) %>% as.matrix()
  tickers   <- colnames(mat_raw)
  T_total   <- nrow(mat_raw)
  
  # Rebalance every 3 months starting once 120 months of post-2010 history exist
  reb_indices <- seq(lookback_min, T_total - 1, by = 3)
  
  reb_weights <- purrr::map_dfr(reb_indices, function(idx) {
    solve_rebalance_t(
      hist_mat_raw = mat_raw[1:idx, , drop = FALSE],
      Amat = Amat, b_eq = b_eq, b_lb = b_lb, b_grp = b_grp, tickers = tickers
    ) %>%
      mutate(reb_date = dates_vec[idx], reb_idx = idx)
  })
  
  strategies <- c("MinVol", "MV_Gamma5", "Max_Sharpe", "ERC")
  
  oos_returns <- purrr::map_dfr(strategies, function(strat) {
    w_prev_end <- NULL
    
    purrr::map_dfr(seq_along(reb_indices), function(k) {
      idx_start <- reb_indices[k]
      idx_end   <- if (k < length(reb_indices)) reb_indices[k + 1] else T_total
      
      w_target <- reb_weights %>%
        filter(reb_idx == idx_start) %>%
        arrange(match(Tickers, tickers)) %>%
        pull(!!sym(strat))
      
      turnover <- if (is.null(w_prev_end)) 0 else 0.5 * sum(abs(w_target - w_prev_end))
      n_eff    <- 1 / sum(w_target^2)
      w_curr   <- w_target
      
      q_df <- purrr::map_dfr((idx_start + 1):idx_end, function(m) {
        r_m        <- coalesce(mat_raw[m, ], 0)
        port_gross <- sum(w_curr * r_m)
        cost_m     <- if (m == idx_start + 1) turnover * tc_bps else 0
        port_net   <- port_gross - cost_m
        
        # Drift weights forward with realized monthly returns
        w_drifted <- w_curr * (1 + r_m)
        w_curr <<- if (sum(w_drifted) > 0) w_drifted / sum(w_drifted) else w_curr
        
        tibble(
          date      = dates_vec[m],
          Strategy  = strat,
          Gross_Ret = port_gross,
          Net_Ret   = port_net,
          Turnover  = if (m == idx_start + 1) turnover else 0,
          N_Eff     = n_eff
        )
      })
      
      w_prev_end <<- w_curr
      q_df
    })
  })
  
  list(weights = reb_weights, returns = oos_returns)
}