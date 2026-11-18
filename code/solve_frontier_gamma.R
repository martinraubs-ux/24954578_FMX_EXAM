solve_frontier_gamma <- function(gamma_val, mu, Sigma, Amat, bvec, tickers) {
  # Minimize (gamma / 2) * w' Sigma w - w' mu  <=>  Max w' mu - (gamma / 2) * w' Sigma w
  # Guard gamma = 0 with 1e-4 so Dmat stays strictly positive-definite
  sol <- quadprog::solve.QP(
    Dmat = pmax(gamma_val, 1e-4) * Sigma,
    dvec = mu,
    Amat = Amat,
    bvec = bvec,
    meq  = 1
  )
  
  w <- pmax(0, sol$solution)
  w <- w / sum(w)
  
  N        <- length(w)
  port_ret <- sum(w * mu) * 12
  port_vol <- sqrt(as.numeric(t(w) %*% Sigma %*% w)) * sqrt(12)
  
  # Check which group constraints are binding via sol$iact (cols 2N+2 to 2N+5 of Amat)
  active_cols <- sol$iact
  
  tibble(
    gamma        = gamma_val,
    Ann_Return   = port_ret,
    Ann_Vol      = port_vol,
    Sharpe_Ratio = (port_ret - 0.06) / port_vol, # Adjust Rf (e.g., 6% cash rate) as needed
    N_Eff        = 1 / sum(w^2),
    Bind_Bonds   = (2 * N + 2) %in% active_cols,
    Bind_Equity  = (2 * N + 3) %in% active_cols,
    Bind_Commods = (2 * N + 4) %in% active_cols,
    Bind_Global  = (2 * N + 5) %in% active_cols,
    weights      = list(tibble(Tickers = tickers, Weight = round(w, 6)))
  )
}