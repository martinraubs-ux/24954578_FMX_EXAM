solve_frontier_gamma <- function(gamma_val, mu, Sigma, Amat, bvec, tickers) {
  # Minimize (gamma / 2) * w' Sigma w - w' mu  <=>  Max w' mu - (gamma / 2) * w' Sigma w
  # Guard gamma = 0 with 1e-4 so Dmat stays strictly positive-definite
  sol <- quadprog::solve.QP(
    Dmat = gamma_val * Sigma,
    dvec = mu,
    Amat = Amat,
    bvec = bvec,
    meq  = 1
  )
  
  w <- pmax(0, sol$solution)
  w <- w / sum(w)
  
  ann_arith_ret <- sum(w * mu) * 12
  ann_var       <- as.numeric(t(w) %*% Sigma %*% w) * 12
  ann_vol       <- sqrt(ann_var)
  ann_geom_ret  <- ann_arith_ret - 0.5 * ann_var # Expected geometric (compounded) return
  
  tibble(
    gamma          = gamma_val,
    Ann_Arith_Ret  = ann_arith_ret,
    Ann_Geom_Ret   = ann_geom_ret,
    Ann_Vol        = ann_vol,
    Sharpe_Ratio   = (ann_arith_ret - 0.06) / ann_vol,
    N_Eff          = 1 / sum(w^2),
    weights        = list(tibble(Tickers = tickers, Weight = round(w, 6)))
  )
}