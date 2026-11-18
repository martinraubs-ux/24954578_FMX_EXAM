solve_rebalance_t <- function(hist_mat_raw, Amat, b_eq, b_lb, b_grp, tickers, gamma_mod = 5) {
  
  N <- length(tickers)
  
  # 1. Enforce 5-year (60-month) eligibility BEFORE imputing missing observations
  obs_t  <- colSums(!is.na(hist_mat_raw))
  b_ub_t <- dplyr::if_else(obs_t >= 60, -0.35, 0.00)
  
  # Safety check: if fewer than 3 assets are eligible (3 * 0.35 = 1.05 >= 1), relax to >= 36 months
  if (sum(-b_ub_t) < 1.0) {
    b_ub_t <- dplyr::if_else(obs_t >= 36, -0.35, 0.00)
  }
  bvec_t <- c(b_eq, b_lb, b_ub_t, b_grp)
  
  # 2. Impute sporadic NAs on eligible columns using column medians
  clean_mat <- apply(hist_mat_raw, 2, function(col) {
    if (all(is.na(col))) return(rep(0, length(col)))
    col[is.na(col)] <- median(col, na.rm = TRUE)
    col
  })
  
  # Annualize Sigma_t and mu_t (* 12) so Dmat is well-conditioned for quadprog::solve.QP
  Sigma_t <- RiskPortfolios::covEstimation(clean_mat, control = list(type = "lw")) * 12
  mu_t    <- colMeans(clean_mat, na.rm = TRUE) * 12
  
  safe_qp <- purrr::safely(quadprog::solve.QP)
  
  # Strategy A: Minimum Variance (MinVol)
  res_minvol <- safe_qp(Dmat = 2 * Sigma_t, dvec = rep(0, N), Amat = Amat, bvec = bvec_t, meq = 1)
  if (!is.null(res_minvol$error)) {
    stop("Feasibility error in MinVol — check that eligible Local/Bond assets can sum to 1 under b_grp.")
  }
  w_mv_min <- pmax(0, res_minvol$result$solution)
  w_mv_min <- w_mv_min / sum(w_mv_min)
  
  # Strategy B: Moderate Mean-Variance (gamma = 5)
  res_mv5 <- safe_qp(Dmat = gamma_mod * Sigma_t, dvec = mu_t, Amat = Amat, bvec = bvec_t, meq = 1)
  w_mv_5  <- if (is.null(res_mv5$error)) {
    w <- pmax(0, res_mv5$result$solution)
    w / sum(w)
  } else {
    w_mv_min
  }
  
  # Strategy C: Max Sharpe Ratio along a well-conditioned gamma grid
  gamma_grid <- c(2, 3, 5, 8, 12, 20, 40, 80, 150)
  sr_candidates <- purrr::map(gamma_grid, function(g) {
    res_g <- safe_qp(Dmat = g * Sigma_t, dvec = mu_t, Amat = Amat, bvec = bvec_t, meq = 1)
    if (!is.null(res_g$error)) return(NULL)
    
    w_g <- pmax(0, res_g$result$solution)
    w_g <- w_g / sum(w_g)
    vol_g <- sqrt(as.numeric(t(w_g) %*% Sigma_t %*% w_g))
    sr    <- if (vol_g > 0) (sum(w_g * mu_t) - 0.06) / vol_g else -Inf
    list(w = w_g, sr = sr)
  }) %>%
    purrr::compact()
  
  w_max_sr <- if (length(sr_candidates) > 0) {
    sr_candidates[[which.max(purrr::map_dbl(sr_candidates, "sr"))]]$w
  } else {
    w_mv_5
  }
  
  # Strategy D: Constrained ERC
  elig_t <- which(b_ub_t < 0)
  w_erc_raw <- rep(0, N)
  erc_fit <- purrr::safely(RiskPortfolios::optimalPortfolio)(
    Sigma = Sigma_t[elig_t, elig_t, drop = FALSE],
    control = list(type = "erc", constraint = "lo")
  )
  w_erc_raw[elig_t] <- if (is.null(erc_fit$error)) {
    as.numeric(erc_fit$result)
  } else {
    (1 / sqrt(diag(Sigma_t)[elig_t])) / sum(1 / sqrt(diag(Sigma_t)[elig_t]))
  }
  
  res_erc <- safe_qp(Dmat = diag(N), dvec = w_erc_raw, Amat = Amat, bvec = bvec_t, meq = 1)
  w_erc_t <- if (is.null(res_erc$error)) {
    w <- pmax(0, res_erc$result$solution)
    w / sum(w)
  } else {
    w_mv_min
  }
  
  # Percentage Risk Contributions (%RC) at date t
  calc_rc <- function(w) as.numeric(w * (Sigma_t %*% w)) / as.numeric(t(w) %*% Sigma_t %*% w)
  
  tibble::tibble(
    Tickers    = tickers,
    MinVol     = w_mv_min,
    MV_Gamma5  = w_mv_5,
    Max_Sharpe = w_max_sr,
    ERC        = w_erc_t,
    RC_MinVol  = calc_rc(w_mv_min),
    RC_MV5     = calc_rc(w_mv_5),
    RC_MaxSR   = calc_rc(w_max_sr),
    RC_ERC     = calc_rc(w_erc_t)
  )
}