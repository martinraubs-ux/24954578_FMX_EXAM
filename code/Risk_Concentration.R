pacman::p_load("tidyverse")

# Re-uses PCA_byhand() (and LW_shrink()) from the earlier file:
source("code/PCA_byhand_LW.R")

#' Portfolio risk concentration: stock-level and principal-component-level
#'
#' @param data_wide  Wide data frame of asset returns (date column + one column per asset).
#' @param weights    Named numeric vector of portfolio weights (names = asset column names).
#'                   Assets in data_wide with no weight get 0.
#' @param scale.     FALSE = PCA on the covariance matrix (risk in return units; default for
#'                   risk attribution). TRUE = PCA on the correlation matrix (exposures are
#'                   adjusted for each asset's volatility, so the decomposition is still exact).
#' @param shrink     "none" or "lw" (Ledoit-Wolf), passed on to PCA_byhand().
#' @param universe   "held" = only stocks with a non-zero weight enter the PCA and covariance
#'                   (the fund's own holdings); "all" = every column in data_wide is used,
#'                   with non-held stocks given zero weight.
#' @param date_col   Name of the date column, or NULL if none.
#' @param warn       Warn about weights that cannot be matched to the data.
#'
#' NA handling (as before): all-NA columns are dropped, remaining NAs are set to 0.
#' Zero-variance columns are also dropped.
#'
#' @return A list with:
#'   port_vol  - portfolio volatility in the frequency of the returns (e.g. daily)
#'   stock_rc  - each asset's % contribution to portfolio variance
#'   pc_rc     - PC exposure, variance contribution and % share for each PC
#'   ENB       - Effective Number of Bets (Meucci): exp(entropy) of the PC risk shares
#'   check     - TRUE if PC contributions add up to total portfolio variance

Risk_Concentration <- function(data_wide,
                               weights,
                               scale. = FALSE,
                               shrink = c("none", "lw"),
                               universe = c("held", "all"),
                               date_col = "date",
                               warn = TRUE) {
  
  shrink <- match.arg(shrink)
  universe <- match.arg(universe)
  
  if (is.null(names(weights))) stop("weights must be a named vector")
  
  returns <- data_wide
  if (!is.null(date_col) && date_col %in% names(returns)) {
    returns <- returns %>% select(-all_of(date_col))
  }
  
  # Drop all-NA columns, zero-fill the rest, drop zero-variance columns:
  returns <-
    returns %>%
    select(where(~ !all(is.na(.x)))) %>%
    mutate(across(everything(), ~ replace_na(.x, 0))) %>%
    select(where(~ sd(.x) > 0)) %>%
    as.data.frame()
  
  assets <- colnames(returns)
  
  # Match weights to assets:
  w <- setNames(rep(0, length(assets)), assets)
  common <- intersect(assets, names(weights))
  w[common] <- weights[common]
  
  if (warn && length(setdiff(names(weights), assets)) > 0) {
    warning("Weights with no return data (ignored): ",
            paste(setdiff(names(weights), assets), collapse = ", "))
  }
  
  # Keep only the stocks the fund actually holds:
  if (universe == "held") {
    held <- names(w)[w != 0]
    returns <- returns[, held, drop = FALSE]
    w <- w[held]
    assets <- held
  }
  
  # PCA by hand (optionally Ledoit-Wolf):
  pca <- PCA_byhand(returns, scale. = scale., shrink = shrink, date_col = NULL)
  
  # Volatility scaling: if PCA used the correlation matrix, Sigma = D R D
  sds <- if (scale.) apply(returns, 2, sd) else setNames(rep(1, length(assets)), assets)
  
  Sigma <- diag(sds) %*% pca$dispersion %*% diag(sds)
  dimnames(Sigma) <- list(assets, assets)
  
  port_var <- as.numeric(t(w) %*% Sigma %*% w)
  
  # Stock-level risk contribution: w_i * (Sigma w)_i / total variance
  stock_rc <-
    tibble(Tickers = assets,
           Weight = as.numeric(w),
           Risk_Contribution = as.numeric(w * (Sigma %*% w)) / port_var) %>%
    arrange(desc(Risk_Contribution))
  
  # PC-level: exposure b = E' (D w); variance contribution = b^2 * eigenvalue
  b <- as.numeric(t(pca$eigenvectors) %*% (sds * w))
  var_contrib <- b^2 * pca$eigenvalues
  
  pc_rc <-
    tibble(PC = colnames(pca$eigenvectors),
           Exposure = b,
           Eigenvalue = pca$eigenvalues,
           Var_Contribution = var_contrib,
           Share = var_contrib / sum(var_contrib)) %>%
    mutate(PC = factor(PC, levels = PC),
           Cum_Share = cumsum(Share))
  
  # Effective Number of Bets:
  p <- pc_rc$Share[pc_rc$Share > 0]
  ENB <- exp(-sum(p * log(p)))
  
  list(port_vol = sqrt(port_var),
       stock_rc = stock_rc,
       pc_rc    = pc_rc,
       ENB      = ENB,
       check    = isTRUE(all.equal(sum(var_contrib), port_var)))
}
