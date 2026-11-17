pacman::p_load("tidyverse")

#' Ledoit-Wolf linear shrinkage (2004), target = scaled identity
#'
#' @param X  Numeric matrix (rows = observations, columns = series).
#' @return   Shrunk covariance matrix, with the shrinkage intensity as attribute "shrinkage".

LW_shrink <- function(X) {

  X <- scale(as.matrix(X), center = TRUE, scale = FALSE)
  n <- nrow(X)
  p <- ncol(X)

  S <- crossprod(X) / n                      # sample covariance (1/n)
  m <- sum(diag(S)) / p                      # average variance = target scale

  d2 <- sum((S - m * diag(p))^2) / p         # dispersion of S around the target

  # Estimation error of S (b-bar squared):
  b2_bar <- (sum(rowSums(X^2)^2) - n * sum(S^2)) / (n^2 * p)
  b2 <- min(b2_bar, d2)                      # cannot exceed d2

  shrinkage <- b2 / d2                       # intensity between 0 and 1

  out <- shrinkage * m * diag(p) + (1 - shrinkage) * S
  dimnames(out) <- dimnames(S)

  attr(out, "shrinkage") <- shrinkage
  out
}


#' PCA by hand (eigen decomposition), with optional Ledoit-Wolf shrinkage
#'
#' @param data_wide  Wide data frame of numeric series (one column per asset).
#' @param scale.     TRUE = correlation matrix; FALSE = covariance matrix.
#' @param shrink     "none" = sample matrix; "lw" = Ledoit-Wolf shrinkage.
#' @param date_col   Name of the date column, or NULL if there is none.

PCA_byhand <- function(data_wide, scale. = TRUE, shrink = c("none", "lw"), date_col = "date") {

  shrink <- match.arg(shrink)

  if (!is.null(date_col) && date_col %in% names(data_wide)) {
    data_wide <- data_wide %>% select(-all_of(date_col))
  }

  X <- scale(as.matrix(data_wide), center = TRUE, scale = scale.)

  # Dispersion matrix:
  if (shrink == "none") {
    dispmat <- if (scale.) cor(data_wide) else cov(data_wide)
    intensity <- NA_real_
  } else {
    # Shrinking the standardised data towards the identity shrinks correlations towards zero
    dispmat <- LW_shrink(X)
    intensity <- attr(dispmat, "shrinkage")
    if (scale.) dispmat <- cov2cor(dispmat)
  }

  eig <- eigen(dispmat, symmetric = TRUE)
  evec <- eig$vectors
  eval <- eig$values

  rownames(evec) <- colnames(X)
  colnames(evec) <- paste0("PC", seq_len(ncol(evec)))

  prop <-
    tibble(PC = factor(colnames(evec), levels = colnames(evec)),
           Eigenvalue = eval,
           Prop_Var = eval / sum(eval)) %>%
    mutate(Cum_Var = cumsum(Prop_Var))

  list(eigenvalues  = eval,
       eigenvectors = evec,
       prop_var     = prop,
       scores       = X %*% evec,
       dispersion   = dispmat,
       shrinkage    = intensity)
}


# ---------------------------------------------------------------
# Example: compare sample vs Ledoit-Wolf
# ---------------------------------------------------------------

# data_byhand <-
#   data %>% select(date, Tickers, Returns) %>% spread(Tickers, Returns)

# pca_s  <- PCA_byhand(data_byhand, shrink = "none")
# pca_lw <- PCA_byhand(data_byhand, shrink = "lw")

# pca_lw$shrinkage                          # how much shrinkage was applied
# bind_cols(pca_s$prop_var %>% select(PC, Sample = Prop_Var),
#           LW = pca_lw$prop_var$Prop_Var)  # variance explained, side by side

# With identity-target shrinkage the eigenvectors are unchanged (up to sign):
# all.equal(abs(pca_s$eigenvectors), abs(pca_lw$eigenvectors))
