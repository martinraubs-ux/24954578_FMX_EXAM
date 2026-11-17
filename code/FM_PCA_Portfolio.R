pacman::p_load("tidyverse")

# Re-uses PCA_byhand() (and LW_shrink()) from the earlier file:
source("code/PCA_byhand_LW.R")
source("code/Risk_Concentration.R")

#' Fit a FactoMineR PCA for the same window / universe, with the portfolio as a
#' supplementary variable (so it can be plotted with factoextra)
#'
#' The rc object from Risk_Concentration() is a plain list, not a FactoMineR object,
#' so it cannot be plotted by factoextra directly. This function rebuilds the PCA on the
#' same data with FactoMineR::PCA(). The portfolio return series (weights x returns) is added as
#' a supplementary quantitative variable: it is NOT used to form the components, but its
#' correlation with each component is shown on the variable map.
#'
#' Note: FactoMineR's PCA does not apply Ledoit-Wolf shrinkage.
#'
#' @return A FactoMineR PCA object.

FM_PCA_Portfolio <- function(data_wide,
                             weights,
                             scale. = TRUE,
                             universe = c("held", "all"),
                             date_col = "date",
                             port_name = "Portfolio") {
  
  universe <- match.arg(universe)
  pacman::p_load("FactoMineR", "factoextra")
  
  returns <- data_wide
  if (!is.null(date_col) && date_col %in% names(returns)) {
    returns <- returns %>% select(-all_of(date_col))
  }
  
  returns <-
    returns %>%
    select(where(is.numeric)) %>%
    select(where(~ !all(is.na(.x)))) %>%
    mutate(across(everything(), ~ replace_na(.x, 0))) %>%
    select(where(~ sd(.x) > 0)) %>%
    as.data.frame()
  
  assets <- colnames(returns)
  w <- setNames(rep(0, length(assets)), assets)
  common <- intersect(assets, names(weights))
  w[common] <- weights[common]
  
  if (universe == "held") {
    held <- names(w)[w != 0]
    returns <- returns[, held, drop = FALSE]
    w <- w[held]
  }
  
  # Portfolio return series over the window:
  df <- returns
  df[[port_name]] <- as.numeric(as.matrix(returns) %*% w)
  
  PCA(df, scale.unit = scale., quanti.sup = ncol(df), ncp = ncol(returns), graph = FALSE)
}
