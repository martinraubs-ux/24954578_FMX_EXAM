library(tidyverse)
library(slider)       
library(lubridate)
library(PerformanceAnalytics)


#  PURE FUNCTION: ROLLING INTER-CONSTITUENT CORRELATIONS & PCA ABSORPTION
calc_rolling_correlations <- function(const_df, window = 60, start_date = "2019-10-01") {
  
  # 1. Clean, coerce to numeric, and deduplicate (date, Tickers) before pivoting
  wide_ret <- const_df %>%
    mutate(
      date   = as.Date(date),
      Return = as.numeric(Return)
    ) %>%
    filter(date >= as.Date(start_date), !is.na(Tickers), !is.na(Return)) %>%
    group_by(date, Tickers) %>%
    summarise(Return = mean(Return, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Tickers, values_from = Return) %>%
    arrange(date)
  
  dates_vec <- wide_ret$date
  mat_ret   <- wide_ret %>%
    select(-date) %>%
    as.matrix() # Force strict numeric matrix
  
  # 2. Functional rolling window over row indices
  slide_dfr(
    .x = seq_len(nrow(mat_ret)),
    .f = function(idx) {
      if (idx < window) return(NULL)
      
      sub_mat <- mat_ret[(idx - window + 1):idx, , drop = FALSE]
      
      # Keep only stocks with complete data AND non-zero variance in this window
      valid_cols <- colSums(is.na(sub_mat)) == 0 & apply(sub_mat, 2, sd) > 0
      sub_mat    <- sub_mat[, valid_cols, drop = FALSE]
      
      if (ncol(sub_mat) < 5) return(NULL)
      
      # Pairwise Correlation Distribution
      cor_mat   <- cor(sub_mat)
      upper_tri <- cor_mat[upper.tri(cor_mat)]
      
      # PCA Absorption Ratio (% Variance explained by PC1)
      pca_fit   <- prcomp(sub_mat, center = TRUE, scale. = TRUE)
      pc1_var   <- (pca_fit$sdev[1]^2) / sum(pca_fit$sdev^2)
      
      tibble(
        date        = dates_vec[idx],
        cor_median  = median(upper_tri, na.rm = TRUE),
        cor_mean    = mean(upper_tri, na.rm = TRUE),
        cor_q25     = quantile(upper_tri, 0.25, na.rm = TRUE),
        cor_q75     = quantile(upper_tri, 0.75, na.rm = TRUE),
        pca_pc1_abs = pc1_var
      )
    }
  ) %>%
    filter(date >= as.Date("2020-01-01"))
}