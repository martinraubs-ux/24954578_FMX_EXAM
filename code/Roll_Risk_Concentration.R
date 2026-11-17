pacman::p_load("tidyverse")

# Re-uses PCA_byhand() (and LW_shrink()) from the earlier file:
source("code/PCA_byhand_LW.R")
source("code/Risk_Concentration.R")
#' Rolling risk concentration (time-varying weights)
#'
#' For each rolling window, the fund weights used are those from the most recent
#' rebalance on or before the window's end date, applied to the covariance
#' estimated over that window.
#'
#' @param data_wide  Wide data frame of daily returns (date + one column per stock).
#'                   Include every stock the fund has ever held.
#' @param weights_df Long data frame of fund weights, e.g. SafeCore: date, Tickers, Weight
#'                   (one row per stock per rebalance date).
#' @param name_col   Name of the ticker column in weights_df.
#' @param weight_col Name of the weight column in weights_df.
#' @param lookback   Window length in observations.
#' @param step       Re-estimate every `step` observations.
#' @param n_pcs      Number of PC risk shares to report.
#' @param universe   "held" (default) or "all", as in Risk_Concentration(): "held" re-defines
#'                   the stock universe in each window as the stocks held at that rebalance.
#' Other arguments as in Risk_Concentration().
#'
#' @return Tibble: date, Weights_Date (the rebalance used), Port_Vol, ENB,
#'         PC1_Share, PC2_Share, ...

Roll_Risk_Concentration <- function(data_wide,
                                    weights_df,
                                    name_col = "Tickers",
                                    weight_col = "Weight",
                                    lookback = 250,
                                    step = 5,
                                    n_pcs = 3,
                                    scale. = FALSE,
                                    shrink = "none",
                                    universe = "held",
                                    date_col = "date") {
  
  data_wide <- data_wide %>% arrange(.data[[date_col]])
  dates <- data_wide[[date_col]]
  N <- nrow(data_wide)
  
  if (lookback > N) stop("lookback is longer than the number of observations")
  
  # One named weight vector per rebalance date:
  weights_df <- weights_df %>% select(all_of(c(date_col, name_col, weight_col)))
  reb_dates <- sort(unique(weights_df[[date_col]]))
  
  weights_list <-
    map(reb_dates, function(d) {
      weights_df %>%
        filter(.data[[date_col]] == d) %>%
        select(all_of(c(name_col, weight_col))) %>%
        tibble::deframe()
    })
  
  end_idx <- seq(lookback, N, by = step)
  
  map_dfr(end_idx, function(i) {
    
    d <- dates[i]
    
    # Latest rebalance on or before the window end:
    k <- which(reb_dates <= d)
    if (length(k) == 0) return(NULL)   # fund did not exist yet
    k <- max(k)
    
    rc <- Risk_Concentration(data_wide[(i - lookback + 1):i, ], weights_list[[k]],
                             scale. = scale., shrink = shrink, universe = universe,
                             date_col = date_col, warn = FALSE)
    
    if (is.null(rc)) return(NULL)   # no usable stocks in this window
    
    shares <- setNames(as.list(rc$pc_rc$Share[seq_len(n_pcs)]),
                       paste0("PC", seq_len(n_pcs), "_Share"))
    
    bind_cols(tibble(date = d, Weights_Date = reb_dates[k],
                     Port_Vol = rc$port_vol, ENB = rc$ENB),
              as_tibble(shares))
  })
}