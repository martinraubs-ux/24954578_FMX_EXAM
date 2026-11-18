# Constituent volatility
# calcualates volatility metric for constitunets per index
constituent_vol <- function(panel, window = 60) {
  panel %>% arrange(Tickers, date) %>%
    group_by(Index, Tickers) %>%
    mutate(Vol = slide_dbl(Returns, sd, .before = window - 1, .complete = TRUE) * sqrt(252)) %>%
    group_by(Index, date) %>%
    summarise(MedianVol = median(Vol, na.rm = TRUE),
              WtdVol    = weighted.mean(Vol, Weight, na.rm = TRUE), .groups = "drop")
}