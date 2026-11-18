#Calculatin alpha share
active_share <- function(cons, bench, idx) {
  bm <- cons %>% filter(Index == bench) %>% select(date, Tickers, Wb = Weight)
  map_dfr(idx, \(i) {
    x <- cons %>% filter(Index == i) %>% select(date, Tickers, W = Weight)
    x %>% full_join(bm, by = c("date", "Tickers")) %>%
      semi_join(x, by = "date") %>%                 # keep only dates the index exists
      mutate(across(c(W, Wb), ~ replace_na(.x, 0))) %>%
      group_by(date) %>% summarise(ActiveShare = 0.5 * sum(abs(W - Wb))) %>%
      mutate(Index = i)
  })
}
