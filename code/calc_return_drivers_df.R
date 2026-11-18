calc_return_drivers_df <- function(df, min_months = 24) {
  
  clean_df <- df %>%
    mutate(ym = floor_date(as.Date(date), "month"))
  
  abc_window <- clean_df %>%
    filter(Name == "ABC_Fund") %>%
    summarise(start_ym = min(ym), end_ym = max(ym))
  
  bench_df <- clean_df %>%
    filter(Name == "CappedSWIX") %>%
    select(ym, Bench_Ret = Returns) %>%
    distinct(ym, .keep_all = TRUE)
  
  peer_consensus <- clean_df %>%
    filter(!Name %in% c("CappedSWIX", "ABC_Fund")) %>%
    group_by(ym) %>%
    summarise(Peer_Consensus = mean(Returns, na.rm = TRUE), .groups = "drop")
  
  clean_df %>%
    filter(Name != "CappedSWIX", ym >= abc_window$start_ym, ym <= abc_window$end_ym) %>%
    mutate(Group = if_else(Name == "ABC_Fund", "ABC_Fund", "Industry Peers")) %>%
    inner_join(bench_df, by = "ym") %>%
    inner_join(peer_consensus, by = "ym") %>%
    group_by(Name, Group) %>%
    filter(n() >= min_months) %>%
    summarise(
      mkt_r_sq   = cor(Returns, Bench_Ret, use = "complete.obs")^2,
      peer_cor   = cor(Returns, Peer_Consensus, use = "complete.obs"),
      ann_excess = (prod(1 + Returns) / prod(1 + Bench_Ret))^(12 / n()) - 1,
      .groups    = "drop"
    )
}