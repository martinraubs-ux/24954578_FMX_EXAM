
calendar_ranks <- function(df, bench = "Capped SWIX") {
  df %>%
    filter(name != .env$bench) %>%
    mutate(year = year(date)) %>%
    group_by(name, role, year) %>%
    summarise(n = n(), ret = prod(1 + ret) - 1, .groups = "drop") %>%
    filter(n == 12) %>%
    group_by(year) %>%
    mutate(
      pct = percent_rank(ret),
      rank = min_rank(desc(ret)),
      n_funds = n()
    ) %>%
    ungroup()
}