calc_rolling_beta <- function(clean_df, bench_name = "Capped SWIX", window_months = 36) {
  bench_df <- clean_df %>%
    filter(Name == bench_name) %>%
    select(date, bench_ret = Returns)
  
  clean_df %>%
    filter(Name != bench_name) %>%
    inner_join(bench_df, by = "date") %>%
    group_by(Name, entity_type) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      n_obs = slide_dbl(Returns, length, .before = window_months - 1, .complete = TRUE),
      rolling_beta = slide2_dbl(
        .x = Returns,
        .y = bench_ret,
        .f = ~ cov(.x, .y, use = "complete.obs") / var(.y, na.rm = TRUE),
        .before = window_months - 1,
        .complete = TRUE
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(rolling_beta), n_obs == window_months)
}