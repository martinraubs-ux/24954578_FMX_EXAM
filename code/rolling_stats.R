# Rolling beta, R2 and tracking error of each fund vs the benchmark
rolling_stats <- function(df, window) {
  b <- df %>% filter(name == bench) %>% select(date, bench_ret = ret)
  roll <- function(f) function(x, y) slide2_dbl(x, y, f, .before = window - 1, .complete = TRUE)
  df %>% filter(name != bench) %>% inner_join(b, by = "date") %>%
    arrange(name, date) %>% group_by(name) %>%
    mutate(beta = roll(~ cov(.x, .y) / var(.y))(ret, bench_ret),
           r2   = roll(~ cor(.x, .y)^2)(ret, bench_ret),
           te   = roll(~ sd(.x - .y) * sqrt(12))(ret, bench_ret)) %>%
    ungroup()
}