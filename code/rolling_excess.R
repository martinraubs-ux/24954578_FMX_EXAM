# Rolling annualised excess return of every fund vs the benchmark
rolling_excess <- function(df, window) {
  ann <- function(x) prod(1 + x)^(12 / length(x)) - 1
  rolled <- df %>% arrange(name, date) %>% group_by(name) %>%
    mutate(roll = slide_dbl(ret, ann, .before = window - 1, .complete = TRUE)) %>% ungroup()
  b <- rolled %>% filter(name == bench) %>% select(date, bench_roll = roll)
  rolled %>% filter(name != bench) %>% left_join(b, by = "date") %>%
    mutate(excess = roll - bench_roll) %>% drop_na(excess)
}