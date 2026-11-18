# Monthly cross-sectional dispersion (sd, in pp) and each fund's return relative to the median
dispersion_panel <- function(df, min_funds) {
  active <- df %>% filter(name != bench)
  mkt <- active %>% group_by(date) %>%
    summarise(dispersion = sd(ret) * 100, med = median(ret), n = n(), .groups = "drop") %>%
    filter(n >= min_funds)
  active %>% inner_join(mkt %>% select(-n), by = "date") %>% mutate(rel = ret - med)
}