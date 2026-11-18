# ---- 4. Relative performance over time --------------------------------------
# Calculates rolling excess returns
rolling_excess <- function(df, bench, window) {
  ann <- function(x) prod(1 + x)^(12 / length(x)) - 1
  rolled <- df %>% arrange(name, date) %>% group_by(name) %>%
    mutate(roll = slide_dbl(ret, ann, .before = window - 1, .complete = TRUE)) %>%
    ungroup()
  b <- rolled %>% filter(name == bench) %>% select(date, bench_roll = roll)
  rolled %>% filter(name != bench) %>% left_join(b, by = "date") %>%
    mutate(excess = roll - bench_roll) %>% drop_na(excess)
}

# Calculate the percentile of peer performance metrics to plot a fan chart
fan_data <- function(ex) {
  ex %>% group_by(date) %>%
    summarise(p10 = quantile(excess[role == "Peer"], .10), p25 = quantile(excess[role == "Peer"], .25),
              med = median(excess[role == "Peer"]),
              p75 = quantile(excess[role == "Peer"], .75), p90 = quantile(excess[role == "Peer"], .90),
              .groups = "drop") %>%
    left_join(ex %>% filter(role == "ABC Fund") %>% select(date, abc = excess), by = "date")
}

plot_fan <- function(fan, window) {
  ggplot(fan, aes(date)) +
    geom_hline(yintercept = 0, colour = pal[["Benchmark"]], linetype = "dashed") +
    geom_ribbon(aes(ymin = p10, ymax = p90), fill = "grey70", alpha = 0.35) +
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = "grey50", alpha = 0.45) +
    geom_line(aes(y = med), colour = "grey30", linetype = "dotted") +
    geom_line(aes(y = abc), colour = pal[["ABC Fund"]], linewidth = 1.2, na.rm = TRUE) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "ABC Fund vs the peer 'fan'",
         subtitle = sprintf("Rolling %d-month annualised return above benchmark. Light band = middle 80%% of peers, dark band = middle 50%%, dotted = peer median", window),
         x = NULL, y = "Annualised excess return")
}

# Rank each stock's 12 month return
calendar_ranks <- function(df, bench) {
  df %>% filter(name != bench) %>% mutate(year = year(date)) %>%
    group_by(name, role, year) %>%
    summarise(n = n(), ret = prod(1 + ret) - 1, .groups = "drop") %>%
    filter(n == 12) %>%                                                 # keeps only full years
    group_by(year) %>%
    mutate(pct = percent_rank(ret),      # percent rank  0 is worst
           rank = min_rank(desc(ret)),   # ordinal rank
           n_funds = n()) %>%            # number of funds with full data for the year
    ungroup()
}

plot_rank_heatmap <- function(ranks) {
  d   <- ranks %>% mutate(name = fct_reorder(name, pct, .fun = mean), year = factor(year))
  abc <- d %>% filter(role == "ABC Fund") %>% mutate(lab = sprintf("%d/%d", rank, n_funds))
  ggplot(d, aes(year, name, fill = pct)) +
    geom_tile(colour = "white") +
    geom_tile(data = abc, fill = NA, colour = "black", linewidth = 0.9) +
    geom_text(data = abc, aes(label = lab), size = 3) +
    scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0.5,
                         labels = percent_format(), name = "Percentile rank") +
    labs(title = "Calendar-year ranking: where did each fund finish?",
         subtitle = "Blue = top of the industry, red = bottom. ABC Fund boxed, labelled with rank / number of funds",
         x = NULL, y = NULL)
}
