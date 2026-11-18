# ---- 2. Rolling beta / R2 / tracking error vs benchmark ----------------------
rolling_stats <- function(df, bench, window) {
  b <- df %>% filter(name == bench) %>% select(date, bench_ret = ret)
  roll <- function(f) function(x, y) slide2_dbl(x, y, f, .before = window - 1, .complete = TRUE)
  
  df %>%
    filter(name != bench) %>% 
    inner_join(b, by = "date") %>%       # assuring equal dates
    arrange(name, date) %>%
    group_by(name) %>%
    mutate(
      beta  = roll(~ cov(.x, .y) / var(.y))(ret, bench_ret),
      r2    = roll(~ cor(.x, .y)^2)(ret, bench_ret),
      te    = roll(~ sd(.x - .y) * sqrt(12))(ret, bench_ret)
    ) %>%
    ungroup()
}

year_end <- function(df) {
  df %>% mutate(year = year(date)) %>%
    group_by(year) %>% filter(date == max(date)) %>% ungroup()
}

latest_summary <- function(df, metric) {
  s <- df %>% drop_na({{ metric }}) %>%                    # Remove rows where metric is missing
    group_by(date) %>% filter(any(role == "ABC Fund")) %>% ungroup() %>%
    filter(date == max(date))
  peers <- s %>% filter(role == "Peer") %>% pull({{ metric }})         # pulls metric for peers
  abc   <- s %>% filter(role == "ABC Fund") %>% pull({{ metric }})     # Pulls metric for ABC
  list(date = unique(s$date), peer_med = median(peers), abc = abc,
       share_peers_above = mean(peers > abc))
}

# Boxplots (peer cross-section at each year-end) + jittered peers + ABC overlay
plot_box_overlay <- function(df, metric, ylab, title, subtitle, hline = NULL, y_labels = waiver()) {
  snap  <- df %>% drop_na({{ metric }}) %>% year_end() %>% mutate(year = factor(year))
  peers <- filter(snap, role == "Peer")
  abc   <- filter(snap, role == "ABC Fund")
  
  ggplot(peers, aes(year, {{ metric }})) +
    list(if (!is.null(hline)) geom_hline(yintercept = hline, linetype = "dashed", colour = pal[["Benchmark"]])) +
    geom_boxplot(outlier.shape = NA, fill = NA, colour = "grey30", width = 0.6) +
    geom_jitter(colour = pal[["Peer"]], width = 0.15, alpha = 0.7, size = 1.8) +
    geom_line(data = abc, aes(group = 1), colour = pal[["ABC Fund"]], linewidth = 1) +
    geom_point(data = abc, colour = pal[["ABC Fund"]], size = 3.5) +
    scale_y_continuous(labels = y_labels) +
    labs(title = title, subtitle = subtitle, x = NULL, y = ylab)
}
