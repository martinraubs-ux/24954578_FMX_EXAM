plot_rolling_beta <- function(beta_df, sample_every_n_months = 6) {
  if (nrow(beta_df) == 0) {
    stop("`beta_df` is empty. Check that `bench_name` matches `Name` exactly, `date` parsed cleanly without NAs, and you have at least 36 months of overlapping data.")
  }
  
  plot_dates <- beta_df %>%
    distinct(date) %>%
    arrange(date) %>%
    slice(seq(1, n(), by = sample_every_n_months)) %>%
    pull(date)
  
  filtered_beta <- beta_df %>%
    filter(date %in% plot_dates) %>%
    mutate(date_label = factor(format(date, "%b %Y"), levels = format(sort(unique(date)), "%b %Y")))
  
  peers_df  <- filtered_beta %>% filter(role == "Peer")
  target_df <- filtered_beta %>% filter(role == "ABC Fund")
  
  ggplot() +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", linewidth = 0.6) +
    geom_boxplot(
      data = peers_df,
      aes(x = date_label, y = rolling_beta),
      fill = "#D9E1E8", color = "#4A607A", alpha = 0.65, outlier.shape = NA, width = 0.5
    ) +
    geom_jitter(
      data = peers_df,
      aes(x = date_label, y = rolling_beta),
      color = "#4A607A", alpha = 0.35, width = 0.12, size = 1.6
    ) +
    geom_line(
      data = target_df,
      aes(x = date_label, y = rolling_beta, group = 1),
      color = "#D95F02", linewidth = 1.2
    ) +
    geom_point(
      data = target_df,
      aes(x = date_label, y = rolling_beta),
      color = "#D95F02", size = 3.2
    ) +
    labs(
      title = "Active Managers Hug the Benchmark — ABC Fund Delivers True Differentiation",
      subtitle = "Rolling 36-month beta vs. CappedSWIX | Peer distribution (box & points) vs. ABC Fund (orange)",
      x = NULL,
      y = "3-Year Rolling Beta",
      caption = "Dashed line = 1.0 (Benchmark Beta). Boxplots show peer 25th-75th percentiles."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}
