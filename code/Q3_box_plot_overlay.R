# Boxplot of peers at each year-end + jittered peers + ABC overlay
plot_box_overlay <- function(df, metric, title, subtitle, ylab, hline = NULL, y_labels = waiver()) {
  snap  <- df %>% drop_na({{ metric }}) %>% year_end() %>% mutate(year = factor(year))
  peers <- filter(snap, role == "Peer")
  abc   <- filter(snap, role == "ABC Fund")
  ggplot(peers, aes(year, {{ metric }})) +
    list(if (!is.null(hline)) geom_hline(yintercept = hline, linetype = "dashed", colour = pal[["Benchmark"]])) +
    geom_jitter(colour = pal[["Peer"]], width = 0.15, alpha = 0.7, size = 1.8) +
    geom_boxplot(outlier.shape = NA, fill = NA, colour = "grey30", width = 0.6) +
    geom_line(data = abc, aes(group = 1), colour = pal[["ABC Fund"]], linewidth = 1) +
    geom_point(data = abc, colour = pal[["ABC Fund"]], size = 3.5) +
    scale_y_continuous(labels = y_labels) +
    labs(title = title, subtitle = subtitle, x = NULL, y = ylab) + theme_minimal()
}