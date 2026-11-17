library(tidyverse)
plot_correlation_regimes <- function(df) {
  plot_df <- df %>% filter(!is.na(cor_median))
  
  p_cor <- ggplot(plot_df, aes(x = date)) +
    add_regime_shading() +
    geom_ribbon(aes(ymin = cor_q25, ymax = cor_q75, fill = "25th–75th Percentile Range"), alpha = 0.25) +
    geom_line(aes(y = cor_median, color = "Median Pairwise Correlation"), linewidth = 0.85) +
    geom_hline(yintercept = mean(plot_df$cor_median, na.rm = TRUE), 
               linetype = "dashed", color = "grey40") +
    scale_color_manual(values = c("Median Pairwise Correlation" = "#0E6251")) +
    scale_fill_manual(values = c("25th–75th Percentile Range" = "#48C9B0")) +
    scale_y_continuous(labels = number_format(accuracy = 0.05)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title    = "Time-Varying Inter-Constituent Correlations & Systemic Absorption",
      subtitle = "60-day rolling pairwise correlation distribution (top) vs. PC1 variance absorption ratio (bottom)",
      x        = NULL,
      y        = "Pairwise Correlation"
    ) +
    theme_quant()
  
  p_pca <- ggplot(plot_df, aes(x = date, y = pca_pc1_abs)) +
    add_regime_shading() +
    geom_area(fill = "#5D6D7E", alpha = 0.25) +
    geom_line(color = "#2C3E50", linewidth = 0.8) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      x       = NULL,
      y       = "PC1 Variance Explained",
      caption = "Note: Spikes in PC1 absorption and median correlation indicate macro-driven markets where stock selection alpha is compressed."
    ) +
    theme_quant()
  
  p_cor / p_pca + plot_layout(heights = c(1.3, 1))
}

