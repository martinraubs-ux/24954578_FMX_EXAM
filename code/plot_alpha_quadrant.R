library(tidyverse)
plot_alpha_quadrant <- function(df) {
  quad_df <- p3 <- alpha_master_df %>%
    filter(!is.na(CSV_weighted_60d), !is.na(cor_median), date >= as.Date("2021-01-01")) %>%
    mutate(Year = factor(year(date))) %>%
    {
      med_cor <- median(.$cor_median, na.rm = TRUE)
      med_csv <- median(.$CSV_weighted_60d, na.rm = TRUE)
      
      ggplot(., aes(x = cor_median, y = CSV_weighted_60d, color = Year)) +
        geom_vline(xintercept = med_cor, linetype = "dashed", color = "grey45") +
        geom_hline(yintercept = med_csv, linetype = "dashed", color = "grey45") +
        geom_point(alpha = 0.6, size = 1.8) +
        annotate("label", x = min(.$cor_median), y = max(.$CSV_weighted_60d),
                 label = "PEAK ALPHA OPPORTUNITY\n(High Dispersion + Low Correlation)",
                 hjust = 0, vjust = 1, size = 3, fontface = "bold", fill = "#EAFAF1", color = "#1E8449") +
        annotate("label", x = max(.$cor_median), y = max(.$CSV_weighted_60d),
                 label = "MACRO DIVERGENCE / STRESS\n(High Dispersion + High Correlation)",
                 hjust = 1, vjust = 1, size = 3, fontface = "bold", fill = "#FEF9E7", color = "#9A7D0A") +
        annotate("label", x = min(.$cor_median), y = min(.$CSV_weighted_60d),
                 label = "QUIET IDIOSYNCRATIC\n(Low Dispersion + Low Correlation)",
                 hjust = 0, vjust = 0, size = 3, fill = "#F4F6F7", color = "grey30") +
        annotate("label", x = max(.$cor_median), y = min(.$CSV_weighted_60d),
                 label = "LOW ALPHA GRIND\n(Low Dispersion + High Correlation)",
                 hjust = 1, vjust = 0, size = 3, fill = "#FDEDEC", color = "#922B21") +
        scale_color_viridis_d(option = "turbo") +
        scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
        labs(
          title    = "JSE Alpha Opportunity Regime Matrix (2021–2026)",
          subtitle = "60-day Weighted CSV vs. 60-day Median Pairwise Correlation (2020 distortion excluded for clarity)",
          x        = "60-Day Rolling Median Inter-Constituent Correlation",
          y        = "60-Day Weighted Cross-Sectional Volatility (CSV)"
        ) +
        theme_quant()
    }
}

