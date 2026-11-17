plot_csv_dispersion <- function(df) {
  p1 <- alpha_master_df %>%
    select(date, `Cap-Weighted CSV (J433)` = CSV_weighted_60d, 
           `Equal-Weighted CSV` = CSV_equal_60d) %>%
    pivot_longer(-date, names_to = "Metric", values_to = "CSV") %>%
    filter(!is.na(CSV)) %>%
    ggplot(aes(x = date, y = CSV, color = Metric)) +
    add_regime_shading() +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = c("Cap-Weighted CSV (J433)" = "#1B4F72", 
                                  "Equal-Weighted CSV"      = "#E67E22")) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title    = "JSE Capped SWIX (J433): 120-Day Rolling Return Dispersion",
      subtitle = "Weighted vs. Equal-Weighted Cross-Sectional Volatility (corporate-action outliers >40%/day filtered)",
      x        = NULL,
      y        = "120-Day Avg Daily Dispersion (CSV)"
    ) +
    theme_quant()
}

