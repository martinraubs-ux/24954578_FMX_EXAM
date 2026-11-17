library(tidyverse)
plot_macro_drivers <- function(df, macro_pattern = "SAVI|V2X|MOVE|VIX|EPU|CVIX") {
  
  # Automatically match any macro columns in alpha_master_df
  valid_macros <- df %>%
    select(matches(macro_pattern, ignore.case = TRUE)) %>%
    names()
  
  if (length(valid_macros) == 0) {
    stop("No matching macro columns found. Run names(macro_df) to inspect column names.")
  }
  
  df %>%
    select(date, CSV_weighted_21d, cor_median, all_of(valid_macros)) %>%
    # Pivot macro factors FIRST so missing dates in one series don't drop others
    pivot_longer(cols = all_of(valid_macros), names_to = "Macro_Factor", values_to = "Macro_Val") %>%
    filter(!is.na(Macro_Val), !is.na(CSV_weighted_21d), !is.na(cor_median)) %>%
    # Standardize within each Macro_Factor group
    group_by(Macro_Factor) %>%
    mutate(
      Macro_Z = as.numeric(scale(Macro_Val)),
      CSV_Z   = as.numeric(scale(CSV_weighted_21d)),
      Cor_Z   = as.numeric(scale(cor_median))
    ) %>%
    ungroup() %>%
    pivot_longer(cols = c(CSV_Z, Cor_Z), names_to = "Alpha_Metric", values_to = "Alpha_Z") %>%
    mutate(
      Alpha_Metric = recode(Alpha_Metric,
                            "CSV_Z" = "Weighted CSV (Dispersion)",
                            "Cor_Z" = "Median Pairwise Correlation")
    ) %>%
    ggplot(aes(x = date)) +
    geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
    geom_area(aes(y = Macro_Z, fill = "Macro Volatility Index (Z-Score)"), alpha = 0.25) +
    geom_line(aes(y = Alpha_Z, color = Alpha_Metric), linewidth = 0.75) +
    facet_grid(Macro_Factor ~ Alpha_Metric, scales = "free_y") +
    scale_fill_manual(values = c("Macro Volatility Index (Z-Score)" = "#7F8C8D")) +
    scale_color_manual(values = c("Weighted CSV (Dispersion)"   = "#1B4F72",
                                  "Median Pairwise Correlation" = "#B03A2E")) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(
      title    = "How JSE Alpha Opportunity Responds to Global Macro & FX Volatility",
      subtitle = "Standardized Z-scores comparing Weighted CSV & Rolling Correlation against Macro Volatility Indices",
      x        = NULL,
      y        = "Standardized Z-Score (Std. Dev. from Mean)",
      caption  = "Shaded grey area = Macro Volatility Index. Blue/Red lines = Local Alpha Opportunity metrics."
    ) +
    theme_quant()
}

