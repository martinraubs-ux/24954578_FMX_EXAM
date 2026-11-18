# Calculating and plotting sector weights per index 
library(slider)
sector_weights <- function(cons) {
  cons %>% group_by(Index, date, Sector) %>%
    summarise(SectorW = sum(Weight), .groups = "drop")
}

plot_sector_weights <- function(sw, idx, show_y = TRUE) {
  sw %>%
    filter(Index == idx) %>%
    mutate(Sector = factor(Sector, levels = names(sector_cols))) %>%
    ggplot(aes(date, SectorW, fill = Sector)) +
    geom_area(alpha = 0.9, colour = "white", linewidth = 0.1) +
    scale_fill_manual(values = sector_cols, drop = FALSE) +
    scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
    scale_x_date(expand = c(0, 0), date_breaks = "5 years", date_labels = "%Y") +
    labs(title = idx, x = NULL, y = if (show_y) "Weight" else NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 11, hjust = 0),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
}


plot_sector_growth <- function(sector_ret) {
  sector_ret %>%
    mutate(Index = factor(Index, levels = idx_order)) %>%
    arrange(date) %>%
    group_by(Index, Sector) %>%
    mutate(Growth = cumprod(1 + Return)) %>%
    ungroup() %>%
    ggplot(aes(date, Growth, colour = Sector)) +
    geom_line(linewidth = 0.6) +
    facet_wrap(~Index, ncol = 2) +
    scale_colour_manual(values = sector_cols) +
    scale_y_log10(labels = scales::label_number(prefix = "R")) +
    labs(title = "Growth of R1 invested in each sector",
         subtitle = "Sector sub-portfolios within each size index (log scale)",
         x = NULL, y = NULL, colour = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", size = 14),
          strip.text = element_text(face = "bold", hjust = 0))
}

plot_sector_rolling <- function(sector_ret, years = 3) {
  n <- 252 * years
  sector_ret %>%
    mutate(Index = factor(Index, levels = idx_order)) %>%
    arrange(date) %>%
    group_by(Index, Sector) %>%
    filter(n() > n) %>%
    mutate(RollRet = slide::slide_dbl(Return, ~ prod(1 + .x)^(252 / length(.x)) - 1,
                                      .before = n - 1, .complete = TRUE)) %>%
    ungroup() %>% drop_na(RollRet) %>%
    ggplot(aes(date, RollRet, colour = Sector)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_line(linewidth = 0.5) +
    facet_wrap(~Index, ncol = 2) +
    scale_colour_manual(values = sector_cols) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste0("Rolling ", years, "-year annualised return by sector"),
         x = NULL, y = NULL, colour = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold", size = 14),
          strip.text = element_text(face = "bold", hjust = 0))
}