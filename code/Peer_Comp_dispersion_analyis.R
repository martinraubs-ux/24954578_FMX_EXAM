# ---- 5. Dispersion analysis --------------------------------------------------
dispersion_panel <- function(df, bench, min_funds = 5) {
  active <- df %>% filter(name != bench)       # filters out bench
  mkt <- active %>% group_by(date) %>%
    summarise(dispersion = sd(ret) * 100,      # dispersion
              med = median(ret),               # median returns
              n = n(), 
              .groups = "drop") %>%
    filter(n >= min_funds)                    #make sure there are enough funds to actually calc dispersion
  active %>% inner_join(mkt %>% select(-n), by = "date") %>% mutate(rel = ret - med)
}

# section data inthirds
add_buckets <- function(d) {
  mutate(d, bucket = factor(ntile(dispersion, 3), labels = c("Low", "Medium", "High")))
}

bucket_summary <- function(d) {
  d %>% group_by(bucket) %>%
    summarise(avg_rel = mean(rel),
              hit = mean(rel > 0),
              n = n(),
              .groups = "drop")
}

plot_dispersion_scatter <- function(d) {
  ggplot(d, aes(dispersion, rel)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_point(colour = pal[["ABC Fund"]], alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", colour = "black", fill = "grey80") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "Monthly relative return vs dispersion", x = "Cross-sectional dispersion of fund returns (pp)",
         y = "ABC Fund minus peer median")
}

plot_dispersion_buckets <- function(d) {
  s <- bucket_summary(d)
  ggplot(s, aes(bucket, avg_rel)) +
    geom_col(fill = pal[["ABC Fund"]], width = 0.6) +
    geom_hline(yintercept = 0) +
    geom_text(aes(label = paste0("Beats median\nin ", percent(hit, 1), " of months")),
              vjust = -0.3, size = 3.2) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1), expand = expansion(mult = c(0.05, 0.25))) +
    labs(title = "By dispersion tercile", x = "Dispersion regime", y = "Average monthly outperformance")
}

dispersion_slopes <- function(panel) {
  panel %>% group_by(name, role) %>% nest() %>%
    mutate(fit = map(data, ~ lm(rel ~ dispersion, data = .x)),
           tidy = map(fit, ~ tidy(.x, conf.int = TRUE))) %>%
    select(-data, -fit) %>% unnest(tidy) %>%
    filter(term == "dispersion") %>% ungroup()
}

plot_slopes <- function(slopes) {
  slopes %>% mutate(name = fct_reorder(name, estimate)) %>%
    ggplot(aes(estimate, name, colour = role)) +
    geom_vline(xintercept = 0, colour = "grey50") +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.35) +
    scale_colour_manual(values = pal, guide = "none") +
    scale_x_continuous(labels = percent_format(accuracy = 0.01)) +
    labs(title = "Who benefits most when dispersion rises?",
         subtitle = "Change in monthly return vs peer median for each +1pp of dispersion (95% CI)",
         x = "Sensitivity to dispersion", y = NULL)
}
