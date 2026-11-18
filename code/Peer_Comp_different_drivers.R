# ---- 3. Different drivers: residual (market-neutral) correlation ------------

# Calculates average pairwise comparison
avg_pairwise_cor <- function(m) {
  cm <- cor(m, use = "pairwise.complete.obs")
  diag(cm) <- NA
  tibble(name = colnames(cm), avg_cor = rowMeans(cm, na.rm = TRUE))
}


# How do funds move together before and after the benchmark
driver_similarity <- function(df, bench) {
  w      <- to_wide(df)                                                   # makes wide
  b      <- w[[bench]]                                                    # seperates benchmark and funds
  funds  <- w %>% select(-date, -all_of(bench))
  resids <- map_dfc(funds, ~ resid(lm(.x ~ b, na.action = na.exclude)))
  
  inner_join(avg_pairwise_cor(funds), avg_pairwise_cor(resids),           # calcualtes raw and residual return
             by = "name", suffix = c("_raw", "_resid")) %>%               # pairwise correlation
    left_join(distinct(df, name, role), by = "name")
}

plot_driver_similarity <- function(d) {
  long <- d %>%
    mutate(name = fct_reorder(name, avg_cor_resid)) %>%
    pivot_longer(c(avg_cor_raw, avg_cor_resid), names_to = "measure", values_to = "cor") %>%
    mutate(measure = recode(measure, avg_cor_raw = "Raw returns",
                            avg_cor_resid = "After removing market"))
  ggplot(long, aes(cor, name)) +
    geom_line(aes(group = name), colour = "grey80") +
    geom_point(aes(colour = role, shape = measure), size = 2.8) +
    scale_colour_manual(values = pal, guide = "none") +
    scale_shape_manual(values = c("Raw returns" = 1, "After removing market" = 16), name = NULL) +
    labs(title = "How similar is each fund to the rest of the industry?",
         subtitle = "Average correlation with other active funds, before and after stripping out the market",
         x = "Average correlation with other active funds", y = NULL)
}
