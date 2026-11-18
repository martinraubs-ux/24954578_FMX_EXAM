# ---- 6. Output helpers -------------------------------------------------------
save_plots <- function(plots, dir, width = 9, height = 5) {
  iwalk(plots, ~ ggsave(file.path(dir, paste0(.y, ".png")), .x,
                        width = width, height = height, dpi = 200, bg = "white"))
}

# ---- 7. Orchestration --------------------------------------------------------
main <- function(cfg) {
  dir.create(cfg$out_dir, showWarnings = FALSE, recursive = TRUE)
  
  df   <- load_returns(cfg$data_path) %>% add_role(cfg) %>% maybe_anonymise(cfg)
  roll <- rolling_stats(df, cfg$bench, cfg$beta_window)    
  
  b <- latest_summary(roll, beta)
  r <- latest_summary(roll, r2)
  
  drv      <- driver_similarity(df, cfg$bench)
  drv_abc  <- drv %>% filter(role == "ABC Fund")
  drv_peer <- drv %>% filter(role == "Peer")
  
  ex      <- rolling_excess(df, cfg$bench, cfg$perf_window)
  fan     <- fan_data(ex)
  win_pct <- fan %>% drop_na(abc) %>% summarise(v = mean(abc > med)) %>% pull(v)
  ranks   <- calendar_ranks(df, cfg$bench)
  abc_rk  <- ranks %>% filter(role == "ABC Fund")
  
  panel      <- dispersion_panel(df, cfg$bench, cfg$min_funds)
  abc_panel  <- panel %>% filter(role == "ABC Fund") %>% add_buckets()
  bsum       <- bucket_summary(abc_panel)
  slopes     <- dispersion_slopes(panel)
  abc_slope  <- slopes %>% filter(role == "ABC Fund")
  slope_rank <- slopes %>% mutate(rk = min_rank(desc(estimate))) %>%
    filter(role == "ABC Fund") %>% pull(rk)
  
  plots <- list(
    `1_beta_boxplot` = plot_box_overlay(
      roll, beta, "Rolling 36m beta vs CappedSWIX",
      "Typical peer beta is close to the market's",
      "Boxes = middle 50% of peers at each year-end, grey dots = individual peers, orange = ABC Fund",
      hline = 1),
    `2_r2_boxplot` = plot_box_overlay(
      roll, r2, "Rolling 36m R-squared vs CappedSWIX",
      "Share of return variability explained by the benchmark",
      "Lower = more of the fund's return pattern comes from something other than the market",
      y_labels = percent_format(accuracy = 1)),
    `3_driver_similarity` = plot_driver_similarity(drv),
    `4_fan_chart`         = plot_fan(fan, cfg$perf_window),
    `5_rank_heatmap`      = plot_rank_heatmap(ranks),
    `6_dispersion` = plot_dispersion_scatter(abc_panel) + plot_dispersion_buckets(abc_panel) +
      plot_layout(widths = c(1.3, 1)),
    `7_dispersion_slopes` = plot_slopes(slopes)
  )
  
  notes <- c(
    beta = sprintf("At %s the median peer's 36m beta was %.2f vs %.2f for ABC Fund; %s of peers had a higher beta.",
                   format(b$date, "%b %Y"), b$peer_med, b$abc, percent(b$share_peers_above, 1)),
    r2   = sprintf("At %s the market explained %s of the median peer's variability vs %s for ABC Fund; %s of peers were more market-driven.",
                   format(r$date, "%b %Y"), percent(r$peer_med, 1), percent(r$abc, 1), percent(r$share_peers_above, 1)),
    drivers = sprintf("After removing the market, ABC Fund's average correlation with other active funds is %.2f vs a peer median of %.2f.",
                      drv_abc$avg_cor_resid, median(drv_peer$avg_cor_resid)),
    fan  = sprintf("ABC Fund's rolling %d-month excess return was above the peer median in %s of observations.",
                   cfg$perf_window, percent(win_pct, 1)),
    ranks = sprintf("ABC Fund finished in the top quartile in %d of %d full calendar years.",
                    sum(abc_rk$pct >= 0.75), nrow(abc_rk)),
    dispersion = sprintf("High-dispersion months: ABC beat the peer median by %s a month vs %s in low-dispersion months (slope p-value %.3f).",
                         percent(bsum$avg_rel[bsum$bucket == "High"], 0.01),
                         percent(bsum$avg_rel[bsum$bucket == "Low"], 0.01), abc_slope$p.value),
    slopes = sprintf("ABC Fund ranks %d of %d funds on sensitivity to dispersion.", slope_rank, nrow(slopes))
  )
  
  save_plots(plots, cfg$out_dir)
  walk(notes, message)   # talking points printed to console
  message("Charts saved to ", cfg$out_dir)
  
  invisible(list(plots = plots, notes = notes, roll = roll, drivers = drv, fan = fan,
                 ranks = ranks, bucket_summary = bsum, slopes = slopes))
}