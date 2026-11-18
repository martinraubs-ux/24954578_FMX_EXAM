# calculating stock concentration metrics

concentration <- function(cons) {
  cons %>% group_by(Index, date) %>%
    summarise(N     = n(),
              HHI   = sum(Weight^2),
              EffN  = 1 / HHI,                        # effective number of stocks
              Top5  = sum(sort(Weight, decreasing = TRUE)[1:5]),
              Top10 = sum(sort(Weight, decreasing = TRUE)[1:10]),
              .groups = "drop")
}