library(tidyverse)
library(scales)
library(patchwork) # For combining plots cleanly

# 1. Custom institutional ggplot theme
theme_quant <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.2), margin = margin(b = 4)),
      plot.subtitle    = element_text(color = "grey35", size = rel(0.95), margin = margin(b = 12)),
      plot.caption     = element_text(color = "grey50", size = rel(0.8), hjust = 0, margin = margin(t = 10)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
      legend.position  = "top",
      legend.title     = element_blank(),
      legend.text      = element_text(size = rel(0.9)),
      strip.text       = element_text(face = "bold", size = rel(0.95)),
      axis.title       = element_text(color = "grey25", size = rel(0.9))
    )
}

# 2. Functional helper to add macro regime shading to any time-series plot
add_regime_shading <- function() {
  list(
    annotate("rect", xmin = as.Date("2020-02-20"), xmax = as.Date("2020-05-31"),
             ymin = -Inf, ymax = Inf, fill = "#D95F02", alpha = 0.12),
    annotate("text", x = as.Date("2020-04-10"), y = Inf, label = "COVID Shock",
             vjust = 1.5, size = 3, fontface = "italic", color = "#D95F02"),
    annotate("rect", xmin = as.Date("2022-03-01"), xmax = as.Date("2023-07-31"),
             ymin = -Inf, ymax = Inf, fill = "#7570B3", alpha = 0.08),
    annotate("text", x = as.Date("2022-11-15"), y = Inf, label = "Global Rate Hiking Cycle",
             vjust = 1.5, size = 3, fontface = "italic", color = "#7570B3")
  )
}