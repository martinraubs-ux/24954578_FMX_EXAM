# saving figures
save_fig <- function(plot, name, width = 9, height = 6, dir = "figures") {
  ggsave(filename = file.path(dir, paste0(name, ".png")),
         plot = plot, width = width, height = height, dpi = 300, bg = "white")
  invisible(plot)
}