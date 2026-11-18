avg_pairwise_cor <- function(m) {
  cm <- cor(m, use = "pairwise.complete.obs"); diag(cm) <- NA
  tibble(name = colnames(cm), avg_cor = rowMeans(cm, na.rm = TRUE))
}
