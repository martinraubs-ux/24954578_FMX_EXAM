# Evaluating which Garch model fits best

fit_models <- function(x) {
  models <- 1:4; model.list <- list()
  for (p in models) {
    spec <- ugarchspec(
      variance.model = list(model = c("sGARCH","gjrGARCH","eGARCH","apARCH")[p], garchOrder = c(1, 1)),
      mean.model     = list(armaOrder = c(1, 0), include.mean = TRUE),
      distribution.model = c("norm","snorm","std","sstd","ged","sged","nig","ghyp","jsu")[1])
    model.list[[p]] <- ugarchfit(spec = spec, data = as.numeric(x))
  }
  names(model.list) <- c("sGARCH","gjrGARCH","eGARCH","apARCH")
  fit.mat <- sapply(model.list, infocriteria)
  rownames(fit.mat) <- rownames(infocriteria(model.list[[1]]))
  fit.mat
}