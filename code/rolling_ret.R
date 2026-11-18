rolling_ret <- function(df, years = 1) {
  n <- 252 * years
  df %>% arrange(date) %>% group_by(Index) %>%
    mutate(RollRet = slide_dbl(Return, ~ prod(1 + .x)^(252 / length(.x)) - 1,
                               .before = n - 1, .complete = TRUE)) %>%
    ungroup() %>% mutate(Window = paste0(years, "Y"))
}