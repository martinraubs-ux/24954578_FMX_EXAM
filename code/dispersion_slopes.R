# Regress relative return on dispersion, one regression per fund
dispersion_slopes <- function(panel) {
  panel %>% group_by(name, role) %>% nest() %>%
    mutate(fit  = map(data, ~ lm(rel ~ dispersion, data = .x)),
           tidy = map(fit, ~ tidy(.x, conf.int = TRUE))) %>%
    select(-data, -fit) %>% unnest(tidy) %>% filter(term == "dispersion") %>% ungroup()
}