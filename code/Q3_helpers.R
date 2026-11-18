# Q3 helpers
load_returns <- function(path) {
  read_rds(path) %>%
    rename(
      name = Name,
      date = date,
      ret  = returns
    ) %>%
    mutate(date = ceiling_date(as.Date(date), "month", change_on_boundary = TRUE) - days(1)) %>%
    drop_na() %>%
    arrange(name, date)
}

add_role <- function(df) {
  mutate(df, role = case_when(name == bench ~ "Benchmark",
                              name == fund ~ "ABC Fund",
                              TRUE ~ "Peer"))
}

anonymise_peers <- function(df) {
  peers  <- sort(unique(df$name[df$role == "Peer"]))
  lookup <- set_names(sprintf("Peer %02d", seq_along(peers)), peers)
  mutate(df, name = coalesce(unname(lookup[name]), name))
}

to_wide <- function(df) df %>%
  select(date, name, ret) %>% 
  pivot_wider(names_from = name, values_from = ret)


year_end <- function(df) {
  df %>% mutate(year = year(date)) %>%
    group_by(year) %>%
    filter(date == max(date)) %>%
    ungroup()
}



