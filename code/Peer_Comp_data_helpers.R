# ---- 1. Data helpers ---------------------------------------------------------

# Assures data is right type and structure
load_returns <- function(path) {
  read_rds(path) %>%
    rename_with(str_to_lower) %>%
    rename_with(~ "returns", any_of("monthly_returns")) %>%
    transmute(date = as.Date(date),
              name = str_trim(as.character(name)),
              ret  = as.numeric(returns)) %>%
    drop_na() %>%
    arrange(name, date)
}

# Useful for automating bench name and specific fund naming
add_role <- function(df, cfg) {
  mutate(df, role = case_when(name == cfg$bench ~ "Benchmark",
                              name == cfg$fund  ~ "ABC Fund",
                              TRUE              ~ "Peer"))
}

## Not needed 
## Already annonomised
anonymise_peers <- function(df) {
  peers  <- sort(unique(df$name[df$role == "Peer"]))
  lookup <- set_names(sprintf("Peer %02d", seq_along(peers)), peers)
  mutate(df, name = coalesce(unname(lookup[name]), name))
}

maybe_anonymise <- function(df, cfg) if (cfg$anonymise_peers) anonymise_peers(df) else df

# Make wide
to_wide <- function(df) {
  df %>% select(date, name, ret) %>% pivot_wider(names_from = name, values_from = ret)
}