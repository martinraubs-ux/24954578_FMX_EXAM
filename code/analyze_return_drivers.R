analyze_return_drivers <- function(clean_df) {
  # Wide matrix of complete dates across funds
  wide_ret <- clean_df %>%
    select(date, Name, Returns) %>%
    pivot_wider(names_from = Name, values_from = Returns) %>%
    drop_na()
  
  peer_names <- clean_df %>%
    filter(entity_type == "Peer") %>%
    distinct(Name) %>%
    pull(Name)
  
  # Peer Equal-Weighted Index to measure "Crowd Correlation / R-squared"
  peer_index <- wide_ret %>%
    transmute(
      date,
      Peer_Consensus = rowMeans(across(all_of(peer_names)), na.rm = TRUE)
    )
  
  r_sq_summary <- wide_ret %>%
    left_join(peer_index, by = "date") %>%
    pivot_longer(-c(date, Peer_Consensus), names_to = "Name", values_to = "Returns") %>%
    group_by(Name) %>%
    summarise(
      r_sq_peer_consensus = cor(Returns, Peer_Consensus)^2,
      .groups = "drop"
    ) %>%
    left_join(clean_df %>% distinct(Name, entity_type), by = "Name")
  
  # PCA fit strictly on Peers to extract the top 2 industry return drivers
  peer_mat <- wide_ret %>% select(all_of(peer_names)) %>% as.matrix()
  pca_fit  <- prcomp(peer_mat, center = TRUE, scale. = TRUE)
  var_exp  <- summary(pca_fit)$importance[2, 1:2] * 100
  
  # Project ALL entities (Peers, Benchmark, ABC Fund) onto the Peer Factor space
  all_scaled <- scale(
    wide_ret %>% select(-date) %>% as.matrix(),
    center = pca_fit$center,
    scale  = pca_fit$scale
  )
  # Correlation of each fund with PC1 and PC2
  pc_scores <- pca_fit$x[, 1:2]
  loadings_df <- cor(all_scaled, pc_scores) %>%
    as_tibble(rownames = "Name") %>%
    rename(PC1 = 2, PC2 = 3) %>%
    left_join(r_sq_summary, by = "Name")
  
  list(loadings = loadings_df, var_exp = var_exp)
}