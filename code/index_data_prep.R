# Takes constituents data and wrangles 
prep_constituents <- function(df) {
  df %>%
    transmute(date, Tickers, Index = Fund, Sector, Weight) %>%
    filter(!is.na(Weight), Weight > 0) %>%
    group_by(Index, date) %>%
    mutate(Weight = Weight / sum(Weight)) %>%   # weights sum to 1 in each index-month
    ungroup()
}


# Takes a index constituent data set and seperate daily rets dataset and creats a useabl3
# dataset with both srtock returns and weights
build_stock_panel <- function(cons, daily) {
  cons %>%
    mutate(ym = floor_date(date, "month") %m+% months(1)) %>%
    select(-date) %>%
    inner_join(daily %>% mutate(ym = floor_date(date, "month")),
               by = c("Tickers", "ym"), relationship = "many-to-many")
}


# Takes panel form above and makes index returns
build_index_returns <- function(panel, by = c("Index")) {
  panel %>%
    group_by(across(all_of(c(by, "date")))) %>%
    summarise(Return = sum(Weight * Returns, na.rm = TRUE) /
                sum(Weight[!is.na(Returns)]), .groups = "drop")
}

#drop short series
drop_short_series <- function(df, min_days = 500) {
  df %>% group_by(Index, Sector) %>% filter(n() >= min_days) %>% ungroup()
}