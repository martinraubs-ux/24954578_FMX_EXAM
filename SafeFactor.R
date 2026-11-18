pacman::p_load("tidyverse", "lubridate", "zoo", "broom")

# =============================================================
# 0. Assumed inputs - rename to match your own objects
# =============================================================
# Stock_returns_daily : date, Tickers, Name, Returns       (daily stock returns)
# SafeCore            : date, Tickers, Weight, Sector, Fund (monthly rebalanced weights)
# Factor_Scores       : date, Tickers, Factor, Factor_Value (third-party scores, long)
# Bench_weights       : date, Tickers, Weight               (Capped SWIX J433 constituent weights)

ret_is_log <- FALSE                      # TRUE if Stock_returns_daily$Returns are log returns
lr <- function(x) if (ret_is_log) x else log1p(x)

# Benchmark (J433) daily returns - check the ticker label in your index file:
Bench_returns <-
  Idx_Daily %>%
  filter(Tickers == "J433") %>%
  arrange(date) %>%
  mutate(Mkt = Returns) %>%
  filter(!is.na(Retruns)) %>%
  select(date, Returns)

# Check the third-party factor labels first (needed for the mapping in section 4):
# Factor_Scores %>% distinct(Factor)

# =============================================================
# 1. Third-party scores and their stock universe
# =============================================================
tp_long <-
  Factor_Scores %>%
  mutate(ym = floor_date(date, "month")) %>%     # month key, so month-end dates always match
  select(ym, Tickers, Factor, TP_Score = Factor_Value)

# I standardise my scores over the SAME stocks the third party scores in each month,
# so the comparison is like for like:
univ <- tp_long %>% distinct(ym, Tickers)

# =============================================================
# 2. Daily risk measures: trailing 6-month volatility and beta
# =============================================================
win <- 126   # ~6 months of trading days

roll_fun  <- function(x, w, f) if (length(x) < w) rep(NA_real_, length(x)) else zoo::rollapplyr(x, w, f, fill = NA)
roll_mean <- function(x, w)    if (length(x) < w) rep(NA_real_, length(x)) else zoo::rollmeanr(x, w, fill = NA)

daily <-
  Stock_returns_daily %>%
  select(date, Tickers, Returns) %>%
  filter(!is.na(Returns)) %>%
  left_join(Bench_returns, by = "date") %>%
  arrange(Tickers, date)

risk_daily <-
  daily %>%
  group_by(Tickers) %>%
  mutate(Vol  = roll_fun(Returns, win, sd),
         rm   = roll_mean(Returns, win),
         mm   = roll_mean(Mkt, win),
         rmm  = roll_mean(Returns * Mkt, win),
         m2   = roll_mean(Mkt^2, win),
         Beta = (rmm - rm * mm) / (m2 - mm^2)) %>%      # rolling cov(r, m) / var(m)
  ungroup()

# Keep the last observation of each month:
risk_monthly <-
  risk_daily %>%
  mutate(ym = floor_date(date, "month")) %>%
  group_by(Tickers, ym) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(Tickers, ym, Vol, Beta)

# =============================================================
# 3. Monthly returns, 6-1 momentum and the raw/standardised scores
# =============================================================
ret_monthly_raw <-
  daily %>%
  mutate(ym = floor_date(date, "month")) %>%
  group_by(Tickers, ym) %>%
  summarise(R = exp(sum(lr(Returns))) - 1, .groups = "drop")

# Full monthly grid per stock, so lags / leads line up with calendar months:
grid <-
  ret_monthly_raw %>%
  group_by(Tickers) %>%
  summarise(first = min(ym), last = max(ym)) %>%
  mutate(ym = map2(first, last, ~ seq(.x, .y, by = "month"))) %>%
  select(Tickers, ym) %>%
  unnest(ym)

ret_monthly <-
  grid %>%
  left_join(ret_monthly_raw, by = c("Tickers", "ym")) %>%
  arrange(Tickers, ym) %>%
  group_by(Tickers) %>%
  mutate(Fwd = lead(R),                                  # next month's return (for factor portfolios)
         l   = log1p(R),
         # 6-month return minus the last month = compounded return over months t-5 ... t-1
         Mom = exp(lag(l, 1) + lag(l, 2) + lag(l, 3) + lag(l, 4) + lag(l, 5)) - 1) %>%
  ungroup() %>%
  select(Tickers, ym, R, Fwd, Mom)

# Cross-sectional winsorise (2.5% each tail) then z-score:
winsor_z <- function(x, p = 0.025) {
  if (sum(!is.na(x)) < 10) return(rep(NA_real_, length(x)))
  q <- quantile(x, c(p, 1 - p), na.rm = TRUE)
  x <- pmin(pmax(x, q[1]), q[2])
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Composite = average of component z-scores, re-standardised each month:
add_composite <- function(df, cols, name) {
  df %>%
    mutate(!!name := rowMeans(across(all_of(cols)))) %>%
    group_by(ym) %>%
    mutate(!!name := winsor_z(.data[[name]])) %>%
    ungroup()
}

my_scores <-
  ret_monthly %>%
  left_join(risk_monthly, by = c("Tickers", "ym")) %>%
  semi_join(univ, by = c("ym", "Tickers")) %>%
  group_by(ym) %>%
  mutate(Momentum = winsor_z(Mom),
         LowVol   = winsor_z(-Vol),      # negative: higher score = LOWER volatility
         LowBeta  = winsor_z(-Beta)) %>%
  ungroup() %>%
  add_composite(c("LowVol", "LowBeta"), "LowRisk") %>%
  add_composite(c("Momentum", "LowVol"), "Mom_LowVol")

my_factors <- c("Momentum", "LowVol", "LowBeta", "LowRisk", "Mom_LowVol")

# =============================================================
# 4. Compare my scores with the third party's
# =============================================================
# Set Theirs to the exact labels in Factor_Scores$Factor. Both of my risk measures are
# compared with their low-vol score to see which one it resembles.
factor_map <- tribble(
  ~Mine,       ~Theirs,
  "Momentum",  "Momentum",
  "LowVol",    "Low Volatility",
  "LowBeta",   "Low Volatility"
)

cmp <-
  my_scores %>%
  select(ym, Tickers, all_of(my_factors)) %>%
  pivot_longer(all_of(my_factors), names_to = "Mine", values_to = "My_Score") %>%
  inner_join(factor_map, by = "Mine") %>%
  inner_join(tp_long, by = c("ym", "Tickers", "Theirs" = "Factor")) %>%
  filter(!is.na(My_Score), !is.na(TP_Score))

# (a) Cross-sectional rank correlation through time
rho_t <-
  cmp %>%
  group_by(ym, Mine) %>%
  summarise(rho = cor(My_Score, TP_Score, method = "spearman"), n = n(), .groups = "drop")

rho_t %>%
  group_by(Mine) %>%
  summarise(Mean_rho = mean(rho), Min_rho = min(rho), Max_rho = max(rho), Share_positive = mean(rho > 0))

ggplot(rho_t) +
  geom_line(aes(ym, rho, color = Mine)) +
  labs(title = "Rank correlation: my scores vs third-party scores", x = "", y = "Spearman rho") +
  fmxdat::theme_fmx() + fmxdat::fmx_cols()

# (b) Scatterplot (latest month)
cmp %>%
  filter(ym == max(ym)) %>%
  ggplot(aes(TP_Score, My_Score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Mine) +
  labs(title = "My score vs third-party score (latest month)", x = "Third-party", y = "Mine") +
  fmxdat::theme_fmx()

# (c) Top / bottom quintile overlap (chance level = 20%)
overlap <-
  cmp %>%
  group_by(ym, Mine) %>%
  mutate(q_mine = ntile(My_Score, 5), q_tp = ntile(TP_Score, 5)) %>%
  summarise(Top    = sum(q_mine == 5 & q_tp == 5) / sum(q_mine == 5),
            Bottom = sum(q_mine == 1 & q_tp == 1) / sum(q_mine == 1),
            .groups = "drop")

overlap %>% group_by(Mine) %>% summarise(across(c(Top, Bottom), mean))

# =============================================================
# 5. Fund and benchmark exposure: weighted-average score each month
# =============================================================
fund_w  <- SafeCore      %>% mutate(ym = floor_date(date, "month")) %>% select(ym, Tickers, Weight)
bench_w <- Bench_weights %>% mutate(ym = floor_date(date, "month")) %>% select(ym, Tickers, Weight)

s_mine <-
  my_scores %>%
  select(ym, Tickers, all_of(my_factors)) %>%
  pivot_longer(all_of(my_factors), names_to = "Factor", values_to = "Score")

s_tp <- tp_long %>% rename(Score = TP_Score)

expo <- function(w, s) {
  tot_w <- w %>% group_by(ym) %>% summarise(Tot = sum(Weight))
  w %>%
    inner_join(s, by = c("ym", "Tickers")) %>%
    filter(!is.na(Score)) %>%
    group_by(ym, Factor) %>%
    summarise(Exposure = sum(Weight * Score) / sum(Weight), Matched = sum(Weight), .groups = "drop") %>%
    left_join(tot_w, by = "ym") %>%
    mutate(Coverage = Matched / Tot) %>%     # share of the portfolio that has a score
    select(-Matched, -Tot)
}

expo_all <-
  list(Fund_Mine = expo(fund_w, s_mine), Bench_Mine = expo(bench_w, s_mine),
       Fund_TP   = expo(fund_w, s_tp),   Bench_TP   = expo(bench_w, s_tp)) %>%
  bind_rows(.id = "Series") %>%
  separate(Series, c("Portfolio", "Source"), sep = "_")

# Active exposure = fund minus the Capped SWIX
active <-
  expo_all %>%
  select(ym, Factor, Source, Portfolio, Exposure) %>%
  pivot_wider(names_from = Portfolio, values_from = Exposure) %>%
  mutate(Active = Fund - Bench)

active %>%
  filter(Factor %in% c("Momentum", "LowVol", "LowBeta", "Low Volatility")) %>%
  ggplot() +
  geom_line(aes(ym, Active, color = Source)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~Factor) +
  labs(title = "SafeCore active factor exposure vs Capped SWIX", x = "", y = "Weighted-average z-score") +
  fmxdat::theme_fmx() + fmxdat::fmx_cols()

# =============================================================
# 6. Returns-based exposure: regress fund returns on factor-mimicking portfolios
# =============================================================
# Long-short quintile portfolios, formed at the end of month t and held over month t+1:
make_fmp <- function(df, factors, n_q = 5) {
  map_dfr(factors, function(f) {
    df %>%
      filter(!is.na(.data[[f]]), !is.na(Fwd)) %>%
      group_by(ym) %>%
      mutate(q = ntile(.data[[f]], n_q)) %>%
      summarise(Ret = mean(Fwd[q == n_q]) - mean(Fwd[q == 1]), .groups = "drop") %>%
      mutate(Factor = f)
  }) %>%
    mutate(ym = ym %m+% months(1))     # the return is earned the month AFTER formation
}

fmp_mine <- make_fmp(my_scores, c("Momentum", "LowVol", "LowBeta"))

# Fund return in month t+1 = weights set at t x stock returns in t+1
fund_ret <-
  fund_w %>%
  inner_join(ret_monthly %>% select(Tickers, ym, Fwd), by = c("Tickers", "ym")) %>%
  filter(!is.na(Fwd)) %>%
  group_by(ym) %>%
  summarise(Fund = sum(Weight * Fwd) / sum(Weight), .groups = "drop") %>%
  mutate(ym = ym %m+% months(1))

bench_ret <-
  Bench_returns %>%
  mutate(ym = floor_date(date, "month")) %>%
  group_by(ym) %>%
  summarise(Mkt = exp(sum(log1p(Mkt))) - 1, .groups = "drop")

reg_data <-
  fmp_mine %>%
  pivot_wider(names_from = Factor, values_from = Ret) %>%
  inner_join(fund_ret, by = "ym") %>%
  inner_join(bench_ret, by = "ym") %>%
  mutate(Active = Fund - Mkt)

# LowVol and LowBeta are highly correlated - use one at a time, not both:
m_fund   <- lm(Fund ~ Mkt + Momentum + LowVol, data = reg_data)
m_active <- lm(Active ~ Momentum + LowVol, data = reg_data)   # active return vs Capped SWIX

tidy(m_fund);   glance(m_fund)
tidy(m_active); glance(m_active)

# Same exercise with the third party's scores:
# tp_wide <- tp_long %>% mutate(Factor = make.names(Factor)) %>%
#   pivot_wider(names_from = Factor, values_from = TP_Score) %>%
#   left_join(ret_monthly %>% select(Tickers, ym, Fwd), by = c("Tickers", "ym"))
# fmp_tp <- make_fmp(tp_wide, c("Momentum", "Low.Volatility"))   # use your make.names'd labels

# =============================================================
# 7. Link to the risk-concentration results
# =============================================================
# (a) Which PCs are the factors? Regress stock scores on the stocks' correlations with each PC
#     (cross-section, at the latest rebalance; res_fm from FM_PCA_Portfolio()).
# loads <- as_tibble(res_fm$var$cor[, 1:3], rownames = "Tickers")
# names(loads) <- c("Tickers", "PC1", "PC2", "PC3")
# cs <- loads %>% inner_join(my_scores %>% filter(ym == floor_date(w_date, "month")), by = "Tickers")
# summary(lm(LowVol ~ PC1 + PC2 + PC3, data = cs))

# (b) Diversifiers: a stock's risk contribution divided by its weight equals its beta to the
#     portfolio. Below 1 = reduces concentration relative to its size.
# rc$stock_rc %>%
#   mutate(Beta_to_Port = Risk_Contribution / Weight) %>%
#   arrange(Beta_to_Port)

# (c) Risk by sector:
# rc$stock_rc %>%
#   left_join(SafeCore %>% filter(date == w_date) %>% select(Tickers, Sector), by = "Tickers") %>%
#   group_by(Sector) %>%
#   summarise(Weight = sum(Weight), Risk_Contribution = sum(Risk_Contribution)) %>%
#   arrange(desc(Risk_Contribution))