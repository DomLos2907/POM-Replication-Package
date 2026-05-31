## When Does AI Outperform Rule-Based Heuristics? ------------------------------
## Matched comparison against rule-based architecture

## 1. Packages ----------------------------------------------------------------
packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "robustbase")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

## 2. Import data -------------------------------------------------------------

runlevel <- read_excel(
  "C:/Users/loske/Desktop/POM_RunLevel_Dataset.xlsx"
)

periodlevel <- read_excel(
  "C:/Users/loske/Desktop/POM_Experiment_Data Kopie.xlsx",
  sheet = "Period-Level"
)

## 3. Define AI architecture(s) ------------------------------------------------
ai_architectures <- c("centralized", "independent", "sequential", "supervised")

service_target <- 0.975

## 4. Variable preparation ----------------------------------------------------

runlevel <- runlevel %>%
  mutate(
    total_profit_mio = total_profit / 1000000,
    avg_service_level = as.numeric(avg_service_level),
    architecture = as.character(architecture),
    demand_volatility = factor(demand_volatility),
    market_noise = factor(market_noise),
    replication = factor(replication)
  )

if (max(runlevel$avg_service_level, na.rm = TRUE) > 1) {
  runlevel$avg_service_level <- runlevel$avg_service_level / 100
}

periodlevel <- periodlevel %>%
  mutate(
    period_profit_mio = period_profit / 1000000,
    architecture = as.character(architecture),
    demand_volatility = factor(demand_volatility),
    market_noise = factor(market_noise),
    replication = factor(replication),
    period = as.integer(period)
  )

## 5. Build downside-risk construct -------------------------------------------
## Downside risk = root mean squared shortfall of period profit below zero.

run_downside <- aggregate(
  period_profit_mio ~ architecture + demand_volatility + market_noise + replication,
  data = periodlevel,
  FUN = function(x) {
    sqrt(mean(pmax(0 - x, 0)^2, na.rm = TRUE))
  }
)

names(run_downside)[
  names(run_downside) == "period_profit_mio"
] <- "downside_risk_zero"

run_metrics <- runlevel %>%
  select(
    architecture,
    demand_volatility,
    market_noise,
    replication,
    total_profit_mio,
    avg_service_level
  ) %>%
  left_join(
    run_downside,
    by = c(
      "architecture",
      "demand_volatility",
      "market_noise",
      "replication"
    )
  )

## 6. Matched AI vs. rule-based comparisons -----------------------------------

build_ai_comparison <- function(ai_arch) {
  
  wide_data <- run_metrics %>%
    filter(architecture %in% c("rule_based", ai_arch)) %>%
    pivot_wider(
      id_cols = c(demand_volatility, market_noise, replication),
      names_from = architecture,
      values_from = c(
        total_profit_mio,
        avg_service_level,
        downside_risk_zero
      )
    )
  
  data.frame(
    ai_architecture = ai_arch,
    demand_volatility = wide_data$demand_volatility,
    market_noise = wide_data$market_noise,
    replication = wide_data$replication,
    
    rule_profit_mio = wide_data$total_profit_mio_rule_based,
    ai_profit_mio = wide_data[[paste0("total_profit_mio_", ai_arch)]],
    
    rule_service_level = wide_data$avg_service_level_rule_based,
    ai_service_level = wide_data[[paste0("avg_service_level_", ai_arch)]],
    
    rule_downside_risk = wide_data$downside_risk_zero_rule_based,
    ai_downside_risk = wide_data[[paste0("downside_risk_zero_", ai_arch)]]
  )
}

comparison <- bind_rows(
  lapply(ai_architectures, build_ai_comparison)
) %>%
  mutate(
    profit_diff_mio = ai_profit_mio - rule_profit_mio,
    service_diff = ai_service_level - rule_service_level,
    downside_risk_diff = ai_downside_risk - rule_downside_risk,
    
    ai_higher_profit = profit_diff_mio > 0,
    ai_higher_service = service_diff > 0,
    ai_lower_downside_risk = downside_risk_diff < 0,
    
    ai_meets_service_target = ai_service_level >= service_target,
    rule_meets_service_target = rule_service_level >= service_target,
    
    ai_profit_and_service_win =
      ai_higher_profit & ai_meets_service_target,
    
    ai_profit_and_risk_win =
      ai_higher_profit & ai_lower_downside_risk,
    
    ai_full_win =
      ai_higher_profit & ai_meets_service_target & ai_lower_downside_risk
  )

comparison

## 7. Overall comparison -------------------------------------------------------

overall_comparison <- comparison %>%
  group_by(ai_architecture) %>%
  summarise(
    n = n(),
    
    mean_profit_diff_mio = mean(profit_diff_mio, na.rm = TRUE),
    median_profit_diff_mio = median(profit_diff_mio, na.rm = TRUE),
    profit_win_rate = mean(ai_higher_profit, na.rm = TRUE),
    
    mean_service_diff = mean(service_diff, na.rm = TRUE),
    service_win_rate = mean(ai_higher_service, na.rm = TRUE),
    
    mean_downside_risk_diff = mean(downside_risk_diff, na.rm = TRUE),
    downside_risk_win_rate = mean(ai_lower_downside_risk, na.rm = TRUE),
    
    profit_and_service_win_rate = mean(ai_profit_and_service_win, na.rm = TRUE),
    profit_and_risk_win_rate = mean(ai_profit_and_risk_win, na.rm = TRUE),
    full_win_rate = mean(ai_full_win, na.rm = TRUE),
    
    .groups = "drop"
  )

overall_comparison

## 8. Environmental conditions: demand volatility x market noise ---------------

environment_comparison <- comparison %>%
  group_by(ai_architecture, demand_volatility, market_noise) %>%
  summarise(
    n = n(),
    
    mean_profit_diff_mio = mean(profit_diff_mio, na.rm = TRUE),
    median_profit_diff_mio = median(profit_diff_mio, na.rm = TRUE),
    profit_win_rate = mean(ai_higher_profit, na.rm = TRUE),
    
    mean_service_diff = mean(service_diff, na.rm = TRUE),
    service_win_rate = mean(ai_higher_service, na.rm = TRUE),
    
    mean_downside_risk_diff = mean(downside_risk_diff, na.rm = TRUE),
    downside_risk_win_rate = mean(ai_lower_downside_risk, na.rm = TRUE),
    
    profit_and_service_win_rate = mean(ai_profit_and_service_win, na.rm = TRUE),
    profit_and_risk_win_rate = mean(ai_profit_and_risk_win, na.rm = TRUE),
    full_win_rate = mean(ai_full_win, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(ai_architecture, demand_volatility, market_noise)

environment_comparison

## 9. Statistical tests within each environment --------------------------------
## One-sided tests:
## profit_diff_mio > 0 means AI has higher profit than rule-based.
## service_diff > 0 means AI has higher service level than rule-based.
## downside_risk_diff < 0 means AI has lower downside risk than rule-based.

tests_by_environment <- comparison %>%
  group_by(ai_architecture, demand_volatility, market_noise) %>%
  summarise(
    n = n(),
    
    mean_profit_diff_mio = mean(profit_diff_mio, na.rm = TRUE),
    profit_ttest_p = tryCatch(
      t.test(profit_diff_mio, mu = 0, alternative = "greater")$p.value,
      error = function(e) NA
    ),
    profit_wilcox_p = tryCatch(
      wilcox.test(profit_diff_mio, mu = 0, alternative = "greater", exact = FALSE)$p.value,
      error = function(e) NA
    ),
    
    mean_service_diff = mean(service_diff, na.rm = TRUE),
    service_ttest_p = tryCatch(
      t.test(service_diff, mu = 0, alternative = "greater")$p.value,
      error = function(e) NA
    ),
    service_wilcox_p = tryCatch(
      wilcox.test(service_diff, mu = 0, alternative = "greater", exact = FALSE)$p.value,
      error = function(e) NA
    ),
    
    mean_downside_risk_diff = mean(downside_risk_diff, na.rm = TRUE),
    downside_ttest_p = tryCatch(
      t.test(downside_risk_diff, mu = 0, alternative = "less")$p.value,
      error = function(e) NA
    ),
    downside_wilcox_p = tryCatch(
      wilcox.test(downside_risk_diff, mu = 0, alternative = "less", exact = FALSE)$p.value,
      error = function(e) NA
    ),
    
    .groups = "drop"
  )

tests_by_environment

## 10. Model-based condition tests --------------------------------------------
## These models test whether AI-rule-based differences vary by demand volatility
## and market noise.

for (ai_arch in unique(comparison$ai_architecture)) {
  
  cat("\n\n============================================================\n")
  cat("AI architecture:", ai_arch, "\n")
  cat("============================================================\n")
  
  data_ai <- comparison %>%
    filter(ai_architecture == ai_arch)
  
  ## Profit difference model
  model_profit_diff <- lmrob(
    profit_diff_mio ~ demand_volatility * market_noise,
    data = data_ai,
    setting = "KS2014"
  )
  
  cat("\nProfit difference model: AI profit minus rule-based profit\n")
  print(summary(model_profit_diff))
  
  ## Service difference model
  model_service_diff <- lmrob(
    service_diff ~ demand_volatility * market_noise,
    data = data_ai,
    setting = "KS2014"
  )
  
  cat("\nService difference model: AI service minus rule-based service\n")
  print(summary(model_service_diff))
  
  ## Downside-risk difference model
  model_downside_diff <- lmrob(
    downside_risk_diff ~ demand_volatility * market_noise,
    data = data_ai,
    setting = "KS2014"
  )
  
  cat("\nDownside-risk difference model: AI downside risk minus rule-based downside risk\n")
  print(summary(model_downside_diff))
  
  ## Probability that AI beats rule-based in profit
  model_profit_win <- glm(
    ai_higher_profit ~ demand_volatility * market_noise,
    data = data_ai,
    family = binomial(link = "logit")
  )
  
  cat("\nProfit win probability model\n")
  print(summary(model_profit_win))
}

## 11. Time-window analysis ----------------------------------------------------
## Tests whether AI outperforms rule-based at different horizons.

time_windows <- c(10, 20, 30, 40, 52)

period_cumulative <- periodlevel %>%
  arrange(architecture, demand_volatility, market_noise, replication, period) %>%
  group_by(architecture, demand_volatility, market_noise, replication) %>%
  mutate(
    cumulative_profit_mio = cumsum(period_profit_mio)
  ) %>%
  ungroup() %>%
  filter(period %in% time_windows) %>%
  select(
    architecture,
    demand_volatility,
    market_noise,
    replication,
    period,
    cumulative_profit_mio
  )

build_window_comparison <- function(ai_arch) {
  
  wide_data <- period_cumulative %>%
    filter(architecture %in% c("rule_based", ai_arch)) %>%
    pivot_wider(
      id_cols = c(demand_volatility, market_noise, replication, period),
      names_from = architecture,
      values_from = cumulative_profit_mio
    )
  
  data.frame(
    ai_architecture = ai_arch,
    demand_volatility = wide_data$demand_volatility,
    market_noise = wide_data$market_noise,
    replication = wide_data$replication,
    period = wide_data$period,
    rule_cumulative_profit_mio = wide_data$rule_based,
    ai_cumulative_profit_mio = wide_data[[ai_arch]]
  )
}

window_comparison <- bind_rows(
  lapply(ai_architectures, build_window_comparison)
) %>%
  mutate(
    cumulative_profit_diff_mio =
      ai_cumulative_profit_mio - rule_cumulative_profit_mio,
    ai_cumulative_profit_win =
      cumulative_profit_diff_mio > 0
  )

window_summary <- window_comparison %>%
  group_by(ai_architecture, demand_volatility, market_noise, period) %>%
  summarise(
    n = n(),
    mean_cumulative_profit_diff_mio =
      mean(cumulative_profit_diff_mio, na.rm = TRUE),
    cumulative_profit_win_rate =
      mean(ai_cumulative_profit_win, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ai_architecture, demand_volatility, market_noise, period)

window_summary

## 12. Visualize where AI beats rule-based -------------------------------------

plot_profit_diff_conditions <- ggplot(
  environment_comparison,
  aes(
    x = market_noise,
    y = demand_volatility,
    fill = mean_profit_diff_mio
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = sprintf("%.2f", mean_profit_diff_mio)),
    color = "black",
    size = 4
  ) +
  facet_wrap(~ ai_architecture) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0
  ) +
  labs(
    title = "Mean Profit Difference: AI Architecture vs. Rule-Based",
    x = "Market Noise",
    y = "Demand Volatility",
    fill = "Profit diff.\n(Mio. EUR)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

plot_profit_diff_conditions

ggsave(
  "C:/Users/loske/Desktop/AI_vs_RuleBased_Profit_Difference_Conditions.png",
  plot_profit_diff_conditions,
  width = 8,
  height = 5,
  dpi = 300
)

## 13. Key outputs to inspect --------------------------------------------------

overall_comparison
environment_comparison
tests_by_environment
window_summary
