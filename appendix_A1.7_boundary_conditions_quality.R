## Heatmaps: Profit, Service Quality, and Downside Risk vs. Rule-Based ----------
## Positive profit/service differences are better than rule-based.
## Negative downside-risk differences are better than rule-based.

## 1. Packages ----------------------------------------------------------------
packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "scales", "gridExtra")

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

## 3. Variable preparation ----------------------------------------------------

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

## 4. Build downside-risk construct -------------------------------------------
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

## 5. Matched comparison against rule-based ------------------------------------

architectures_to_compare <- c(
  "centralized",
  "independent",
  "sequential",
  "supervised"
)

build_comparison <- function(focal_arch) {
  
  wide_data <- run_metrics %>%
    filter(architecture %in% c("rule_based", focal_arch)) %>%
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
    architecture = focal_arch,
    demand_volatility = wide_data$demand_volatility,
    market_noise = wide_data$market_noise,
    replication = wide_data$replication,
    
    profit_diff_mio =
      wide_data[[paste0("total_profit_mio_", focal_arch)]] -
      wide_data$total_profit_mio_rule_based,
    
    service_diff =
      wide_data[[paste0("avg_service_level_", focal_arch)]] -
      wide_data$avg_service_level_rule_based,
    
    downside_risk_diff =
      wide_data[[paste0("downside_risk_zero_", focal_arch)]] -
      wide_data$downside_risk_zero_rule_based
  )
}

comparison <- bind_rows(
  lapply(architectures_to_compare, build_comparison)
)

## 6. Average differences by environment ---------------------------------------

environment_comparison <- comparison %>%
  group_by(architecture, demand_volatility, market_noise) %>%
  summarise(
    n = n(),
    mean_profit_diff_mio = mean(profit_diff_mio, na.rm = TRUE),
    mean_service_diff = mean(service_diff, na.rm = TRUE),
    mean_downside_risk_diff = mean(downside_risk_diff, na.rm = TRUE),
    .groups = "drop"
  )

environment_comparison

## 7. Paper-ready architecture labels ------------------------------------------

architecture_labels <- c(
  "centralized" = "Centralized",
  "independent" = "Independent",
  "sequential" = "Sequential",
  "supervised" = "Supervised"
)

environment_comparison$architecture <- factor(
  environment_comparison$architecture,
  levels = names(architecture_labels),
  labels = architecture_labels
)

## 8. Heatmap function ---------------------------------------------------------

make_heatmap <- function(data, value_var, title, fill_title, midpoint = 0) {
  
  ggplot(
    data,
    aes(
      x = market_noise,
      y = demand_volatility,
      fill = .data[[value_var]]
    )
  ) +
    geom_tile(color = "white") +
    geom_text(
      aes(label = sprintf("%.2f", .data[[value_var]])),
      color = "black",
      size = 4
    ) +
    facet_wrap(~ architecture) +
    scale_fill_gradient2(
      low = "#B2182B",
      mid = "white",
      high = "#2166AC",
      midpoint = midpoint
    ) +
    labs(
      title = title,
      x = "Market Noise",
      y = "Demand Volatility",
      fill = fill_title
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5),
      strip.background = element_rect(fill = "white", color = "black"),
      strip.text = element_text(face = "bold")
    )
}

## 9. Heatmap 1: Profit difference --------------------------------------------
## Positive values mean the architecture has higher profit than rule-based.

plot_profit_diff <- make_heatmap(
  environment_comparison,
  value_var = "mean_profit_diff_mio",
  title = "Mean Profit Difference Relative to Rule-Based",
  fill_title = "Profit diff.\n(Mio. EUR)"
)

plot_profit_diff

## 10. Heatmap 2: Service-quality difference ----------------------------------
## Positive values mean the architecture has higher service quality than rule-based.

plot_service_diff <- make_heatmap(
  environment_comparison,
  value_var = "mean_service_diff",
  title = "Mean Service-Level Difference Relative to Rule-Based",
  fill_title = "Service-level\ndiff."
)

plot_service_diff

