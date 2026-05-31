## Period-Level Trajectory Graphs ---------------------------------------------
## Reference/order: Rule-based first
## All plot labels in English
## Legend: bottom-left, slightly lifted

## 1. Packages ----------------------------------------------------------------
packages <- c("readxl", "dplyr", "ggplot2", "scales", "patchwork")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

## 2. Import period-level data -------------------------------------------------
periodlevel <- read_excel(
  "C:/Users/loske/Desktop/POM_Experiment_Data Kopie.xlsx",
  sheet = "Period-Level"
)

## 3. Variable preparation -----------------------------------------------------

period_var <- "period"
profit_var <- "period_profit"
service_var <- "avg_service_level"

periodlevel <- periodlevel %>%
  mutate(
    period = as.integer(.data[[period_var]]),
    period_profit = as.numeric(.data[[profit_var]]),
    service_level = as.numeric(.data[[service_var]]),
    architecture = factor(
      architecture,
      levels = c(
        "rule_based",
        "centralized",
        "independent",
        "sequential",
        "supervised"
      )
    )
  )

if (max(periodlevel$service_level, na.rm = TRUE) > 1) {
  periodlevel$service_level <- periodlevel$service_level / 100
}

architecture_labels <- c(
  "rule_based" = "T1: Rule-Based",
  "centralized" = "T2: Centralized",
  "independent" = "T3: Independent",
  "sequential" = "T4: Sequential",
  "supervised" = "T5: Supervised"
)

architecture_colors <- c(
  "rule_based" = "#7F8C8D",
  "centralized" = "#18A558",
  "independent" = "#F05A28",
  "sequential" = "#F39C12",
  "supervised" = "#2E86DE"
)

## Common theme for all plots
trajectory_theme <- theme_classic(base_size = 12) +
  theme(
    legend.position = c(0.02, 0.12),
    legend.justification = c(0, 0),
    legend.background = element_rect(color = "gray80", fill = "white"),
    plot.title = element_text(hjust = 0.5)
  )

## 4. Aggregate data by architecture and period --------------------------------

weekly_summary <- periodlevel %>%
  group_by(architecture, period) %>%
  summarise(
    mean_profit_k = mean(period_profit / 1000, na.rm = TRUE),
    mean_service_level = mean(service_level, na.rm = TRUE),
    .groups = "drop"
  )

run_cumulative <- periodlevel %>%
  arrange(architecture, demand_volatility, market_noise, replication, period) %>%
  group_by(architecture, demand_volatility, market_noise, replication) %>%
  mutate(
    cumulative_profit = cumsum(period_profit)
  ) %>%
  ungroup()

cumulative_summary <- run_cumulative %>%
  group_by(architecture, period) %>%
  summarise(
    mean_cumulative_profit_m = mean(cumulative_profit / 1000000, na.rm = TRUE),
    .groups = "drop"
  )

## 5. Plot 1: Profit trajectory ------------------------------------------------

plot_profit_trajectory <- ggplot(
  weekly_summary,
  aes(x = period, y = mean_profit_k, color = architecture)
) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray60") +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = architecture_colors,
    labels = architecture_labels,
    breaks = names(architecture_labels),
    drop = FALSE
  ) +
  labs(
    title = "Profit Trajectory over Time",
    x = "Week",
    y = "Mean Profit ($K per Week)",
    color = NULL
  ) +
  trajectory_theme

plot_profit_trajectory

## 6. Plot 2: Cumulative profit ------------------------------------------------

plot_cumulative_profit <- ggplot(
  cumulative_summary,
  aes(x = period, y = mean_cumulative_profit_m, color = architecture)
) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = architecture_colors,
    labels = architecture_labels,
    breaks = names(architecture_labels),
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = function(x) paste0("$", sprintf("%.1f", x), "M")
  ) +
  labs(
    title = "Cumulative Profit over Time",
    x = "Week",
    y = "Cumulative Profit ($M)",
    color = NULL
  ) +
  trajectory_theme

plot_cumulative_profit

## 7. Plot 3: Service-level trajectory -----------------------------------------

plot_service_trajectory <- ggplot(
  weekly_summary,
  aes(x = period, y = mean_service_level, color = architecture)
) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = architecture_colors,
    labels = architecture_labels,
    breaks = names(architecture_labels),
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Service-Level Trajectory over Time",
    x = "Week",
    y = "Mean Service Level",
    color = NULL
  ) +
  trajectory_theme

plot_service_trajectory

## 8. Combine the three plots --------------------------------------------------
## Use gridExtra instead of patchwork to avoid legend-combination errors

if (!require(gridExtra)) {
  install.packages("gridExtra")
  library(gridExtra)
}

empty_plot <- ggplot() + theme_void()

combined_three_plots <- gridExtra::arrangeGrob(
  plot_profit_trajectory,
  plot_cumulative_profit,
  plot_service_trajectory,
  empty_plot,
  ncol = 2
)

grid::grid.newpage()
grid::grid.draw(combined_three_plots)

## 9. Save plots to Desktop ----------------------------------------------------

ggsave(
  "C:/Users/loske/Desktop/Profit_Trajectory.png",
  plot_profit_trajectory,
  width = 7,
  height = 4.5,
  dpi = 300
)

ggsave(
  "C:/Users/loske/Desktop/Cumulative_Profit_Trajectory.png",
  plot_cumulative_profit,
  width = 7,
  height = 4.5,
  dpi = 300
)

ggsave(
  "C:/Users/loske/Desktop/Service_Level_Trajectory.png",
  plot_service_trajectory,
  width = 7,
  height = 4.5,
  dpi = 300
)

ggsave(
  "C:/Users/loske/Desktop/Three_Period_Level_Trajectories.png",
  combined_three_plots,
  width = 12,
  height = 8,
  dpi = 300
)
