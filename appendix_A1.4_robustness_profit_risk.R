## Robustness Check: Alternative Downside-Risk Thresholds ----------------------
## Reference category: rule-based architecture

## 1. Packages ----------------------------------------------------------------
library(readxl)
library(logistf)
library(robustbase)

## 2. Import period-level data ------------------------------------------------
project_data_path <- "data/raw/POM_Experiment_Data Kopie.xlsx"
desktop_data_path <- "C:/Users/loske/Desktop/POM_Experiment_Data Kopie.xlsx"

if (file.exists(project_data_path)) {
  periodlevel <- read_excel(
    project_data_path,
    sheet = "Period-Level"
  )
} else {
  periodlevel <- read_excel(
    desktop_data_path,
    sheet = "Period-Level"
  )
}

## 3. Variable preparation ----------------------------------------------------
periodlevel$period_profit_mio <- periodlevel$period_profit / 1000000

periodlevel$architecture <- factor(periodlevel$architecture)
periodlevel$demand_volatility <- factor(periodlevel$demand_volatility)
periodlevel$market_noise <- factor(periodlevel$market_noise)
periodlevel$replication <- factor(periodlevel$replication)

periodlevel$architecture <- relevel(periodlevel$architecture, ref = "rule_based")
periodlevel$demand_volatility <- relevel(periodlevel$demand_volatility, ref = "0.1")
periodlevel$market_noise <- relevel(periodlevel$market_noise, ref = "0.03")

## 4. Define alternative thresholds -------------------------------------------
thresholds <- list(
  "Break-even threshold" = 0,
  "Lower-quartile threshold" = as.numeric(
    quantile(periodlevel$period_profit_mio, 0.25, na.rm = TRUE)
  ),
  "Mean-profit threshold" = mean(
    periodlevel$period_profit_mio,
    na.rm = TRUE
  )
)

thresholds

## 5. Helper function: build downside-risk construct --------------------------

build_downside_data <- function(data, tau) {
  downside_data <- aggregate(
    period_profit_mio ~ architecture + demand_volatility + market_noise + replication,
    data = data,
    FUN = function(x) {
      sqrt(mean(pmax(tau - x, 0)^2, na.rm = TRUE))
    }
  )
  
  names(downside_data)[
    names(downside_data) == "period_profit_mio"
  ] <- "downside_risk"
  
  downside_data$architecture <- factor(downside_data$architecture)
  downside_data$demand_volatility <- factor(downside_data$demand_volatility)
  downside_data$market_noise <- factor(downside_data$market_noise)
  
  downside_data$architecture <- relevel(
    downside_data$architecture,
    ref = "rule_based"
  )
  downside_data$demand_volatility <- relevel(
    downside_data$demand_volatility,
    ref = "0.1"
  )
  downside_data$market_noise <- relevel(
    downside_data$market_noise,
    ref = "0.03"
  )
  
  downside_data$has_downside <- downside_data$downside_risk > 0
  
  downside_data
}

## 6. Estimate two-part models for each threshold -----------------------------

model_results <- list()

for (threshold_name in names(thresholds)) {
  
  tau <- thresholds[[threshold_name]]
  
  downside_data <- build_downside_data(
    data = periodlevel,
    tau = tau
  )
  
  occurrence_model <- logistf(
    has_downside ~ architecture + demand_volatility + market_noise,
    data = downside_data
  )
  
  severity_data <- subset(downside_data, downside_risk > 0)
  severity_data <- droplevels(severity_data)
  
  severity_model <- lmrob(
    log(downside_risk) ~ architecture + demand_volatility + market_noise,
    data = severity_data
  )
  
  model_results[[threshold_name]] <- list(
    tau = tau,
    data = downside_data,
    severity_data = severity_data,
    occurrence_model = occurrence_model,
    severity_model = severity_model
  )
}

## 7. Inspect threshold-specific observations ---------------------------------

threshold_summary <- data.frame(
  threshold = names(model_results),
  tau = sapply(model_results, function(x) x$tau),
  observations_stage_1 = sapply(model_results, function(x) nrow(x$data)),
  observations_stage_2 = sapply(
    model_results,
    function(x) sum(x$data$downside_risk > 0)
  ),
  zero_downside_runs = sapply(
    model_results,
    function(x) sum(x$data$downside_risk == 0)
  )
)

threshold_summary

## 8. Formatting functions ----------------------------------------------------

fmt_num <- function(x) {
  ifelse(
    is.na(x),
    "",
    sprintf("%.2f", x)
  )
}

fmt_p <- function(p) {
  ifelse(
    is.na(p),
    "",
    ifelse(
      p < 0.001,
      "$<$0.001",
      ifelse(p > 0.999, "$>$0.999", sprintf("%.3f", p))
    )
  )
}

## 9. Extract Firth logistic regression results -------------------------------

extract_firth <- function(model) {
  coef_tab <- data.frame(
    term = names(model$coefficients),
    estimate = as.numeric(model$coefficients),
    se = sqrt(diag(model$var)),
    p = as.numeric(model$prob)
  )
  
  coef_tab$statistic <- coef_tab$estimate / coef_tab$se
  
  coef_tab
}

## 10. Extract robust severity model results ----------------------------------

extract_lmrob <- function(model) {
  coef_tab <- as.data.frame(summary(model)$coefficients)
  coef_tab$term <- rownames(coef_tab)
  rownames(coef_tab) <- NULL
  
  data.frame(
    term = coef_tab$term,
    estimate = coef_tab$Estimate,
    se = coef_tab$`Std. Error`,
    statistic = coef_tab$`t value`,
    p = coef_tab$`Pr(>|t|)`
  )
}

## 11. Paper-ready labels -----------------------------------------------------

labels <- c(
  "(Intercept)" = "Intercept",
  "architecturecentralized" = "Centralized architecture",
  "architectureindependent" = "Independent architecture",
  "architecturesequential" = "Sequential architecture",
  "architecturesupervised" = "Supervised architecture",
  "demand_volatility0.25" = "High demand volatility",
  "market_noise0.1" = "High market noise"
)

terms <- names(labels)

get_value <- function(tab, term, column) {
  value <- tab[tab$term == term, column]
  if (length(value) == 0) {
    return("")
  }
  value
}

## 12. LaTeX table: occurrence models -----------------------------------------

occurrence_tabs <- lapply(
  model_results,
  function(x) extract_firth(x$occurrence_model)
)

latex_occurrence <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Robustness Check: Downside-Risk Occurrence Under Alternative Thresholds}",
  "\\label{tab:downside_occurrence_thresholds}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccccccccc}",
  "\\hline\\hline",
  " & \\multicolumn{4}{c}{Break-even threshold} & \\multicolumn{4}{c}{Lower-quartile threshold} & \\multicolumn{4}{c}{Mean-profit threshold} \\\\",
  "\\cline{2-5} \\cline{6-9} \\cline{10-13}",
  "Predictors & Est. & SE & $z$ & $p$ & Est. & SE & $z$ & $p$ & Est. & SE & $z$ & $p$ \\\\",
  "\\hline"
)

for (term in terms) {
  row_values <- c(labels[term])
  
  for (threshold_name in names(occurrence_tabs)) {
    tab <- occurrence_tabs[[threshold_name]]
    
    row_values <- c(
      row_values,
      fmt_num(get_value(tab, term, "estimate")),
      fmt_num(get_value(tab, term, "se")),
      fmt_num(get_value(tab, term, "statistic")),
      fmt_p(get_value(tab, term, "p"))
    )
  }
  
  latex_occurrence <- c(
    latex_occurrence,
    paste(row_values, collapse = " & "),
    "\\\\"
  )
}

latex_occurrence <- c(
  latex_occurrence,
  "\\hline",
  paste0(
    "Observations & \\multicolumn{4}{c}{",
    threshold_summary$observations_stage_1[1],
    "} & \\multicolumn{4}{c}{",
    threshold_summary$observations_stage_1[2],
    "} & \\multicolumn{4}{c}{",
    threshold_summary$observations_stage_1[3],
    "} \\\\"
  ),
  paste0(
    "Threshold value & \\multicolumn{4}{c}{",
    sprintf("%.3f", threshold_summary$tau[1]),
    "} & \\multicolumn{4}{c}{",
    sprintf("%.3f", threshold_summary$tau[2]),
    "} & \\multicolumn{4}{c}{",
    sprintf("%.3f", threshold_summary$tau[3]),
    "} \\\\"
  ),
  paste0(
    "Runs without downside risk & \\multicolumn{4}{c}{",
    threshold_summary$zero_downside_runs[1],
    "} & \\multicolumn{4}{c}{",
    threshold_summary$zero_downside_runs[2],
    "} & \\multicolumn{4}{c}{",
    threshold_summary$zero_downside_runs[3],
    "} \\\\"
  ),
  "\\hline\\hline",
  "\\end{tabular}%",
  "}",
  "\\vspace{0.15cm}",
  "\\begin{minipage}{\\textwidth}",
  "\\footnotesize",
  "\\textbf{Note.} Est. denotes the coefficient estimate, SE denotes the standard error, $z$ denotes the z-statistic, and $p$ denotes the p-value. The dependent variable equals one if the run-level downside-risk measure is greater than zero. Models are estimated using Firth logistic regression. Rule-based architecture, low demand volatility, and low market noise are omitted reference categories. Positive architecture coefficients indicate higher downside-risk occurrence relative to rule-based architecture.",
  "\\end{minipage}",
  "\\end{table}"
)

cat(paste(latex_occurrence, collapse = "\n"))

## 13. LaTeX table: severity models -------------------------------------------

severity_tabs <- lapply(
  model_results,
  function(x) extract_lmrob(x$severity_model)
)

latex_severity <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Robustness Check: Downside-Risk Severity Under Alternative Thresholds}",
  "\\label{tab:downside_severity_thresholds}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccccccccc}",
  "\\hline\\hline",
  " & \\multicolumn{4}{c}{Break-even threshold} & \\multicolumn{4}{c}{Lower-quartile threshold} & \\multicolumn{4}{c}{Mean-profit threshold} \\\\",
  "\\cline{2-5} \\cline{6-9} \\cline{10-13}",
  "Predictors & Est. & SE & $t$ & $p$ & Est. & SE & $t$ & $p$ & Est. & SE & $t$ & $p$ \\\\",
  "\\hline"
)

for (term in terms) {
  row_values <- c(labels[term])
  
  for (threshold_name in names(severity_tabs)) {
    tab <- severity_tabs[[threshold_name]]
    
    row_values <- c(
      row_values,
      fmt_num(get_value(tab, term, "estimate")),
      fmt_num(get_value(tab, term, "se")),
      fmt_num(get_value(tab, term, "statistic")),
      fmt_p(get_value(tab, term, "p"))
    )
  }
  
  latex_severity <- c(
    latex_severity,
    paste(row_values, collapse = " & "),
    "\\\\"
  )
}

latex_severity <- c(
  latex_severity,
  "\\hline",
  paste0(
    "Observations & \\multicolumn{4}{c}{",
    threshold_summary$observations_stage_2[1],
    "} & \\multicolumn{4}{c}{",
    threshold_summary$observations_stage_2[2],
    "} & \\multicolumn{4}{c}{",
    threshold_summary$observations_stage_2[3],
    "} \\\\"
  ),
  paste0(
    "Threshold value & \\multicolumn{4}{c}{",
    sprintf("%.3f", threshold_summary$tau[1]),
    "} & \\multicolumn{4}{c}{",
    sprintf("%.3f", threshold_summary$tau[2]),
    "} & \\multicolumn{4}{c}{",
    sprintf("%.3f", threshold_summary$tau[3]),
    "} \\\\"
  ),
  paste0(
    "$R^2$ / Adjusted $R^2$ & \\multicolumn{4}{c}{",
    sprintf(
      "%.3f / %.3f",
      summary(model_results[[1]]$severity_model)$r.squared,
      summary(model_results[[1]]$severity_model)$adj.r.squared
    ),
    "} & \\multicolumn{4}{c}{",
    sprintf(
      "%.3f / %.3f",
      summary(model_results[[2]]$severity_model)$r.squared,
      summary(model_results[[2]]$severity_model)$adj.r.squared
    ),
    "} & \\multicolumn{4}{c}{",
    sprintf(
      "%.3f / %.3f",
      summary(model_results[[3]]$severity_model)$r.squared,
      summary(model_results[[3]]$severity_model)$adj.r.squared
    ),
    "} \\\\"
  ),
  "\\hline\\hline",
  "\\end{tabular}%",
  "}",
  "\\vspace{0.15cm}",
  "\\begin{minipage}{\\textwidth}",
  "\\footnotesize",
  "\\textbf{Note.} Est. denotes the coefficient estimate, SE denotes the standard error, $t$ denotes the t-statistic, and $p$ denotes the p-value. The dependent variable is the log-transformed downside-risk measure, conditional on downside risk being greater than zero. Models are estimated using robust linear regression via \\texttt{lmrob}. Rule-based architecture, low demand volatility, and low market noise are omitted reference categories. Positive architecture coefficients indicate higher downside-risk severity relative to rule-based architecture.",
  "\\end{minipage}",
  "\\end{table}"
)

cat(paste(latex_severity, collapse = "\n"))
