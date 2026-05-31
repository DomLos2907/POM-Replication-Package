## Two-Part Service Shortfall Models ------------------------------------------

## 1. Packages ----------------------------------------------------------------
library(readxl)
library(logistf)
library(robustbase)

## 2. Import run-level data ---------------------------------------------------
runlevel <- read_excel("C:/Users/loske/Desktop/POM_RunLevel_Dataset.xlsx")

## 3. Variable preparation ----------------------------------------------------
runlevel$avg_service_level <- as.numeric(runlevel$avg_service_level)

## If service level is stored as 0-100 instead of 0-1, rescale to 0-1
if (max(runlevel$avg_service_level, na.rm = TRUE) > 1) {
  runlevel$avg_service_level <- runlevel$avg_service_level / 100
}

runlevel$architecture <- factor(runlevel$architecture)
runlevel$demand_volatility <- factor(runlevel$demand_volatility)
runlevel$market_noise <- factor(runlevel$market_noise)

runlevel$architecture <- relevel(runlevel$architecture, ref = "centralized")
runlevel$demand_volatility <- relevel(runlevel$demand_volatility, ref = "0.1")
runlevel$market_noise <- relevel(runlevel$market_noise, ref = "0.03")

## 4. Inspect service-level distribution --------------------------------------

summary(runlevel$avg_service_level)

table(runlevel$avg_service_level == 1)
table(runlevel$avg_service_level < 0.975)

with(
  runlevel,
  table(architecture, avg_service_level == 1)
)

with(
  runlevel,
  table(architecture, avg_service_level < 0.975)
)

hist(
  runlevel$avg_service_level,
  breaks = 30,
  main = "Histogram of Average Service Level",
  xlab = "Average service level",
  ylab = "Frequency",
  col = "lightblue",
  border = "white"
)

## 5. Define service targets --------------------------------------------------

service_targets <- list(
  "100% service target" = 1.000,
  "97.5% service target" = 0.975
)

## 6. Helper function: build service shortfall data ---------------------------

build_service_shortfall_data <- function(data, target) {
  
  service_data <- data
  
  service_data$service_target <- target
  
  service_data$service_shortfall <- pmax(
    target - service_data$avg_service_level,
    0
  )
  
  ## Avoid treating numerical rounding noise as a shortfall
  service_data$service_shortfall[
    service_data$service_shortfall < 1e-10
  ] <- 0
  
  service_data$has_service_shortfall <- service_data$service_shortfall > 0
  
  service_data
}

## 7. Estimate two-part models for both targets -------------------------------

service_results <- list()

for (target_name in names(service_targets)) {
  
  target <- service_targets[[target_name]]
  
  service_data <- build_service_shortfall_data(
    data = runlevel,
    target = target
  )
  
  ## Stage 1: Does the run fall below the service target?
  occurrence_model <- logistf(
    has_service_shortfall ~ architecture + demand_volatility + market_noise,
    data = service_data
  )
  
  ## Stage 2: How large is the shortfall, conditional on shortfall occurring?
  severity_model <- lmrob(
    log(service_shortfall) ~ architecture + demand_volatility + market_noise,
    data = subset(service_data, service_shortfall > 0)
  )
  
  service_results[[target_name]] <- list(
    target = target,
    data = service_data,
    occurrence_model = occurrence_model,
    severity_model = severity_model
  )
}

## 8. Summary of observations -------------------------------------------------

service_threshold_summary <- data.frame(
  target = names(service_results),
  target_value = sapply(service_results, function(x) x$target),
  observations_stage_1 = sapply(service_results, function(x) nrow(x$data)),
  observations_stage_2 = sapply(
    service_results,
    function(x) sum(x$data$service_shortfall > 0)
  ),
  runs_without_shortfall = sapply(
    service_results,
    function(x) sum(x$data$service_shortfall == 0)
  )
)

service_threshold_summary

## 9. Descriptive counts by architecture --------------------------------------

make_count_summary <- function(service_data) {
  data.frame(
    architecture = levels(service_data$architecture),
    no_shortfall = as.vector(
      tapply(!service_data$has_service_shortfall, service_data$architecture, sum)
    ),
    shortfall = as.vector(
      tapply(service_data$has_service_shortfall, service_data$architecture, sum)
    ),
    shortfall_rate = as.vector(
      tapply(service_data$has_service_shortfall, service_data$architecture, mean)
    )
  )
}

counts_100 <- make_count_summary(service_results[[1]]$data)
counts_975 <- make_count_summary(service_results[[2]]$data)

counts_100
counts_975

## 10. Formatting helpers -----------------------------------------------------

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

fmt_pct <- function(x) {
  paste0(sprintf("%.1f", 100 * x), "\\%")
}

## 11. Extract Firth logistic model results -----------------------------------

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

## 12. Extract robust severity model results ----------------------------------

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

## 13. Paper-ready labels -----------------------------------------------------

labels <- c(
  "(Intercept)" = "Intercept",
  "architectureindependent" = "Independent architecture",
  "architecturerule_based" = "Rule-based architecture",
  "architecturesequential" = "Sequential architecture",
  "architecturesupervised" = "Supervised architecture",
  "demand_volatility0.25" = "High demand volatility",
  "market_noise0.1" = "High market noise"
)

terms <- names(labels)

get_value <- function(tab, term, column) {
  value <- tab[tab$term == term, column]
  if (length(value) == 0) {
    return(NA)
  }
  value
}

## 14. LaTeX table: service shortfall counts ----------------------------------

latex_counts <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Service Shortfall Occurrence by Decision-Making Architecture}",
  "\\label{tab:service_shortfall_counts}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccc}",
  "\\hline\\hline",
  " & \\multicolumn{3}{c}{100\\% service target} & \\multicolumn{3}{c}{97.5\\% service target} \\\\",
  "\\cline{2-4} \\cline{5-7}",
  "Architecture & No shortfall & Shortfall & Shortfall rate & No shortfall & Shortfall & Shortfall rate \\\\",
  "\\hline"
)

for (i in seq_len(nrow(counts_100))) {
  latex_counts <- c(
    latex_counts,
    paste0(
      counts_100$architecture[i], " & ",
      counts_100$no_shortfall[i], " & ",
      counts_100$shortfall[i], " & ",
      fmt_pct(counts_100$shortfall_rate[i]), " & ",
      counts_975$no_shortfall[i], " & ",
      counts_975$shortfall[i], " & ",
      fmt_pct(counts_975$shortfall_rate[i]),
      " \\\\"
    )
  )
}

latex_counts <- c(
  latex_counts,
  "\\hline\\hline",
  "\\end{tabular}%",
  "}",
  "\\vspace{0.15cm}",
  "\\begin{minipage}{\\textwidth}",
  "\\footnotesize",
  "\\textbf{Note.} No shortfall indicates that the simulation run meets or exceeds the respective service target. Shortfall indicates that average service level falls below the respective target.",
  "\\end{minipage}",
  "\\end{table}"
)

cat(paste(latex_counts, collapse = "\n"))

## 15. LaTeX table: first-stage occurrence models -----------------------------

occurrence_tabs <- lapply(
  service_results,
  function(x) extract_firth(x$occurrence_model)
)

latex_occurrence <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Decision-Making Architecture and Service Shortfall Occurrence}",
  "\\label{tab:service_shortfall_occurrence}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccccc}",
  "\\hline\\hline",
  " & \\multicolumn{4}{c}{100\\% service target} & \\multicolumn{4}{c}{97.5\\% service target} \\\\",
  "\\cline{2-5} \\cline{6-9}",
  "Predictors & Est. & SE & $z$ & $p$ & Est. & SE & $z$ & $p$ \\\\",
  "\\hline"
)

for (term in terms) {
  row_values <- c(labels[term])
  
  for (target_name in names(occurrence_tabs)) {
    tab <- occurrence_tabs[[target_name]]
    
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
    service_threshold_summary$observations_stage_1[1],
    "} & \\multicolumn{4}{c}{",
    service_threshold_summary$observations_stage_1[2],
    "} \\\\"
  ),
  paste0(
    "Target value & \\multicolumn{4}{c}{",
    sprintf("%.3f", service_threshold_summary$target_value[1]),
    "} & \\multicolumn{4}{c}{",
    sprintf("%.3f", service_threshold_summary$target_value[2]),
    "} \\\\"
  ),
  paste0(
    "Runs without shortfall & \\multicolumn{4}{c}{",
    service_threshold_summary$runs_without_shortfall[1],
    "} & \\multicolumn{4}{c}{",
    service_threshold_summary$runs_without_shortfall[2],
    "} \\\\"
  ),
  "\\hline\\hline",
  "\\end{tabular}%",
  "}",
  "\\vspace{0.15cm}",
  "\\begin{minipage}{\\textwidth}",
  "\\footnotesize",
  "\\textbf{Note.} Est. denotes the coefficient estimate, SE denotes the standard error, $z$ denotes the z-statistic, and $p$ denotes the p-value. The dependent variable equals one if average service level falls below the respective service target. Models are estimated using Firth logistic regression. Positive coefficients indicate a higher probability of service shortfall. Centralized architecture, low demand volatility, and low market noise are omitted reference categories.",
  "\\end{minipage}",
  "\\end{table}"
)

cat(paste(latex_occurrence, collapse = "\n"))

## 16. LaTeX table: second-stage severity models ------------------------------

severity_tabs <- lapply(
  service_results,
  function(x) extract_lmrob(x$severity_model)
)

latex_severity <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Decision-Making Architecture and Service Shortfall Severity}",
  "\\label{tab:service_shortfall_severity}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccccc}",
  "\\hline\\hline",
  " & \\multicolumn{4}{c}{100\\% service target} & \\multicolumn{4}{c}{97.5\\% service target} \\\\",
  "\\cline{2-5} \\cline{6-9}",
  "Predictors & Est. & SE & $t$ & $p$ & Est. & SE & $t$ & $p$ \\\\",
  "\\hline"
)

for (term in terms) {
  row_values <- c(labels[term])
  
  for (target_name in names(severity_tabs)) {
    tab <- severity_tabs[[target_name]]
    
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
    service_threshold_summary$observations_stage_2[1],
    "} & \\multicolumn{4}{c}{",
    service_threshold_summary$observations_stage_2[2],
    "} \\\\"
  ),
  paste0(
    "Target value & \\multicolumn{4}{c}{",
    sprintf("%.3f", service_threshold_summary$target_value[1]),
    "} & \\multicolumn{4}{c}{",
    sprintf("%.3f", service_threshold_summary$target_value[2]),
    "} \\\\"
  ),
  paste0(
    "$R^2$ / Adjusted $R^2$ & \\multicolumn{4}{c}{",
    sprintf(
      "%.3f / %.3f",
      summary(service_results[[1]]$severity_model)$r.squared,
      summary(service_results[[1]]$severity_model)$adj.r.squared
    ),
    "} & \\multicolumn{4}{c}{",
    sprintf(
      "%.3f / %.3f",
      summary(service_results[[2]]$severity_model)$r.squared,
      summary(service_results[[2]]$severity_model)$adj.r.squared
    ),
    "} \\\\"
  ),
  "\\hline\\hline",
  "\\end{tabular}%",
  "}",
  "\\vspace{0.15cm}",
  "\\begin{minipage}{\\textwidth}",
  "\\footnotesize",
  "\\textbf{Note.} Est. denotes the coefficient estimate, SE denotes the standard error, $t$ denotes the t-statistic, and $p$ denotes the p-value. The dependent variable is the log-transformed service shortfall, conditional on average service level falling below the respective service target. Models are estimated using robust linear regression via \\texttt{lmrob}. Positive coefficients indicate larger service shortfalls. Centralized architecture, low demand volatility, and low market noise are omitted reference categories.",
  "\\end{minipage}",
  "\\end{table}"
)

cat(paste(latex_severity, collapse = "\n"))
