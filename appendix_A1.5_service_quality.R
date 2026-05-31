## Service Quality Models: Average Service Level and 97.5% Shortfall -----------
## Reference category: rule-based architecture

## 1. Packages ----------------------------------------------------------------
library(readxl)
library(logistf)
library(robustbase)

## 2. Import run-level data ---------------------------------------------------
project_data_path <- "data/raw/POM_RunLevel_Dataset.xlsx"
desktop_data_path <- "C:/Users/loske/Desktop/POM_RunLevel_Dataset.xlsx"

if (file.exists(project_data_path)) {
  runlevel <- read_excel(project_data_path)
} else {
  runlevel <- read_excel(desktop_data_path)
}

## 3. Variable preparation ----------------------------------------------------
runlevel$avg_service_level <- as.numeric(runlevel$avg_service_level)

## If service level is stored as 0-100 instead of 0-1, rescale to 0-1
if (max(runlevel$avg_service_level, na.rm = TRUE) > 1) {
  runlevel$avg_service_level <- runlevel$avg_service_level / 100
}

runlevel$architecture <- factor(runlevel$architecture)
runlevel$demand_volatility <- factor(runlevel$demand_volatility)
runlevel$market_noise <- factor(runlevel$market_noise)

## Main change: rule-based architecture is now the omitted reference category
runlevel$architecture <- relevel(runlevel$architecture, ref = "rule_based")
runlevel$demand_volatility <- relevel(runlevel$demand_volatility, ref = "0.1")
runlevel$market_noise <- relevel(runlevel$market_noise, ref = "0.03")

## 4. Diagnostic checks -------------------------------------------------------

summary(runlevel$avg_service_level)

## 100% target diagnostic only
table(runlevel$avg_service_level == 1)

## Company-relevant service target
service_target <- 0.975

runlevel$service_shortfall <- pmax(
  service_target - runlevel$avg_service_level,
  0
)

runlevel$service_shortfall[
  runlevel$service_shortfall < 1e-10
] <- 0

runlevel$has_service_shortfall <- as.integer(
  runlevel$service_shortfall > 0
)

table(runlevel$has_service_shortfall)

with(
  runlevel,
  table(architecture, has_service_shortfall)
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

## 5. Main average service-level model ----------------------------------------
## Fractional logit model for bounded service-level outcome

model_service_fractional <- glm(
  avg_service_level ~ architecture + demand_volatility + market_noise,
  data = runlevel,
  family = quasibinomial(link = "logit")
)

summary(model_service_fractional)

## 6. Target-based two-part service shortfall model ---------------------------

## Stage 1: Does the run fall below the 97.5% target?
model_service_shortfall_occurrence <- logistf(
  has_service_shortfall ~ architecture + demand_volatility + market_noise,
  data = runlevel
)

summary(model_service_shortfall_occurrence)

## Stage 2: How large is the shortfall, conditional on falling below 97.5%?
service_severity_data <- subset(runlevel, service_shortfall > 0)
service_severity_data <- droplevels(service_severity_data)

model_service_shortfall_severity <- lmrob(
  log(service_shortfall) ~ architecture + demand_volatility + market_noise,
  data = service_severity_data
)

summary(model_service_shortfall_severity)

## 7. Descriptive shortfall table ---------------------------------------------

service_counts <- data.frame(
  architecture = levels(runlevel$architecture),
  no_shortfall = as.vector(
    tapply(runlevel$has_service_shortfall == 0, runlevel$architecture, sum)
  ),
  shortfall = as.vector(
    tapply(runlevel$has_service_shortfall == 1, runlevel$architecture, sum)
  ),
  shortfall_rate = as.vector(
    tapply(runlevel$has_service_shortfall == 1, runlevel$architecture, mean)
  )
)

service_counts

## 8. Formatting helpers ------------------------------------------------------

fmt_num <- function(x) {
  ifelse(is.na(x), "--", sprintf("%.2f", x))
}

fmt_p <- function(p) {
  ifelse(
    is.na(p),
    "--",
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

## 9. Extraction helpers ------------------------------------------------------

extract_glm <- function(model) {
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
    return(NA)
  }
  value
}

## 10. LaTeX table: average service-level model -------------------------------

service_avg_tab <- extract_glm(model_service_fractional)

latex_service_avg <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Decision-Making Architecture and Average Service Level}",
  "\\label{tab:service_fractional_logit}",
  "\\begin{tabular}{lcccc}",
  "\\hline\\hline",
  "Predictors & Est. & SE & $t$ & $p$ \\\\",
  "\\hline"
)

for (term in terms) {
  latex_service_avg <- c(
    latex_service_avg,
    paste0(
      labels[term], " & ",
      fmt_num(get_value(service_avg_tab, term, "estimate")), " & ",
      fmt_num(get_value(service_avg_tab, term, "se")), " & ",
      fmt_num(get_value(service_avg_tab, term, "statistic")), " & ",
      fmt_p(get_value(service_avg_tab, term, "p")),
      " \\\\"
    )
  )
}

latex_service_avg <- c(
  latex_service_avg,
  "\\hline",
  paste0("Observations & \\multicolumn{4}{c}{", nobs(model_service_fractional), "} \\\\"),
  "\\hline\\hline",
  "\\end{tabular}",
  "\\vspace{0.15cm}",
  "\\begin{minipage}{0.82\\textwidth}",
  "\\footnotesize",
  "\\textbf{Note.} Est. denotes the coefficient estimate, SE denotes the standard error, $t$ denotes the t-statistic, and $p$ denotes the p-value. The dependent variable is average service level. The model is estimated as a fractional logit model using a quasibinomial logit link. Rule-based architecture, low demand volatility, and low market noise are omitted reference categories. Negative architecture coefficients indicate lower average service level relative to rule-based architecture.",
  "\\end{minipage}",
  "\\end{table}"
)

cat(paste(latex_service_avg, collapse = "\n"))
cat("\n\n")

## 11. LaTeX table: 97.5% shortfall counts ------------------------------------

architecture_labels <- c(
  "rule_based" = "Rule-based",
  "centralized" = "Centralized",
  "independent" = "Independent",
  "sequential" = "Sequential",
  "supervised" = "Supervised"
)

latex_counts <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Service Shortfall Occurrence by Decision-Making Architecture}",
  "\\label{tab:service_shortfall_counts_975}",
  "\\begin{tabular}{lccc}",
  "\\hline\\hline",
  "Architecture & No shortfall & Shortfall & Shortfall rate \\\\",
  "\\hline"
)

for (i in seq_len(nrow(service_counts))) {
  arch <- as.character(service_counts$architecture[i])
  
  latex_counts <- c(
    latex_counts,
    paste0(
      architecture_labels[arch], " & ",
      service_counts$no_shortfall[i], " & ",
      service_counts$shortfall[i], " & ",
      fmt_pct(service_counts$shortfall_rate[i]),
      " \\\\"
    )
  )
}

latex_counts <- c(
  latex_counts,
  "\\hline",
  paste0(
    "Total & ",
    sum(service_counts$no_shortfall),
    " & ",
    sum(service_counts$shortfall),
    " & ",
    fmt_pct(mean(runlevel$has_service_shortfall == 1)),
    " \\\\"
  ),
  "\\hline\\hline",
  "\\end{tabular}",
  "\\vspace{0.15cm}",
  "\\begin{minipage}{0.72\\textwidth}",
  "\\footnotesize",
  "\\textbf{Note.} The service target is 97.5\\%. No shortfall indicates that the simulation run meets or exceeds the target. Shortfall indicates that average service level falls below the target.",
  "\\end{minipage}",
  "\\end{table}"
)

cat(paste(latex_counts, collapse = "\n"))
cat("\n\n")

## 12. LaTeX table: 97.5% two-part model --------------------------------------

occurrence_tab <- extract_firth(model_service_shortfall_occurrence)
severity_tab <- extract_lmrob(model_service_shortfall_severity)

latex_twopart <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Decision-Making Architecture and Service Shortfall at the 97.5\\% Target}",
  "\\label{tab:service_shortfall_twopart_975}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccccc}",
  "\\hline\\hline",
  " & \\multicolumn{4}{c}{Shortfall occurrence} & \\multicolumn{4}{c}{Shortfall severity} \\\\",
  "\\cline{2-5} \\cline{6-9}",
  "Predictors & Est. & SE & $z$ & $p$ & Est. & SE & $t$ & $p$ \\\\",
  "\\hline"
)

for (term in terms) {
  latex_twopart <- c(
    latex_twopart,
    paste0(
      labels[term], " & ",
      fmt_num(get_value(occurrence_tab, term, "estimate")), " & ",
      fmt_num(get_value(occurrence_tab, term, "se")), " & ",
      fmt_num(get_value(occurrence_tab, term, "statistic")), " & ",
      fmt_p(get_value(occurrence_tab, term, "p")), " & ",
      fmt_num(get_value(severity_tab, term, "estimate")), " & ",
      fmt_num(get_value(severity_tab, term, "se")), " & ",
      fmt_num(get_value(severity_tab, term, "statistic")), " & ",
      fmt_p(get_value(severity_tab, term, "p")),
      " \\\\"
    )
  )
}

latex_twopart <- c(
  latex_twopart,
  "\\hline",
  paste0(
    "Observations & \\multicolumn{4}{c}{",
    nrow(runlevel),
    "} & \\multicolumn{4}{c}{",
    nrow(service_severity_data),
    "} \\\\"
  ),
  paste0(
    "Runs without shortfall & \\multicolumn{4}{c}{",
    sum(runlevel$has_service_shortfall == 0),
    "} & \\multicolumn{4}{c}{--} \\\\"
  ),
  paste0(
    "$R^2$ / Adjusted $R^2$ & \\multicolumn{4}{c}{--} & \\multicolumn{4}{c}{",
    sprintf(
      "%.3f / %.3f",
      summary(model_service_shortfall_severity)$r.squared,
      summary(model_service_shortfall_severity)$adj.r.squared
    ),
    "} \\\\"
  ),
  "\\hline\\hline",
  "\\end{tabular}%",
  "}",
  "\\vspace{0.15cm}",
  "\\begin{minipage}{\\textwidth}",
  "\\footnotesize",
  "\\textbf{Note.} Est. denotes the coefficient estimate, SE denotes the standard error, $z$ denotes the z-statistic, $t$ denotes the t-statistic, and $p$ denotes the p-value. The service target is 97.5\\%. The occurrence model estimates whether average service level falls below the target and is estimated using Firth logistic regression. The severity model estimates the log-transformed service shortfall, conditional on falling below the target, using robust linear regression via \\texttt{lmrob}. Positive coefficients indicate a higher probability of shortfall or larger shortfall severity relative to rule-based architecture. Rule-based architecture, low demand volatility, and low market noise are omitted reference categories. Rule-based architecture is not estimated in the severity model because no rule-based run falls below the 97.5\\% service target.",
  "\\end{minipage}",
  "\\end{table}"
)

cat(paste(latex_twopart, collapse = "\n"))
