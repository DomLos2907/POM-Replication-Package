## Downside Profit Risk Model -------------------------------------------------

## 1. Packages ----------------------------------------------------------------
library(readxl)
library(robustbase)
library(sjPlot)
library(ggplot2)
library(logistf)

## 2. Import period-level data ------------------------------------------------
periodlevel <- read_excel(
  "C:/Users/loske/Desktop/POM_Experiment_Data Kopie.xlsx",
  sheet = "Period-Level"
)

## 3. Variable preparation ----------------------------------------------------
periodlevel$period_profit_mio <- periodlevel$period_profit / 1000000

periodlevel$architecture <- factor(periodlevel$architecture)
periodlevel$demand_volatility <- factor(periodlevel$demand_volatility)
periodlevel$market_noise <- factor(periodlevel$market_noise)
periodlevel$replication <- factor(periodlevel$replication)

periodlevel$architecture <- relevel(periodlevel$architecture, ref = "centralized")
periodlevel$demand_volatility <- relevel(periodlevel$demand_volatility, ref = "0.1")
periodlevel$market_noise <- relevel(periodlevel$market_noise, ref = "0.03")

## 4. Build downside-risk construct ------------------------------------------
## Downside risk is defined as the root mean squared shortfall
## of period profit below zero within each simulation run.

run_downside_zero <- aggregate(
  period_profit_mio ~ architecture + demand_volatility + market_noise + replication,
  data = periodlevel,
  FUN = function(x) {
    sqrt(mean(pmax(0 - x, 0)^2, na.rm = TRUE))
  }
)

names(run_downside_zero)[
  names(run_downside_zero) == "period_profit_mio"
] <- "downside_risk_zero"

run_downside_zero$architecture <- factor(run_downside_zero$architecture)
run_downside_zero$demand_volatility <- factor(run_downside_zero$demand_volatility)
run_downside_zero$market_noise <- factor(run_downside_zero$market_noise)

run_downside_zero$architecture <- relevel(run_downside_zero$architecture, ref = "centralized")
run_downside_zero$demand_volatility <- relevel(run_downside_zero$demand_volatility, ref = "0.1")
run_downside_zero$market_noise <- relevel(run_downside_zero$market_noise, ref = "0.03")

## 5. Inspect downside-risk construct ----------------------------------------

summary(run_downside_zero$downside_risk_zero)

table(run_downside_zero$downside_risk_zero == 0)

with(
  run_downside_zero,
  table(architecture, downside_risk_zero == 0)
)

hist(
  run_downside_zero$downside_risk_zero,
  breaks = 30,
  main = "Histogram of Downside Profit Risk",
  xlab = "Downside profit risk below zero in Mio. EUR",
  ylab = "Frequency",
  col = "lightblue",
  border = "white"
)

boxplot(
  downside_risk_zero ~ architecture,
  data = run_downside_zero,
  main = "Downside Profit Risk by Architecture",
  xlab = "Architecture",
  ylab = "Downside profit risk below zero in Mio. EUR",
  col = "lightblue",
  border = "gray40"
)

## 6. Two-part downside-risk model -------------------------------------------

## Part 1: Probability that downside risk occurs at all
run_downside_zero$has_downside <- run_downside_zero$downside_risk_zero > 0

## Standard logit model, estimated for comparison
model_downside_occurrence_logit <- glm(
  has_downside ~ architecture + demand_volatility + market_noise,
  data = run_downside_zero,
  family = binomial
)

summary(model_downside_occurrence_logit)

## Firth logistic regression handles separation from architectures
## where all or nearly all runs have downside risk.
model_downside_occurrence_firth <- logistf(
  has_downside ~ architecture + demand_volatility + market_noise,
  data = run_downside_zero
)

summary(model_downside_occurrence_firth)

## Part 2: Severity of downside risk, conditional on downside risk occurring
model_downside_severity <- lmrob(
  log(downside_risk_zero) ~ architecture + demand_volatility + market_noise,
  data = subset(run_downside_zero, downside_risk_zero > 0)
)

summary(model_downside_severity)

## 7. Diagnostics for severity model -----------------------------------------

severity_residuals <- resid(model_downside_severity)
severity_fitted <- fitted(model_downside_severity)

qqnorm(
  severity_residuals,
  main = "Normal Q-Q Plot: Downside Severity Model Residuals"
)
qqline(severity_residuals, col = "red")

plot(
  severity_fitted,
  severity_residuals,
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs. Fitted Values: Downside Severity"
)
abline(h = 0, col = "red")

severity_weights <- weights(model_downside_severity, type = "robustness")

summary(severity_weights)
which(severity_weights < 0.5)

## 8. Paper-ready table in R Viewer ------------------------------------------

tab_model(
  model_downside_occurrence_logit,
  model_downside_severity,
  show.ci = FALSE,
  show.se = TRUE,
  show.stat = TRUE,
  transform = NULL,
  dv.labels = c(
    "Any downside risk",
    "Downside risk severity"
  ),
  pred.labels = c(
    "Constant",
    "Independent architecture",
    "Rule-based architecture",
    "Sequential architecture",
    "Supervised architecture",
    "High demand volatility",
    "High market noise"
  )
)

## Note:
## sjPlot::tab_model may not handle logistf objects cleanly.
## If this fails, use the custom LaTeX table below.

## 9. Predicted probabilities for downside occurrence -------------------------

pred_occurrence_logit <- predict(
  model_downside_occurrence_logit,
  type = "response"
)

run_downside_zero$pred_downside_probability_logit <- pred_occurrence_logit

aggregate(
  pred_downside_probability_logit ~ architecture,
  data = run_downside_zero,
  FUN = mean
)

## Descriptive occurrence rates by architecture
occurrence_rates <- aggregate(
  has_downside ~ architecture,
  data = run_downside_zero,
  FUN = mean
)

occurrence_rates

## 10. Mean downside risk by architecture -------------------------------------

downside_summary <- aggregate(
  downside_risk_zero ~ architecture,
  data = run_downside_zero,
  FUN = function(x) c(
    mean = mean(x),
    median = median(x),
    sd = sd(x),
    zero_share = mean(x == 0)
  )
)

downside_summary

## 11. Custom LaTeX table: two-part model -------------------------------------

fmt_num <- function(x) sprintf("%.2f", x)

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

## Extract Firth logit coefficients
firth_coef <- as.data.frame(model_downside_occurrence_firth$coefficients)
names(firth_coef) <- "Estimate"
firth_coef$term <- rownames(firth_coef)
rownames(firth_coef) <- NULL

firth_coef$SE <- sqrt(diag(model_downside_occurrence_firth$var))
firth_coef$Statistic <- firth_coef$Estimate / firth_coef$SE
firth_coef$p <- model_downside_occurrence_firth$prob

## Extract robust severity coefficients
severity_coef <- as.data.frame(summary(model_downside_severity)$coefficients)
severity_coef$term <- rownames(severity_coef)
rownames(severity_coef) <- NULL

labels <- c(
  "(Intercept)" = "Constant",
  "architectureindependent" = "Independent architecture",
  "architecturerule_based" = "Rule-based architecture",
  "architecturesequential" = "Sequential architecture",
  "architecturesupervised" = "Supervised architecture",
  "demand_volatility0.25" = "High demand volatility",
  "market_noise0.1" = "High market noise"
)

terms <- names(labels)

get_firth <- function(term, column) {
  value <- firth_coef[firth_coef$term == term, column]
  if (length(value) == 0) return("")
  value
}

get_severity <- function(term, column) {
  value <- severity_coef[severity_coef$term == term, column]
  if (length(value) == 0) return("")
  value
}

latex_downside <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Decision-Making Architecture and Downside Profit Risk}",
  "\\label{tab:downside_profit_risk}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccccc}",
  "\\hline\\hline",
  " & \\multicolumn{4}{c}{Any downside risk} & \\multicolumn{4}{c}{Downside risk severity} \\\\",
  "\\cline{2-5} \\cline{6-9}",
  "Predictors & Est. & SE & $z$ & $p$ & Est. & SE & $t$ & $p$ \\\\",
  "\\hline"
)

for (term in terms) {
  latex_downside <- c(
    latex_downside,
    paste0(
      labels[term], " & ",
      ifelse(get_firth(term, "Estimate") == "", "", fmt_num(get_firth(term, "Estimate"))), " & ",
      ifelse(get_firth(term, "SE") == "", "", fmt_num(get_firth(term, "SE"))), " & ",
      ifelse(get_firth(term, "Statistic") == "", "", fmt_num(get_firth(term, "Statistic"))), " & ",
      ifelse(get_firth(term, "p") == "", "", fmt_p(get_firth(term, "p"))), " & ",
      ifelse(get_severity(term, "Estimate") == "", "", fmt_num(get_severity(term, "Estimate"))), " & ",
      ifelse(get_severity(term, "Std. Error") == "", "", fmt_num(get_severity(term, "Std. Error"))), " & ",
      ifelse(get_severity(term, "t value") == "", "", fmt_num(get_severity(term, "t value"))), " & ",
      ifelse(get_severity(term, "Pr(>|t|)") == "", "", fmt_p(get_severity(term, "Pr(>|t|)"))),
      " \\\\"
    )
  )
}

latex_downside <- c(
  latex_downside,
  "\\hline",
  paste0(
    "Observations & \\multicolumn{4}{c}{",
    nobs(model_downside_occurrence_logit),
    "} & \\multicolumn{4}{c}{",
    nobs(model_downside_severity),
    "} \\\\"
  ),
  "\\hline\\hline",
  "\\end{tabular}%",
  "}",
  "\\vspace{0.15cm}",
  "\\begin{minipage}{\\textwidth}",
  "\\footnotesize",
  "\\textbf{Note.} The analysis is conducted at the simulation-run level. Downside profit risk is measured as the root mean squared shortfall of period profit below zero within each simulation run. The first model estimates whether a run experiences any downside risk using Firth logistic regression. The second model estimates downside risk severity conditional on downside risk occurring, using robust linear regression with log-transformed downside risk as the dependent variable. Centralized architecture, low demand volatility, and low market noise are omitted reference categories.",
  "\\end{minipage}",
  "\\end{table}"
)

cat(paste(latex_downside, collapse = "\n"))

## 12. Optional: occurrence and construct summary table -----------------------

occurrence_summary <- with(
  run_downside_zero,
  data.frame(
    architecture = levels(architecture),
    downside_occurrence_rate = as.vector(tapply(has_downside, architecture, mean)),
    zero_downside_share = as.vector(tapply(downside_risk_zero == 0, architecture, mean)),
    mean_downside_risk = as.vector(tapply(downside_risk_zero, architecture, mean)),
    median_downside_risk = as.vector(tapply(downside_risk_zero, architecture, median))
  )
)

occurrence_summary
