## Appendix: One-Stage Service Quality Model ----------------------------------

## 1. Packages ----------------------------------------------------------------
library(readxl)

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

## 4. Descriptive checks ------------------------------------------------------

summary(runlevel$avg_service_level)

table(runlevel$architecture)
table(runlevel$demand_volatility)
table(runlevel$market_noise)

table(runlevel$avg_service_level == 0)
table(runlevel$avg_service_level == 1)

hist(
  runlevel$avg_service_level,
  breaks = 30,
  main = "Histogram of Average Service Level",
  xlab = "Average service level",
  ylab = "Frequency",
  col = "lightblue",
  border = "white"
)

boxplot(
  avg_service_level ~ architecture,
  data = runlevel,
  main = "Average Service Level by Architecture",
  xlab = "Architecture",
  ylab = "Average service level",
  col = "lightblue",
  border = "gray40"
)

## 5. One-stage fractional logit models ---------------------------------------

## Model 1: architecture only
model_service_1 <- glm(
  avg_service_level ~ architecture,
  data = runlevel,
  family = quasibinomial(link = "logit")
)

## Model 2: architecture plus controls
model_service_2 <- glm(
  avg_service_level ~ architecture + demand_volatility + market_noise,
  data = runlevel,
  family = quasibinomial(link = "logit")
)

summary(model_service_1)
summary(model_service_2)

## 6. Formatting helpers ------------------------------------------------------

fmt_num <- function(x) {
  ifelse(is.na(x), "", sprintf("%.2f", x))
}

fmt_num3 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", x))
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

## 7. Extract model results ---------------------------------------------------

extract_glm <- function(model) {
  coefs <- as.data.frame(summary(model)$coefficients)
  coefs$term <- rownames(coefs)
  rownames(coefs) <- NULL
  
  data.frame(
    term = coefs$term,
    estimate = coefs$Estimate,
    se = coefs$`Std. Error`,
    statistic = coefs$`t value`,
    p = coefs$`Pr(>|t|)`
  )
}

service_1_tab <- extract_glm(model_service_1)
service_2_tab <- extract_glm(model_service_2)

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

deviance_explained <- function(model) {
  1 - model$deviance / model$null.deviance
}

## 8. LaTeX table: one-stage service quality model ----------------------------

latex_service <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Decision-Making Architecture and Average Service Level}",
  "\\label{tab:appendix_service_one_stage}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccccc}",
  "\\hline\\hline",
  " & \\multicolumn{8}{c}{Dependent variable: Average service level} \\\\",
  "\\cline{2-9}",
  " & \\multicolumn{4}{c}{Architecture only} & \\multicolumn{4}{c}{Architecture + controls} \\\\",
  "\\cline{2-5} \\cline{6-9}",
  "Predictors & Est. & SE & $t$ & $p$ & Est. & SE & $t$ & $p$ \\\\",
  "\\hline"
)

for (term in terms) {
  latex_service <- c(
    latex_service,
    paste0(
      labels[term], " & ",
      fmt_num(get_value(service_1_tab, term, "estimate")), " & ",
      fmt_num(get_value(service_1_tab, term, "se")), " & ",
      fmt_num(get_value(service_1_tab, term, "statistic")), " & ",
      fmt_p(get_value(service_1_tab, term, "p")), " & ",
      fmt_num(get_value(service_2_tab, term, "estimate")), " & ",
      fmt_num(get_value(service_2_tab, term, "se")), " & ",
      fmt_num(get_value(service_2_tab, term, "statistic")), " & ",
      fmt_p(get_value(service_2_tab, term, "p")),
      " \\\\"
    )
  )
}

latex_service <- c(
  latex_service,
  "\\hline",
  paste0(
    "Observations & \\multicolumn{4}{c}{",
    nobs(model_service_1),
    "} & \\multicolumn{4}{c}{",
    nobs(model_service_2),
    "} \\\\"
  ),
  paste0(
    "Deviance explained & \\multicolumn{4}{c}{",
    fmt_num3(deviance_explained(model_service_1)),
    "} & \\multicolumn{4}{c}{",
    fmt_num3(deviance_explained(model_service_2)),
    "} \\\\"
  ),
  paste0(
    "Dispersion parameter & \\multicolumn{4}{c}{",
    fmt_num3(summary(model_service_1)$dispersion),
    "} & \\multicolumn{4}{c}{",
    fmt_num3(summary(model_service_2)$dispersion),
    "} \\\\"
  ),
  "\\hline\\hline",
  "\\end{tabular}%",
  "}",
  "\\vspace{0.15cm}",
  "\\begin{minipage}{\\textwidth}",
  "\\footnotesize",
  "\\textbf{Note.} Est. denotes the coefficient estimate, SE denotes the standard error, $t$ denotes the t-statistic, and $p$ denotes the p-value. The dependent variable is average service level, measured at the simulation-run level. Models are estimated as fractional logit models using a quasibinomial logit link. Centralized architecture, low demand volatility, and low market noise are omitted reference categories.",
  "\\end{minipage}",
  "\\end{table}"
)

cat(paste(latex_service, collapse = "\n"))
cat("\n\n")

## 9. Predicted service levels by architecture --------------------------------
## Predictions are based on Model 2, holding controls at reference levels.

architecture_labels <- c(
  "centralized" = "Centralized",
  "independent" = "Independent",
  "rule_based" = "Rule-based",
  "sequential" = "Sequential",
  "supervised" = "Supervised"
)

prediction_data <- data.frame(
  architecture = factor(
    levels(runlevel$architecture),
    levels = levels(runlevel$architecture)
  ),
  demand_volatility = factor(
    "0.1",
    levels = levels(runlevel$demand_volatility)
  ),
  market_noise = factor(
    "0.03",
    levels = levels(runlevel$market_noise)
  )
)

pred_link <- predict(
  model_service_2,
  newdata = prediction_data,
  type = "link",
  se.fit = TRUE
)

prediction_data$predicted_service_level <- plogis(pred_link$fit)
prediction_data$lower_95 <- plogis(pred_link$fit - 1.96 * pred_link$se.fit)
prediction_data$upper_95 <- plogis(pred_link$fit + 1.96 * pred_link$se.fit)

prediction_data

## 10. LaTeX table: predicted service levels ----------------------------------

latex_pred <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Predicted Average Service Level by Decision-Making Architecture}",
  "\\label{tab:appendix_service_predicted_levels}",
  "\\begin{tabular}{lccc}",
  "\\hline\\hline",
  "Architecture & Predicted service level & 95\\% CI lower & 95\\% CI upper \\\\",
  "\\hline"
)

for (i in seq_len(nrow(prediction_data))) {
  arch <- as.character(prediction_data$architecture[i])
  
  latex_pred <- c(
    latex_pred,
    paste0(
      architecture_labels[arch], " & ",
      fmt_pct(prediction_data$predicted_service_level[i]), " & ",
      fmt_pct(prediction_data$lower_95[i]), " & ",
      fmt_pct(prediction_data$upper_95[i]),
      " \\\\"
    )
  )
}

latex_pred <- c(
  latex_pred,
  "\\hline\\hline",
  "\\end{tabular}",
  "\\vspace{0.15cm}",
  "\\begin{minipage}{0.78\\textwidth}",
  "\\footnotesize",
  "\\textbf{Note.} Predicted values are based on the fractional logit model with controls. Demand volatility and market noise are held at their reference levels, corresponding to low demand volatility (0.1) and low market noise (0.03).",
  "\\end{minipage}",
  "\\end{table}"
)

cat(paste(latex_pred, collapse = "\n"))
