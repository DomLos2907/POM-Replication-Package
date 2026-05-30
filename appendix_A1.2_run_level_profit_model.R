## Run-Level Robust Profit Model ----------------------------------------------

## 1. Packages ----------------------------------------------------------------
library(readxl)
library(robustbase)
library(sjPlot)
library(ggplot2)

## 2. Import data -------------------------------------------------------------
runlevel <- read_excel("C:/Users/loske/Desktop/POM_RunLevel_Dataset.xlsx")

## 3. Variable preparation ----------------------------------------------------
runlevel$total_profit_mio <- runlevel$total_profit / 1000000

runlevel$architecture <- factor(runlevel$architecture)
runlevel$demand_volatility <- factor(runlevel$demand_volatility)
runlevel$market_noise <- factor(runlevel$market_noise)

runlevel$architecture <- relevel(runlevel$architecture, ref = "centralized")
runlevel$demand_volatility <- relevel(runlevel$demand_volatility, ref = "0.1")
runlevel$market_noise <- relevel(runlevel$market_noise, ref = "0.03")

## 4. Descriptive checks ------------------------------------------------------
summary(runlevel$total_profit_mio)
table(runlevel$architecture)
table(runlevel$demand_volatility)
table(runlevel$market_noise)

x_ticks <- pretty(runlevel$total_profit_mio)

hist(
  runlevel$total_profit_mio,
  breaks = 30,
  main = "Histogram of total_profit",
  xlab = "total_profit in Mio. EUR",
  ylab = "Frequency",
  col = "lightblue",
  border = "white",
  xaxt = "n"
)

axis(
  side = 1,
  at = x_ticks,
  labels = sprintf("%.2f", x_ticks)
)

## 5. Robust run-level models -------------------------------------------------

## Model 1: architecture only
model_profit_robust_1 <- lmrob(
  total_profit_mio ~ architecture,
  data = runlevel
)

## Model 2: architecture plus controls
model_profit_robust_2 <- lmrob(
  total_profit_mio ~ architecture + demand_volatility + market_noise,
  data = runlevel
)

summary(model_profit_robust_1)
summary(model_profit_robust_2)

## 6. Diagnostics for final model ---------------------------------------------
robust_residuals <- resid(model_profit_robust_2)
robust_fitted <- fitted(model_profit_robust_2)

qqnorm(
  robust_residuals,
  main = "Normal Q-Q Plot: Robust Profit Model Residuals"
)
qqline(robust_residuals, col = "red")

plot(
  robust_fitted,
  robust_residuals,
  xlab = "Fitted values",
  ylab = "Residuals",
  main = "Residuals vs. Fitted Values"
)
abline(h = 0, col = "red")

robust_weights <- weights(model_profit_robust_2, type = "robustness")
summary(robust_weights)
which(robust_weights < 0.5)

## 7. Optional tab_model output in R Viewer -----------------------------------
tab_model(
  model_profit_robust_1,
  model_profit_robust_2,
  show.ci = FALSE,
  show.se = TRUE,
  show.stat = TRUE,
  dv.labels = c(
    "Architecture only",
    "Architecture + controls"
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

## 8. Paper-ready LaTeX table: appendix-style format --------------------------

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

extract_lmrob <- function(model) {
  coefs <- as.data.frame(summary(model)$coefficients)
  coefs$term <- rownames(coefs)
  rownames(coefs) <- NULL
  
  data.frame(
    term = coefs$term,
    estimate = fmt_num(coefs$Estimate),
    se = fmt_num(coefs$`Std. Error`),
    statistic = fmt_num(coefs$`t value`),
    p = fmt_p(coefs$`Pr(>|t|)`)
  )
}

model_1_tab <- extract_lmrob(model_profit_robust_1)
model_2_tab <- extract_lmrob(model_profit_robust_2)

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

get_row <- function(tab, term, column) {
  value <- tab[tab$term == term, column]
  if (length(value) == 0) {
    return("")
  }
  value
}

latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Decision-Making Architecture and Average Total Profit}",
  "\\label{tab:run_level_profit_robust}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccccc}",
  "\\hline\\hline",
  " & \\multicolumn{8}{c}{Dependent variable: Average total profit} \\\\",
  "\\cline{2-9}",
  " & \\multicolumn{4}{c}{Architecture only} & \\multicolumn{4}{c}{Architecture + controls} \\\\",
  "\\cline{2-5} \\cline{6-9}",
  "Predictors & Est. & SE & $t$ & $p$ & Est. & SE & $t$ & $p$ \\\\",
  "\\hline"
)

for (term in terms) {
  latex_lines <- c(
    latex_lines,
    paste0(
      labels[term], " & ",
      get_row(model_1_tab, term, "estimate"), " & ",
      get_row(model_1_tab, term, "se"), " & ",
      get_row(model_1_tab, term, "statistic"), " & ",
      get_row(model_1_tab, term, "p"), " & ",
      get_row(model_2_tab, term, "estimate"), " & ",
      get_row(model_2_tab, term, "se"), " & ",
      get_row(model_2_tab, term, "statistic"), " & ",
      get_row(model_2_tab, term, "p"),
      " \\\\"
    )
  )
}

latex_lines <- c(
  latex_lines,
  "\\hline",
  paste0(
    "Observations & \\multicolumn{4}{c}{",
    nobs(model_profit_robust_1),
    "} & \\multicolumn{4}{c}{",
    nobs(model_profit_robust_2),
    "} \\\\"
  ),
  paste0(
    "$R^2$ / Adjusted $R^2$ & \\multicolumn{4}{c}{",
    sprintf(
      "%.3f / %.3f",
      summary(model_profit_robust_1)$r.squared,
      summary(model_profit_robust_1)$adj.r.squared
    ),
    "} & \\multicolumn{4}{c}{",
    sprintf(
      "%.3f / %.3f",
      summary(model_profit_robust_2)$r.squared,
      summary(model_profit_robust_2)$adj.r.squared
    ),
    "} \\\\"
  ),
  "\\hline\\hline",
  "\\end{tabular}%",
  "}",
  "\\vspace{0.15cm}",
  "\\begin{minipage}{\\textwidth}",
  "\\footnotesize",
  "\\textbf{Note.} The analysis is conducted at the simulation-run level. The dependent variable is total profit measured in million euros. Models are estimated using robust linear regression with MM-estimation via \\texttt{lmrob}. Decision-making architecture is dummy-coded, with centralized architecture as the omitted reference category. Low demand volatility (0.1) and low market noise (0.03) are omitted reference categories for the controls.",
  "\\end{minipage}",
  "\\end{table}"
)

cat(paste(latex_lines, collapse = "\n"))

## 9. Correlation matrix of model predictors ----------------------------------

## Create dummy-coded model matrix matching the regression specification
x_matrix <- model.matrix(
  ~ architecture + demand_volatility + market_noise,
  data = runlevel
)

## Remove intercept
x_matrix <- x_matrix[, colnames(x_matrix) != "(Intercept)"]

## Clean variable names
clean_names <- c(
  "Independent architecture",
  "Rule-based architecture",
  "Sequential architecture",
  "Supervised architecture",
  "High demand volatility",
  "High market noise"
)

colnames(x_matrix) <- clean_names

## Correlation matrix
cor_matrix <- cor(x_matrix)
cor_matrix_round <- round(cor_matrix, 3)

cor_matrix_round

## LaTeX table
latex_cor <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Correlation Matrix of Model Predictors}",
  "\\label{tab:correlation_profit_model}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lrrrrrr}",
  "\\hline\\hline",
  paste(
    c("Predictor", clean_names),
    collapse = " & "
  ),
  " \\\\",
  "\\hline"
)

for (i in seq_len(nrow(cor_matrix_round))) {
  latex_cor <- c(
    latex_cor,
    paste0(
      rownames(cor_matrix_round)[i], " & ",
      paste(sprintf("%.3f", cor_matrix_round[i, ]), collapse = " & "),
      " \\\\"
    )
  )
}

latex_cor <- c(
  latex_cor,
  "\\hline\\hline",
  "\\end{tabular}%",
  "}",
  "\\vspace{0.15cm}",
  "\\begin{minipage}{\\textwidth}",
  "\\footnotesize",
  "\\textbf{Note.} The correlation matrix is based on the dummy-coded predictors used in the robust regression model. Centralized architecture, low demand volatility, and low market noise are omitted reference categories.",
  "\\end{minipage}",
  "\\end{table}"
)

cat(paste(latex_cor, collapse = "\n"))
