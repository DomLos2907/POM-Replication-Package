## Appendix A2: Run-Level Robust Profit Model ---------------------------------

## 1. Packages ----------------------------------------------------------------
library(readxl)
library(robustbase)
library(sjPlot)
library(ggplot2)

## 2. Import data -------------------------------------------------------------
project_data_path <- "data/raw/POM_RunLevel_Dataset.xlsx"
desktop_data_path <- "C:/Users/loske/Desktop/POM_RunLevel_Dataset.xlsx"

if (file.exists(project_data_path)) {
  runlevel <- read_excel(project_data_path)
} else {
  runlevel <- read_excel(desktop_data_path)
}

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
model_profit_robust_1 <- lmrob(
  total_profit_mio ~ architecture,
  data = runlevel
)

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

## 8. Helper functions for LaTeX tables ---------------------------------------
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

get_row <- function(tab, term, column) {
  value <- tab[tab$term == term, column]
  if (length(value) == 0) {
    return("")
  }
  value
}

## 9. Main robust regression LaTeX table --------------------------------------
model_1_tab <- extract_lmrob(model_profit_robust_1)
model_2_tab <- extract_lmrob(model_profit_robust_2)

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

latex_model <- c(
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
  " & \\multicolumn{4}{c}{Model (1)} & \\multicolumn{4}{c}{Model (2)} \\\\",
  "\\cline{2-5} \\cline{6-9}",
  "Predictors & Est. & SE & $t$ & $p$ & Est. & SE & $t$ & $p$ \\\\",
  "\\hline"
)

for (term in terms) {
  latex_model <- c(
    latex_model,
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

latex_model <- c(
  latex_model,
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
  "\\textbf{Note.} Est. denotes the coefficient estimate, SE denotes the standard error, $t$ denotes the t-statistic, and $p$ denotes the p-value. The analysis is conducted at the simulation-run level. The dependent variable is total profit measured in million euros. Models are estimated using robust linear regression via \\texttt{lmrob}. Decision-making architecture is dummy-coded, with centralized architecture as the omitted reference category. Low demand volatility (0.1) and low market noise (0.03) are omitted reference categories for the controls.",
  "\\end{minipage}",
  "\\end{table}"
)

cat(paste(latex_model, collapse = "\n"))

## 10. Correlation matrix of model predictors ---------------------------------
x_matrix <- model.matrix(
  ~ architecture + demand_volatility + market_noise,
  data = runlevel
)

x_matrix <- x_matrix[, colnames(x_matrix) != "(Intercept)"]

clean_names <- c(
  "Independent architecture",
  "Rule-based architecture",
  "Sequential architecture",
  "Supervised architecture",
  "High demand volatility",
  "High market noise"
)

colnames(x_matrix) <- clean_names

cor_matrix <- cor(x_matrix)
cor_matrix_round <- round(cor_matrix, 3)

cor_matrix_round

latex_cor <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Correlation Matrix of Model Predictors}",
  "\\label{tab:correlation_profit_model}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lrrrrrr}",
  "\\hline\\hline",
  paste(c("Predictor", clean_names), collapse = " & "),
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
  "\\textbf{Note.} The correlation matrix is based on the dummy-coded predictors used in the final robust regression model. Centralized architecture, low demand volatility, and low market noise are omitted reference categories.",
  "\\end{minipage}",
  "\\end{table}"
)

cat("\n\n")
cat(paste(latex_cor, collapse = "\n"))

## 11. Estimated architecture means and pairwise contrasts --------------------
b <- coef(model_profit_robust_2)
V <- vcov(model_profit_robust_2)

arch_levels <- levels(runlevel$architecture)

X_arch <- model.matrix(
  ~ architecture + demand_volatility + market_noise,
  data = data.frame(
    architecture = factor(
      arch_levels,
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
)

arch_means <- as.vector(X_arch %*% b)
names(arch_means) <- arch_levels

arch_means

pairwise_results <- data.frame()

for (i in 1:(nrow(X_arch) - 1)) {
  for (j in (i + 1):nrow(X_arch)) {
    contrast_vec <- X_arch[i, ] - X_arch[j, ]

    estimate <- as.numeric(contrast_vec %*% b)
    se <- sqrt(as.numeric(contrast_vec %*% V %*% contrast_vec))
    t_value <- estimate / se
    p_value <- 2 * pt(
      abs(t_value),
      df = model_profit_robust_2$df.residual,
      lower.tail = FALSE
    )

    pairwise_results <- rbind(
      pairwise_results,
      data.frame(
        contrast = paste(arch_levels[i], "-", arch_levels[j]),
        estimate = estimate,
        se = se,
        t = t_value,
        p = p_value
      )
    )
  }
}

pairwise_results$p_holm <- p.adjust(pairwise_results$p, method = "holm")

pairwise_results
pairwise_results[order(pairwise_results$p_holm), ]

## 12. Pairwise contrast LaTeX table ------------------------------------------
contrast_labels <- c(
  "centralized - independent" = "Centralized -- Independent",
  "centralized - rule_based" = "Centralized -- Rule-based",
  "centralized - sequential" = "Centralized -- Sequential",
  "centralized - supervised" = "Centralized -- Supervised",
  "independent - rule_based" = "Independent -- Rule-based",
  "independent - sequential" = "Independent -- Sequential",
  "independent - supervised" = "Independent -- Supervised",
  "rule_based - sequential" = "Rule-based -- Sequential",
  "rule_based - supervised" = "Rule-based -- Supervised",
  "sequential - supervised" = "Sequential -- Supervised"
)

pairwise_table <- pairwise_results
pairwise_table$contrast_label <- contrast_labels[pairwise_table$contrast]

latex_pairwise <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Pairwise Contrasts Between Decision-Making Architectures}",
  "\\label{tab:pairwise_architecture_contrasts}",
  "\\begin{tabular}{lrrrrr}",
  "\\hline\\hline",
  "Contrast & Est. & SE & $t$ & $p$ & Holm-adjusted $p$ \\\\",
  "\\hline"
)

for (i in seq_len(nrow(pairwise_table))) {
  latex_pairwise <- c(
    latex_pairwise,
    paste0(
      pairwise_table$contrast_label[i], " & ",
      fmt_num(pairwise_table$estimate[i]), " & ",
      fmt_num(pairwise_table$se[i]), " & ",
      fmt_num(pairwise_table$t[i]), " & ",
      fmt_p(pairwise_table$p[i]), " & ",
      fmt_p(pairwise_table$p_holm[i]),
      " \\\\"
    )
  )
}

latex_pairwise <- c(
  latex_pairwise,
  "\\hline\\hline",
  "\\end{tabular}",
  "\\vspace{0.15cm}",
  "\\begin{minipage}{0.90\\textwidth}",
  "\\footnotesize",
  "\\textbf{Note.} Pairwise contrasts are computed from the final robust regression model with demand volatility and market noise controls. Estimates indicate differences in average total profit measured in million euros. Positive estimates indicate that the first architecture in the contrast has higher average total profit than the second architecture. Holm-adjusted $p$-values account for multiple comparisons across architecture contrasts.",
  "\\end{minipage}",
  "\\end{table}"
)

cat("\n\n")
cat(paste(latex_pairwise, collapse = "\n"))

