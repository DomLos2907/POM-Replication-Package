## Appendix A.1: Run-Level Profit Model Diagnostics ---------------------------
## Standalone script for reproducing Appendix A.1.
## Reference category: rule-based architecture

library(readxl)
library(lme4)
library(lmerTest)

## Import data ----------------------------------------------------------------
project_data_path <- "data/raw/POM_RunLevel_Dataset.xlsx"
desktop_data_path <- "C:/Users/loske/Desktop/POM_RunLevel_Dataset.xlsx"

if (file.exists(project_data_path)) {
  runlevel <- read_excel(project_data_path)
} else {
  runlevel <- read_excel(desktop_data_path)
}

## Variable preparation -------------------------------------------------------
runlevel$total_profit_mio <- runlevel$total_profit / 1000000

runlevel$architecture <- factor(runlevel$architecture)
runlevel$demand_volatility <- factor(runlevel$demand_volatility)
runlevel$market_noise <- factor(runlevel$market_noise)
runlevel$replication <- factor(runlevel$replication)

## Main change: rule-based architecture is the omitted reference category
runlevel$architecture <- relevel(runlevel$architecture, ref = "rule_based")
runlevel$demand_volatility <- relevel(runlevel$demand_volatility, ref = "0.1")
runlevel$market_noise <- relevel(runlevel$market_noise, ref = "0.03")

## Models ---------------------------------------------------------------------

model_A1_lm <- lm(
  total_profit_mio ~ architecture + demand_volatility + market_noise,
  data = runlevel
)

model_A1_lmer <- lmer(
  total_profit_mio ~ architecture + demand_volatility + market_noise + (1 | replication),
  data = runlevel,
  REML = FALSE
)

summary(model_A1_lm)
summary(model_A1_lmer)

## Random-effect variance -----------------------------------------------------

replication_variance <- as.data.frame(VarCorr(model_A1_lmer))$vcov[1]

## Q-Q plots for appendix, saved to Desktop as PNG ----------------------------

png(
  "C:/Users/loske/Desktop/Appendix_A1_QQ_Linear_Model.png",
  width = 1600,
  height = 1100,
  res = 200
)
qqnorm(
  resid(model_A1_lm),
  main = "Normal Q-Q Plot: Linear Model"
)
qqline(resid(model_A1_lm), col = "red")
dev.off()

png(
  "C:/Users/loske/Desktop/Appendix_A1_QQ_Mixed_Effects_Model.png",
  width = 1600,
  height = 1100,
  res = 200
)
qqnorm(
  resid(model_A1_lmer),
  main = "Normal Q-Q Plot: Mixed-Effects Model"
)
qqline(resid(model_A1_lmer), col = "red")
dev.off()

## Helper functions -----------------------------------------------------------

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

extract_model <- function(model) {
  coefs <- as.data.frame(summary(model)$coefficients)
  coefs$term <- rownames(coefs)
  rownames(coefs) <- NULL
  
  p_col <- grep("Pr", names(coefs), value = TRUE)
  stat_col <- grep("t value", names(coefs), value = TRUE)
  
  data.frame(
    term = coefs$term,
    estimate = fmt_num(coefs$Estimate),
    se = fmt_num(coefs$`Std. Error`),
    statistic = fmt_num(coefs[[stat_col]]),
    p = fmt_p(coefs[[p_col]])
  )
}

## Extract model results ------------------------------------------------------

lm_tab <- extract_model(model_A1_lm)
lmer_tab <- extract_model(model_A1_lmer)

## Paper-ready variable labels ------------------------------------------------

labels <- c(
  "(Intercept)" = "Constant",
  "architecturecentralized" = "Centralized architecture",
  "architectureindependent" = "Independent architecture",
  "architecturesequential" = "Sequential architecture",
  "architecturesupervised" = "Supervised architecture",
  "demand_volatility0.25" = "High demand volatility",
  "market_noise0.1" = "High market noise"
)

terms <- names(labels)

lm_tab <- lm_tab[match(terms, lm_tab$term), ]
lmer_tab <- lmer_tab[match(terms, lmer_tab$term), ]

## Build LaTeX table ----------------------------------------------------------

latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\scriptsize",
  "\\caption{Run-Level Profit Model Diagnostics}",
  "\\label{tab:ModelA1}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccccc}",
  "\\hline\\hline",
  " & \\multicolumn{4}{c}{Linear model} & \\multicolumn{4}{c}{Mixed-effects model} \\\\",
  "\\cline{2-5} \\cline{6-9}",
  "Predictors & Est. & SE & $t$ & $p$ & Est. & SE & $t$ & $p$ \\\\",
  "\\hline"
)

for (i in seq_along(terms)) {
  latex_lines <- c(
    latex_lines,
    paste0(
      labels[terms[i]], " & ",
      lm_tab$estimate[i], " & ",
      lm_tab$se[i], " & ",
      lm_tab$statistic[i], " & ",
      lm_tab$p[i], " & ",
      lmer_tab$estimate[i], " & ",
      lmer_tab$se[i], " & ",
      lmer_tab$statistic[i], " & ",
      lmer_tab$p[i],
      " \\\\"
    )
  )
}

latex_lines <- c(
  latex_lines,
  "\\hline",
  paste0(
    "Observations & \\multicolumn{4}{c}{",
    nobs(model_A1_lm),
    "} & \\multicolumn{4}{c}{",
    nobs(model_A1_lmer),
    "} \\\\"
  ),
  paste0(
    "$R^2$ / Adjusted $R^2$ & \\multicolumn{4}{c}{",
    sprintf(
      "%.3f / %.3f",
      summary(model_A1_lm)$r.squared,
      summary(model_A1_lm)$adj.r.squared
    ),
    "} & \\multicolumn{4}{c}{--} \\\\"
  ),
  paste0(
    "Replication random-intercept variance & \\multicolumn{4}{c}{--} & \\multicolumn{4}{c}{",
    sprintf("%.4f", replication_variance),
    "} \\\\"
  ),
  "\\hline\\hline",
  "\\multicolumn{9}{l}{\\footnotesize Reference categories: rule-based architecture, low demand volatility, and low market noise.} \\\\",
  "\\end{tabular}%",
  "}",
  "\\end{table}"
)

## Print LaTeX code to console ------------------------------------------------

cat(paste(latex_lines, collapse = "\n"))
