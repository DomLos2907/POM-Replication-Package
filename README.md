# POM Replication Package

This repository contains the R code used to reproduce the empirical analyses for the POM decision-making architecture study.

## Repository Structure

- `scripts/00_setup.R`: package loading and shared paths.
- `scripts/01_run_level_profit.R`: run-level average total profit analysis, model diagnostics, and appendix table generation.
- `scripts/02_period_level_profit.R`: period-level mixed-effects models for period profit.
- `scripts/03_variability_models.R`: variability, downside risk, and two-stage consistency analyses.
- `appendix/model_a1_diagnostics.tex`: LaTeX appendix text for run-level profit diagnostics.
- `data/raw/`: place the raw Excel workbooks here before running the scripts.
- `figures/`: generated figures are written here.
- `tables/`: generated LaTeX table snippets are written here.

## Required Data

The scripts expect the following files:

- `data/raw/POM_RunLevel_Dataset.xlsx`
- `data/raw/POM_Experiment_Data.xlsx`

The second file corresponds to the workbook currently named `POM_Experiment_Data Kopie.xlsx`. Rename it to `POM_Experiment_Data.xlsx` when placing it in `data/raw/`.

## R Packages

Install the required packages once:

```r
install.packages(c(
  "readxl",
  "lme4",
  "lmerTest",
  "robustbase",
  "robustlmm",
  "sjPlot",
  "ggplot2",
  "ggeffects"
))
```

## Reproduction Order

Run the scripts in this order:

```r
source("scripts/00_setup.R")
source("scripts/01_run_level_profit.R")
source("scripts/02_period_level_profit.R")
source("scripts/03_variability_models.R")
```

The scripts create appendix-ready figures and LaTeX table snippets in `figures/` and `tables/`.

