# Diabetes GBD 2023 — Asia-Pacific Analysis

## Overview

Analysis of the **Global Burden of Diabetes in the Asia-Pacific region** using GBD 2023 data. The project covers temporal trends, joinpoint regression, country-level comparisons, age-sex stratification, risk factor analysis, and future burden prediction.

## Analysis Pipeline

| Step | Script | Description |
|------|--------|-------------|
| 1 | `1. launch_lancet_table.R` | Generate Lancet-format summary tables |
| 3 | `3. Jointpoint.R` | Joinpoint regression for trend detection |
| 4 | `4. Country draw.R` | Country-level burden mapping |
| 5 | `5. Age sex.R` | Age and sex stratified analysis |
| 6 | `6. Risk.R` | Risk factor attribution |
| 7 | `7. Predict.R` | Future burden forecasting (BAPC, etc.) |
| 8 | `8. Bar plot.docx` | Bar chart templates for publications |

## Key Features

- **Asia-Pacific Focus** — Regional and country-level analysis
- **Joinpoint Regression** — Detecting significant trend changes
- **BAPC Prediction** — Bayesian age-period-cohort forecasting
- **Lancet-format Tables** — Publication-ready summary tables

## Requirements

- R ≥ 4.0
- Key packages: `ggplot2`, `BAPC`, `dplyr`

## License

Research use only.
