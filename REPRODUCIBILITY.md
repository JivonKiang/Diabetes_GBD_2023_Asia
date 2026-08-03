# Reproducibility map

This repository is a public code release. The underlying GBD extracts and
third-party hierarchy files are obtained separately under their own terms.

## Expected inputs

The current scripts expect an authorised local data directory containing the
following workflow inputs:

- `external_publications/hierarchies/location_GBD2021.csv`
- the age-standardised-rate CSV under
  `1-Age-Standardized Rate (ASR) for four indicators/`
- the trend CSV under `2-TPC for four indicators/`
- the joinpoint CSV under `3-Jointpoint regression analysis/`
- intermediate Excel or CSV files consumed by scripts 4-7

Set `GBD_DATA_ROOT` for scripts 1 and 3. Scripts 4-7 consume intermediate
files created during the analysis workflow. Exact source-data access and use
must follow the applicable IHME terms.

## Analysis map

| Stage | Script | Main purpose |
| --- | --- | --- |
| 1 | `1. launch_lancet_table.R` | Regional summary tables and derived measures |
| 2 | `3. Jointpoint.R` | Trend and joinpoint analyses |
| 3 | `4. Country draw.R` | Country-level visualisation |
| 4 | `5. Age sex.R` | Age and sex stratification |
| 5 | `6. Risk.R` | Risk-factor attribution |
| 6 | `7. Predict.R` | Future burden scenarios |

The release does not claim that restricted source data are redistributed or
that a fresh checkout is a one-command reproduction without authorised input
data and the required R packages.
