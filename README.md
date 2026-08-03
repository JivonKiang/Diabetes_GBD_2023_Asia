# Diabetes GBD 2023 - Asia-Pacific Analysis

Public analysis code for the Asia-Pacific diabetes burden study using Global
Burden of Disease (GBD) 2023 estimates.

Maintainer: Fan Jiang (`JivonKiang`)

ORCID: <https://orcid.org/0000-0002-9257-206X>

## Associated publication

The repository supports the published article:

> Li Y, et al. East Asia and high-income Asia Pacific burden of diabetes
> mellitus from 1990 to 2023. *iScience* (2026).

DOI: <https://doi.org/10.1016/j.isci.2026.116590>

This is a publication-linked research-code release. It makes the analysis
workflow inspectable and reusable with authorised GBD data while keeping
restricted source data outside the repository.

## Scope

The workflow covers temporal trends, joinpoint regression, country-level
comparisons, age-sex stratification, risk-factor attribution, and future
burden prediction for East Asia and high-income Asia Pacific.

## Analysis pipeline

| Script | Purpose |
| --- | --- |
| `1. launch_lancet_table.R` | Generate regional summary tables |
| `3. Jointpoint.R` | Run joinpoint trend analyses |
| `4. Country draw.R` | Create country-level burden visualisations |
| `5. Age sex.R` | Analyse age and sex strata |
| `6. Risk.R` | Analyse risk-factor attribution |
| `7. Predict.R` | Produce future burden projections |
| `8. Bar plot.docx` | Publication figure template |

## Requirements

- R >= 4.0
- R packages used by the scripts, including `dplyr`, `ggplot2`, `openxlsx`,
  `tidyverse`, `BAPC`, and `nih.joinpoint`

## Data and reproducibility boundary

The repository is a code release. It does not redistribute raw or restricted
GBD extracts, individual-level data, or third-party source files. Obtain GBD
data directly from the authorised IHME source and follow the applicable data
use terms. Do not commit restricted or locally licensed input files.

The scripts use a local data directory. Set the `GBD_DATA_ROOT` environment
variable before running scripts 1 or 3; this directory must contain the input
folders expected by those scripts, including the required location hierarchy
file under `external_publications/hierarchies/`. Scripts 4-7 consume the
intermediate Excel or CSV files produced during the analysis workflow.

For example in PowerShell:

```powershell
$env:GBD_DATA_ROOT = "C:\path\to\your\authorised\gbd-data"
Rscript "1. launch_lancet_table.R"
Rscript "3. Jointpoint.R"
```

The repository intentionally does not include the author's local filesystem
paths or the underlying GBD extracts.

## Citation

Please cite the associated iScience article when using the code or reproducing
its analysis. See `CITATION.cff` for machine-readable citation metadata.

## License

Original source code is released under the MIT License. The MIT License does
not grant rights to IHME data, third-party software, publication text or
figures, or other material governed by separate terms. See
`DATA_LICENSE.md` for the data boundary.
