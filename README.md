# Are natural disasters political events for municipalities?

**Carla Morvan** (CEE-M, Univ. Montpellier, CNRS, INRAe, Institut Agro)

**Sonia Paty** (Université Lyon 2, CNRS, GATE Lyon Saint-Étienne)

## Abstract

This paper provides new evidence on the role of political alignment in natural disaster relief and its electoral reward at the municipal level. We exploit an original dataset on natural disasters and French municipalities between 2008 and 2020. Using a difference-in-differences strategy, we find that political alignment between the local incumbent and the central government significantly increases the probability of obtaining a natural disaster declaration from the state. We also apply a Heckman selection model to assess whether citizens reward incumbents who obtained a declaration: although a natural disaster reduces the probability of reelection, this negative effect is smaller when the disaster declaration is granted by the state.

**Keywords:** Political alignment; natural disasters; quasi-natural experiment.
**JEL:** D72, Q54

---

## Repository structure

```
repository/
├── README.md
├── data/
│   └── README_data.md       # Data sources and variable descriptions
└── code/
    ├── 01_build_panels.R    # Build all analysis panels from raw data
    ├── 02_did_period1.R     # DiD + robustness, Period 1 (2008-2014)
    ├── 03_did_period2.R     # DiD + robustness, Period 2 (2014-2020)
    ├── 04_heckman.R         # Heckman selection model + validity tests
    └── 05_descriptive.Rmd   # Descriptive statistics and figures
```

## How to reproduce

**Step 1 — Set up paths.**
Each script contains a `PATH_ROOT` variable at the top. Set it to your local data directory before running.

**Step 2 — Download raw data.**
All raw data sources are publicly available — see `data/README_data.md` for download links and file names.

**Step 3 — Run scripts in order.**

```r
source("code/01_build_panels.R")
source("code/02_did_period1.R")
source("code/03_did_period2.R")
source("code/04_heckman.R")
```

Then knit `code/05_descriptive.Rmd` for descriptive statistics and figures.

## Dependencies

```r
install.packages(c(
  "dplyr", "tidyr", "stringr", "readr", "readxl", "lubridate",
  "data.table", "stringi", "jsonlite", "zoo",
  "fixest", "MatchIt", "sampleSelection",
  "ggplot2", "broom", "ggtext", "scales", "viridis", "sf",
  "texreg", "car", "boot", "knitr"
))
```

## Data availability

Raw data are not distributed in this repository. All sources are publicly available — see `data/README_data.md` for details. Electoral data for mayors were constructed from files published by the French Ministry of the Interior and are available upon request.

