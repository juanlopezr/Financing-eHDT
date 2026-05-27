# Financing eHDT — Replication code

Replication code and input data for the analyses reported in the paper: *Financing Road Freight Electrification: Modeling the Total Cost of Ownership of Battery-Electric Trucks under Residual Value and Interest Rate Uncertainties*.

## Authors

- **Juan C. Lopez** *(Corresponding author)* — <jclopezr@ucdavis.edu>
- **Andreas Kopf** — <andreas.kopf@itf-oecd.org>
- **Matteo Craglia** — <mcraglia@ucdavis.edu>
- **Miguel Jaller** — <mjaller@ucdavis.edu>

## Repository layout

```
.
├── README.md
├── .gitignore
├── TCO-Modeling-Clean.Rmd                  # Total Cost of Ownership
├── Raroc-Modeling-Clean.Rmd                # Lender RAROC / interest-rate model
├── Raroc_Region-Clean.Rmd                  # Regional RAROC variations
├── Government_expense_cumulative-Clean.Rmd # Government expenditure + sensitivity
└── Inputs/
    ├── BET-Price-IEA_adjusted.csv          # BET retail prices (IEA, ICCT-adjusted)
    ├── BET-Price.csv                       # BET retail prices (ICCT only)
    ├── PD_RR_Regions.csv                   # EIB regional PD / recovery rates (TCO model)
    ├── VKT_perYear.csv                     # Annual vehicle kilometres travelled
    └── RAROC/
        ├── EIB_Data_Regional Difference.R  # Builds PD_WB.rds, RecoveryRate_WB(-2).rds
        ├── Regional Difference.R           # Builds OperationalCost_WB.rds, CentralBank_WB.rds
        ├── EIB_PD_RecoveryRates.xlsx       # Input for EIB script
        ├── CB_interest_rates.csv           # Input for Regional Difference script
        ├── World_bank_regions_adjusted.csv # World Bank region mapping
        ├── ISO_converter.csv               # ISO3 ↔ region/country mapping
        ├── Operational Costs/              # World Bank Global Financial Development export
        ├── PD_WB.rds                       # Pre-built outputs consumed by the Rmds
        ├── RecoveryRate_WB.rds
        ├── CentralBank_WB.rds
        └── OperationalCost_WB.rds
```

## Requirements

- **R** ≥ 4.2
- **RStudio** recommended (the working-directory anchor uses `rstudioapi` when interactive)
- R packages:

```r
install.packages(c(
  "dplyr", "tidyverse", "reshape", "reshape2", "ggplot2",
  "nlme", "Hmisc", "stats", "scales", "FinCal", "waterfalls",
  "patchwork", "svglite", "cowplot", "grid", "gridGraphics",
  "readxl", "plyr", "ggpattern"
))
```

## Citation

If you use this code, please cite the accompanying paper (see paper repository / journal record for full citation).

## License

This work is licensed under a **Creative Commons Attribution 4.0 International License (CC-BY-4.0)** — <https://creativecommons.org/licenses/by/4.0/>.
