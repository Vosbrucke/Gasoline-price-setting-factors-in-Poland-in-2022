# Gasoline price-setting factors in Poland in 2022

This repository contains the analysis and reproducible code for examining the main drivers behind the sharp increase in Pb95 gasoline prices in Poland in 2022. The project combines source data, modular R processing functions, and plotting to explain how Brent Oil prices, FX rates, taxes, margins, market conditions affected retail fuel prices.

The project is organized around a clean separation between:

- reusable data-processing functions in `R/`
- generated data in `Data/Processed/`
- raw inputs in `Data/Raw/`
- final visual outputs in `Plots/`
- the narrative analysis in `ANALYSIS.md`

---

## Repository structure

```text
.
├── ANALYSIS.md                 # narrative explanation of the project and findings
├── README.md                   # project overview and usage instructions
├── LICENSE
├── analysis.R                  # exploratory analysis script
├── Data/
│   ├── Processed/              # cleaned / generated datasets used by analysis and plots
│   │   ├── full_data.csv
│   │   ├── fuel_price_EU.csv
│   │   ├── countries_eu_inflation.csv
│   │   ├── inflation_countries.csv
│   │   ├── slim-2_pl.csv
│   │   └── special_aggregates.csv
│   └── Raw/                   # raw source files and externally sourced data
│       └── ...
├── Plots/                     # generated charts and map exports
├── R/
│   ├── load_required_libraries.R
│   ├── process_data_functions.R
│   ├── visualisation_functions.R
│   ├── inflation_chart.R
│   ├── univariate_map_eu.R
│   ├── bivariate_map_eu.R
│   └── ...
└── .gitignore
```

---

## Project goal

The analysis aims to separate the observed retail price of Pb95 into its main components and assess which factors were responsible for the market turmoil in 2022. In practical terms, the project studies:

- Brent crude oil price developments
- USD/PLN exchange rate effects
- wholesale fuel prices
- excise taxes, VAT, and fees
- station margins and market structure
- EU-wide comparison of fuel-price dynamics
- inflation and cross-country context

---

## Code organization

The project follows a modular structure that keeps the analysis readable and maintainable.

### `R/` folder

This folder contains the reusable logic used throughout the project.

- `load_required_libraries.R`  
  Installs missing packages and loads the libraries used across the project.

- `process_data_functions.R`  
  Contains the ETL-style functions that read raw data, build the model dataset, and write processed outputs.

- `visualisation_functions.R`  
  Contains reusable plotting functions for price charts, more detailed decomposition charts, and map visualizations.

- `inflation_chart.R`  
  Contains the inflation chart logic used for time-series comparison across countries.

- `univariate_map_eu.R` and `bivariate_map_eu.R`  
  Helper functions for European map visualizations.

### Data folders

- `Data/Raw/` contains source datasets and external raw inputs.
- `Data/Processed/` contains the cleaned CSV files used downstream in the analysis and plotting workflow.

---

## Entry points

The main workflow is intentionally thin and simple:

```r
source("R/load_required_libraries.R")
source("R/process_data_functions.R")
source("R/visualisation_functions.R")

process_data()

# generate core plots
generate_gasoil_visualisations()
```

Additional visualization entry points include:

```r
source("R/load_required_libraries.R")
source("R/process_data_functions.R")
source("R/visualisation_functions.R")

generate_map_visualisations()
generate_inflation_visualisations()
```

These functions orchestrate the plotting workflow and keep the project easy to rerun when the underlying data changes.

---

## Main functions

### Data preparation

- `resolve_repo_root()`  
  Resolves the project root regardless of the current working directory.

- `process_data()`  
  Runs the project pipeline to build the processed outputs if they are missing.

- `read_bankier_pb95_prices()`  
  Reads the Bankier Pb95 series.

- `read_usd_pln_weekly()`  
  Reads the weekly USD/PLN series.

- `read_brent_weekly()`  
  Reads the weekly Brent price data.

- `read_wholesale_fuel_prices()`  
  Reads wholesale fuel price inputs.

- `attach_price_factors()`  
  Joins the tax, fee, and margin decomposition to the model dataset.

### Visual analysis

- `read_plot_data()`  
  Loads the processed dataset for plotting.

- `plot_model_price_comparison()`  
  Compares modelled and observed retail prices with major drivers.

- `plot_wholesale_vs_brent_index()`  
  Compares the wholesale index against Brent in PLN.

- `plot_price_factor_shares()`  
  Shows the contribution of each component to the final price.

- `plot_brent_in_pln()`  
  Plots Brent in PLN and highlights the distribution.

- `generate_gasoil_visualisations()`  
  Builds the main gasoil-related visual outputs.

- `generate_inflation_visualisations()`  
  Builds the inflation-related plots and comparisons.

- `generate_map_visualisations()`  
  Builds the map-based analyses for Europe.

---

## Reproducibility and best practices

This project follows several good repository practices:

- code is separated into reusable functions instead of one-off scripts
- source data and processed outputs are kept in dedicated folders
- plotting and modelling logic are separated from data preparation
- functions are documented inline with roxygen-style comments
- the top-level project structure is kept simple and readable
- the workflow can be rerun without manually editing the code for folder changes

---

## Workflow

```r
# 1) load dependencies
source("R/load_required_libraries.R")

# 2) source the project functions
source("R/process_data_functions.R")
source("R/visualisation_functions.R")

# 3) generate or refresh processed data
process_data()

# 4) create charts
generate_gasoil_visualisations()
generate_map_visualisations()
generate_inflation_visualisations()
```

---

## Related documentation

- [ANALYSIS.md](ANALYSIS.md) contains the narrative and interpretation of the results.
- [R/README_functions.md](R/README_functions.md) contains a function-level index for the reusable R implementation.

This project is designed to make the analysis easy to understand, easy to rerun, and easy to extend with new data or additional visualizations.
