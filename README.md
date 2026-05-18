# Bangladesh GDP Regression Analysis

This repository contains R scripts and Quarto documents for analyzing and visualizing actual vs. predicted real GDP growth for Bangladesh. 

## Project Structure
* `scripts/`: Contains the core R functions for data preparation and regression plotting (`sfn_data_prep`, `sfn_gdp_reg`, `sfn_plot_reg`).
* `data/`: Processed datasets.
* `index.qmd`: Quarto document generating the final report and visualizations.

## Visualizations
The code generates golden-ratio formatted regression plots using `ggplot2` and `patchwork`, comparing historical growth periods (e.g., 1980-2000, 2004-2015, and 2016-2024).

## Requirements
* R
* `ggplot2`
* `patchwork`
* `dplyr`
* `tidyr`
