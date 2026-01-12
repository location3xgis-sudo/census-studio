# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Census Studio is an ArcGIS Pro Python Toolbox (.pyt) that provides tools for downloading, analyzing, and visualizing American Community Survey (ACS) census data. The toolbox uses R scripts (via tidycensus) for Census API interactions while providing a GUI through ArcGIS Pro.

## Architecture

**Python Toolbox (`Census_Studio.pyt`)**: ArcGIS Pro interface layer containing tool definitions, parameter handling, and R script orchestration. Each tool class defines parameters in `getParameterInfo()` and executes R scripts via subprocess in `execute()`.

**R Scripts**: Backend data processing:
- `acs_download.R` - Downloads ACS data via tidycensus get_acs()
- `acs_compare_time.R` - Compares variables across time periods with population-weighted interpolation for cross-decade boundary changes
- `acs_join_data.R` - Joins ACS data to user polygons (area or population weighted)
- `acs_demographic_profile.R` - Generates comparison reports (study area vs county/state/nation)
- `acs_hotspot_analysis.R` - Getis-Ord Gi* spatial clustering with MOE adjustment
- `generate_acs_lookup.R` - Creates variable lookup JSON files from Census API
- `generate_all_lookups.R` - Batch generates lookups for all year/survey combinations

**Lookup Files (`lookups/`)**: Pre-generated JSON files mapping ACS variables by year and survey type (acs1, acs5). Files are named `acs_variable_lookup_{year}_{survey}.json`.

## Key Patterns

### Parameter Passing
R scripts receive arguments via `commandArgs(trailingOnly = TRUE)`. Python passes arguments positionally:
```python
subprocess.run([rscript_path, script_path, arg1, arg2, ...])
```

### Output Formats
Scripts detect output format from file extension (.shp, .gpkg, .csv) and handle shapefile field name truncation (10 char limit) automatically.

### Census API Key
Stored in R's .Renviron file (`CENSUS_API_KEY`). Tools check `Sys.getenv("CENSUS_API_KEY")` before API calls.

### MOE (Margin of Error) Handling
Scripts calculate Coefficient of Variation (CV) for reliability classification:
- High: CV < 12%
- Medium: CV 12-40%
- Low: CV > 40%

### Cross-Decade Comparisons
When comparing data across census decades (e.g., 2019 vs 2021), boundary changes are handled via `interpolate_pw()` using block-level population weights.

## R Dependencies

Required packages: `tidycensus`, `sf`, `dplyr`, `jsonlite`, `spdep` (for hot spot analysis)

## Regenerating Lookup Files

```r
source("generate_all_lookups.R")
```
This generates lookup files for ACS 5-Year (2009-2023) and ACS 1-Year (2005-2023, excluding 2020).
