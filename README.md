# Census Studio

An ArcGIS Pro Python Toolbox for downloading, analyzing, and visualizing American Community Survey (ACS) census data.

## Overview

Census Studio provides a user-friendly interface within ArcGIS Pro to access U.S. Census Bureau data via the tidycensus R package. It handles the complexity of Census API calls, variable selection, and geographic boundary downloads while outputting ready-to-use feature classes.

## Features

- **Variable Browser**: Browse ACS variables by category and table with human-readable labels
- **Multi-Geography Support**: Download data at state, county, tract, block group, place, ZCTA, and other levels
- **Multi-State Downloads**: Select multiple states and filter by specific counties across states
- **Automatic Geometry**: Downloads and attaches Census TIGER boundary files
- **MOE Support**: Includes margin of error fields with configurable confidence levels
- **Time Comparisons**: Compare variables across different time periods with boundary interpolation
- **Spatial Analysis**: Built-in hot spot analysis (Getis-Ord Gi*) with MOE-aware significance adjustment

## Requirements

### Software
- ArcGIS Pro 3.x
- R 4.x ([Download](https://cran.r-project.org/))

### R Packages
```r
install.packages(c("tidycensus", "sf", "dplyr", "jsonlite", "spdep"))
```

### Census API Key
1. Request a free API key at [census.gov/developers](https://api.census.gov/data/key_signup.html)
2. Run the **Set Census API Key** tool in the toolbox to configure it

## Installation

1. Clone or download this repository
2. In ArcGIS Pro, open the Catalog pane
3. Right-click on Toolboxes > Add Toolbox
4. Navigate to `Census_Studio.pyt`

## Tools

### Census Data
| Tool | Description |
|------|-------------|
| **Download ACS Census Data** | Download ACS data with geometry as a feature class |
| **Join ACS Data to Features** | Join Census data to existing polygons using spatial interpolation |
| **Compare Time Periods** | Compare variables across different years with boundary change handling |

### Analysis
| Tool | Description |
|------|-------------|
| **Hot Spot Analysis** | Getis-Ord Gi* clustering with MOE reliability adjustment |
| **Demographic Profile Report** | Generate comparison reports (study area vs county/state/nation) |
| **Normalize Data** | Calculate per capita, density, and percentage fields |
| **MOE Reliability Filter** | Filter features by estimate reliability (CV thresholds) |

### Configuration
| Tool | Description |
|------|-------------|
| **Generate Variable Lookup File** | Regenerate variable browser data for a specific year/survey |
| **Set Census API Key** | Configure your Census API key |

## Usage

### Basic Download
1. Open the **Download ACS Census Data** tool
2. Select Year and Survey (ACS 5-Year or 1-Year)
3. Browse variables by Category > Table > Variables, or enter codes manually
4. Choose Geography Level and State(s)
5. Optionally filter by County
6. Set output location (must be a geodatabase feature class)
7. Run

### Variable Codes
Variables follow Census Bureau naming conventions:
- `B01001_001` - Total population
- `B19013_001` - Median household income
- `B25077_001` - Median home value

Use the variable browser or find codes at [data.census.gov](https://data.census.gov/).

## Data Notes

- **ACS 5-Year**: Available for all geographies, less current (5-year rolling average)
- **ACS 1-Year**: More current, only available for areas with 65,000+ population
- **Margin of Error**: All ACS estimates include MOE fields. High CV (>40%) indicates low reliability.
- **County FIPS**: For ambiguous county names (e.g., "Yellowstone"), use the 3-digit FIPS code shown in the dropdown.

## Project Structure

```
Scripts/
├── Census_Studio.pyt      # Main toolbox
├── acs_download.R         # Data download script
├── acs_compare_time.R     # Time comparison script
├── acs_join_data.R        # Spatial join script
├── acs_demographic_profile.R
├── acs_hotspot_analysis.R
├── generate_acs_lookup.R  # Variable lookup generator
├── generate_county_lookup.R
├── generate_all_lookups.R
└── lookups/
    ├── acs_variable_lookup_YYYY_surveyN.json
    └── us_counties.json
```

## License

MIT License

## Acknowledgments

- [tidycensus](https://walker-data.com/tidycensus/) by Kyle Walker
- U.S. Census Bureau for data and API access
