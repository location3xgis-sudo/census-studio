# =============================================================================
# Generate US Counties Lookup File
# Creates a JSON file mapping state abbreviations to their counties with FIPS codes
# =============================================================================

suppressPackageStartupMessages({
  library(tidycensus)
  library(jsonlite)
  library(dplyr)
})

cat("Generating US Counties Lookup...\n")

# Get county-level data for all states using a simple variable
# This gives us the county names and FIPS codes
counties_data <- suppressMessages(
  get_acs(
    geography = "county",
    variables = "B01001_001",  # Total population (just need geography info)
    year = 2023,
    survey = "acs5",
    geometry = FALSE
  )
)

cat(paste("Retrieved", nrow(counties_data), "counties\n"))

# Parse GEOID to extract state and county FIPS
counties_data <- counties_data %>%
  mutate(
    state_fips = substr(GEOID, 1, 2),
    county_fips = substr(GEOID, 3, 5),
    # Extract county name (remove state suffix like ", Colorado")
    county_name = gsub(",.*$", "", NAME)
  )

# State FIPS to abbreviation mapping
state_fips_to_abbr <- c(
  "01" = "AL", "02" = "AK", "04" = "AZ", "05" = "AR", "06" = "CA",
  "08" = "CO", "09" = "CT", "10" = "DE", "11" = "DC", "12" = "FL",
  "13" = "GA", "15" = "HI", "16" = "ID", "17" = "IL", "18" = "IN",
  "19" = "IA", "20" = "KS", "21" = "KY", "22" = "LA", "23" = "ME",
  "24" = "MD", "25" = "MA", "26" = "MI", "27" = "MN", "28" = "MS",
  "29" = "MO", "30" = "MT", "31" = "NE", "32" = "NV", "33" = "NH",
  "34" = "NJ", "35" = "NM", "36" = "NY", "37" = "NC", "38" = "ND",
  "39" = "OH", "40" = "OK", "41" = "OR", "42" = "PA", "44" = "RI",
  "45" = "SC", "46" = "SD", "47" = "TN", "48" = "TX", "49" = "UT",
  "50" = "VT", "51" = "VA", "53" = "WA", "54" = "WV", "55" = "WI",
  "56" = "WY", "72" = "PR"
)

# Add state abbreviation
counties_data <- counties_data %>%
  mutate(state_abbr = state_fips_to_abbr[state_fips]) %>%
  filter(!is.na(state_abbr))  # Remove any territories not in our mapping

# Build the lookup structure
cat("Building lookup structure...\n")

county_lookup <- list()
for (state in unique(counties_data$state_abbr)) {
  state_counties <- counties_data %>%
    filter(state_abbr == state) %>%
    arrange(county_name) %>%
    select(name = county_name, fips = county_fips) %>%
    as.data.frame()

  county_lookup[[state]] <- state_counties
}

# Sort by state abbreviation
county_lookup <- county_lookup[sort(names(county_lookup))]

# Write to JSON
output_file <- "lookups/us_counties.json"

cat(paste("Writing to", output_file, "...\n"))
json_output <- toJSON(county_lookup, pretty = TRUE, auto_unbox = FALSE)
writeLines(json_output, output_file)

# Summary
cat("\n=== Summary ===\n")
cat(paste("States:", length(county_lookup), "\n"))
cat(paste("Total counties:", nrow(counties_data), "\n"))
cat(paste("Output file:", output_file, "\n"))
cat("Done!\n")
