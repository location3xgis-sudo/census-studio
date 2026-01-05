# =============================================================================
# Generate ACS Variable Lookup File
# 
# This script creates a JSON lookup file containing all ACS variables
# organized by category and table for use in the ArcGIS Pro toolbox.
#
# Can be run standalone or called from ArcGIS Pro toolbox via command line.
# =============================================================================

# Category assignment function
assign_category <- function(table_code, concept) {
  code <- toupper(table_code)
  concept_lower <- tolower(concept)
  
  # B-tables (Detailed Tables)
  if (grepl("^B01", code)) return("Demographics - Age & Sex")
  if (grepl("^B02", code)) return("Demographics - Race")
  if (grepl("^B03", code)) return("Demographics - Hispanic Origin")
  if (grepl("^B04", code)) return("Demographics - Ancestry")
  if (grepl("^B05", code)) return("Demographics - Citizenship & Immigration")
  if (grepl("^B06", code)) return("Demographics - Place of Birth & Mobility")
  if (grepl("^B07", code)) return("Demographics - Migration")
  if (grepl("^B08", code)) return("Transportation - Commuting")
  if (grepl("^B09", code)) return("Demographics - Children & Household Relationships")
  if (grepl("^B10", code)) return("Demographics - Grandparents & Grandchildren")
  if (grepl("^B11", code)) return("Demographics - Household Type & Family")
  if (grepl("^B12", code)) return("Demographics - Marital Status")
  if (grepl("^B13", code)) return("Demographics - Fertility")
  if (grepl("^B14", code)) return("Education - School Enrollment")
  if (grepl("^B15", code)) return("Education - Educational Attainment")
  if (grepl("^B16", code)) return("Demographics - Language")
  if (grepl("^B17", code)) return("Income & Poverty - Poverty Status")
  if (grepl("^B18", code)) return("Health - Disability Status")
  if (grepl("^B19", code)) return("Income & Poverty - Household Income")
  if (grepl("^B20", code)) return("Income & Poverty - Individual Earnings")
  if (grepl("^B21", code)) return("Demographics - Veteran Status")
  if (grepl("^B22", code)) return("Income & Poverty - Food Stamps/SNAP")
  if (grepl("^B23", code)) return("Employment - Labor Force Status")
  if (grepl("^B24", code)) return("Employment - Industry & Occupation")
  if (grepl("^B25", code)) return("Housing - Characteristics")
  if (grepl("^B26", code)) return("Demographics - Group Quarters")
  if (grepl("^B27", code)) return("Health - Health Insurance")
  if (grepl("^B28", code)) return("Technology - Computer & Internet Use")
  if (grepl("^B29", code)) return("Demographics - Citizen Voting Age")
  if (grepl("^B99", code)) return("Data Quality - Allocation Flags")
  
  # C-tables (Collapsed versions of B-tables)
  if (grepl("^C", code)) return("Summary Tables (Collapsed)")
  
  # S-tables (Subject Tables)
  if (grepl("^S", code)) {
    if (grepl("income|poverty|earnings", concept_lower)) return("Subject Tables - Income & Poverty")
    if (grepl("education|school", concept_lower)) return("Subject Tables - Education")
    if (grepl("housing|home|rent", concept_lower)) return("Subject Tables - Housing")
    if (grepl("employ|occupation|industry|work", concept_lower)) return("Subject Tables - Employment")
    if (grepl("health|insurance|disability", concept_lower)) return("Subject Tables - Health")
    if (grepl("transport|commut|vehicle", concept_lower)) return("Subject Tables - Transportation")
    return("Subject Tables - Other")
  }
  
  # DP-tables (Data Profiles)
  if (grepl("^DP", code)) return("Data Profiles")
  
  # CP-tables (Comparison Profiles)
  if (grepl("^CP", code)) return("Comparison Profiles")
  
  return("Other Tables")
}

# Main function
generate_lookup <- function(year, survey, output_file) {
  # Load required libraries
  if (!require("tidycensus", quietly = TRUE)) {
    stop("tidycensus package is required. Install with: install.packages('tidycensus')")
  }
  if (!require("dplyr", quietly = TRUE)) {
    stop("dplyr package is required. Install with: install.packages('dplyr')")
  }
  if (!require("jsonlite", quietly = TRUE)) {
    stop("jsonlite package is required. Install with: install.packages('jsonlite')")
  }
  
  cat(paste("Loading ACS", survey, "variables for", year, "...\n"))
  
  # Load all variables
  vars <- tryCatch({
    load_variables(year = year, dataset = survey)
  }, error = function(e) {
    stop(paste("Error loading variables:", e$message))
  })
  
  cat(paste("Retrieved", nrow(vars), "variables\n"))
  
  # Extract table code from variable name (e.g., B19013 from B19013_001)
  vars <- vars %>%
    mutate(
      table_code = gsub("_.*$", "", name),
      label_clean = gsub("^Estimate!!", "", label),
      label_clean = gsub("^Annotation of ", "", label_clean)
    )
  
  # Get unique tables with their concepts (table descriptions)
  tables <- vars %>%
    group_by(table_code) %>%
    summarize(
      concept = first(concept),
      variable_count = n(),
      .groups = "drop"
    ) %>%
    filter(!is.na(concept) & concept != "")
  
  cat(paste("Found", nrow(tables), "unique tables\n"))
  
  # Apply category assignments
  tables <- tables %>%
    rowwise() %>%
    mutate(category = assign_category(table_code, concept)) %>%
    ungroup()
  
  # Build the final lookup structure
  cat("Building lookup structure...\n")
  
  # Create variables list
  variables_df <- vars %>%
    select(name, label_clean, table_code) %>%
    rename(code = name, label = label_clean)
  
  # Create tables list
  tables_df <- tables %>%
    select(table_code, concept, category) %>%
    rename(code = table_code, label = concept)
  
  # Get unique categories
  categories <- sort(unique(tables_df$category))
  
  # Build nested structure
  lookup <- list(
    metadata = list(
      generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      year = year,
      survey = survey,
      total_variables = nrow(variables_df),
      total_tables = nrow(tables_df),
      total_categories = length(categories)
    ),
    categories = categories,
    tables = split(
      tables_df %>% select(code, label),
      tables_df$category
    ),
    variables = split(
      variables_df %>% select(code, label),
      variables_df$table_code
    )
  )
  
  # Convert to JSON and save
  cat(paste("Writing to", output_file, "...\n"))
  json_output <- toJSON(lookup, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_output, output_file)
  
  # Summary
  cat("\n")
  cat("=== Summary ===\n")
  cat(paste("Output file:", output_file, "\n"))
  cat(paste("Categories:", length(categories), "\n"))
  cat(paste("Tables:", nrow(tables_df), "\n"))
  cat(paste("Variables:", nrow(variables_df), "\n"))
  
  invisible(lookup)
}

# =============================================================================
# Main execution
# =============================================================================

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Parse arguments or use defaults
if (length(args) >= 3) {
  year <- as.integer(args[1])
  survey <- args[2]
  output_file <- args[3]
} else if (length(args) >= 2) {
  year <- as.integer(args[1])
  survey <- args[2]
  # Default to lookups subfolder
  if (!dir.exists("lookups")) dir.create("lookups")
  output_file <- file.path("lookups", paste0("acs_variable_lookup_", year, "_", survey, ".json"))
} else {
  year <- 2023
  survey <- "acs5"
  # Default to lookups subfolder
  if (!dir.exists("lookups")) dir.create("lookups")
  output_file <- file.path("lookups", paste0("acs_variable_lookup_", year, "_", survey, ".json"))
}

# Run the function
generate_lookup(year, survey, output_file)