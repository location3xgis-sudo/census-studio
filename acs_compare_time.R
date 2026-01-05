# =============================================================================
# ACS Census Data Time Period Comparison Script
# Compares a single variable across two time periods
# Uses interpolate_pw() for cross-decade boundary changes
# =============================================================================

# --- Helper Functions ---

is_empty <- function(x) {
  is.null(x) || is.na(x) || (is.character(x) && nchar(trimws(x)) == 0)
}

parse_states <- function(state_str) {
  if (is_empty(state_str)) return(NULL)
  states <- trimws(strsplit(state_str, ",")[[1]])
  states <- states[nchar(states) > 0]
  if (length(states) == 0) return(NULL)
  if (length(states) == 1) return(states)
  return(states)
}

parse_counties <- function(county_str) {
  if (is_empty(county_str)) return(NULL)
  counties <- trimws(strsplit(county_str, ",")[[1]])
  counties <- counties[nchar(counties) > 0]
  if (length(counties) == 0) return(NULL)
  if (length(counties) == 1) return(counties)
  return(counties)
}

detect_output_format <- function(output_path) {
  path_lower <- tolower(output_path)
  if (grepl("\\.shp$", path_lower)) return("shapefile")
  if (grepl("\\.gpkg$", path_lower)) return("geopackage")
  return("shapefile")
}

truncate_shapefile_fields <- function(data) {
  original_names <- names(data)
  new_names <- character(length(original_names))
  used_names <- character()
  for (i in seq_along(original_names)) {
    orig <- original_names[i]
    if (orig == "geometry") { new_names[i] <- orig; next }
    if (nchar(orig) <= 10) {
      new_names[i] <- orig
      used_names <- c(used_names, toupper(orig))
      next
    }
    truncated <- substr(orig, 1, 10)
    counter <- 1
    base_truncated <- truncated
    while (toupper(truncated) %in% used_names) {
      suffix <- as.character(counter)
      truncated <- paste0(substr(base_truncated, 1, 10 - nchar(suffix)), suffix)
      counter <- counter + 1
    }
    new_names[i] <- truncated
    used_names <- c(used_names, toupper(truncated))
  }
  names(data) <- new_names
  return(data)
}

# Calculate statistical significance of change
# Returns: 1 (significant increase), -1 (significant decrease), 0 (not significant)
calc_significance <- function(est1, est2, moe1, moe2, confidence = 0.90) {
  # Handle NA values
  if (is.na(est1) || is.na(est2) || is.na(moe1) || is.na(moe2)) return(NA)
  
  # Z-score for confidence level
  z_score <- switch(as.character(confidence),
                    "0.90" = 1.645,
                    "0.95" = 1.96,
                    "0.99" = 2.576,
                    1.645
  )
  
  # Calculate MOE of the difference
  moe_diff <- sqrt(moe1^2 + moe2^2)
  
  # Calculate the difference
  diff <- est2 - est1
  
  # Calculate standard error of difference
  se_diff <- moe_diff / z_score
  
  # Avoid division by zero
  if (se_diff == 0) return(0)
  
  # Test statistic
  test_stat <- abs(diff) / se_diff
  
  # Determine significance
  result <- ifelse(test_stat >= z_score,
                   ifelse(diff > 0, 1, -1),
                   0)
  
  return(result)
}

# --- Main Script ---

args <- commandArgs(trailingOnly = TRUE)

# Parse arguments
year1 <- as.integer(args[1])
year2 <- as.integer(args[2])
survey <- args[3]
variable <- args[4]
var_type <- args[5]
geography <- args[6]
state <- parse_states(args[7])
county <- parse_counties(args[8])
moe_level <- as.numeric(args[9])
output_fc <- args[10]
truncate_fields <- if (length(args) >= 11) toupper(args[11]) %in% c("TRUE", "T", "1", "YES") else TRUE

cat("=============================================================================\n")
cat("ACS Time Period Comparison\n")
cat("=============================================================================\n")
cat(paste("Year 1:", year1, "\n"))
cat(paste("Year 2:", year2, "\n"))
cat(paste("Survey:", survey, "\n"))
cat(paste("Variable:", variable, "\n"))
cat(paste("Variable Type:", var_type, "\n"))
cat(paste("Geography:", geography, "\n"))
if (!is.null(state)) cat(paste("State(s):", paste(state, collapse = ", "), "\n"))
if (!is.null(county)) cat(paste("County(s):", paste(county, collapse = ", "), "\n"))
cat(paste("Output:", output_fc, "\n"))
cat("=============================================================================\n\n")

# Load required packages
cat("Loading required R packages...\n")
library(tidycensus)
library(sf)
library(dplyr)

if (Sys.getenv("CENSUS_API_KEY") == "") {
  stop("Census API key not found. Run 'Set Census API Key' tool first.")
}

# Determine if cross-decade comparison (boundary change)
decade1 <- floor(year1 / 10) * 10
decade2 <- floor(year2 / 10) * 10
cross_decade <- decade1 != decade2

if (cross_decade && geography %in% c("tract", "block group")) {
  cat(paste("\nNOTE: Cross-decade comparison detected (", decade1, "s vs ", decade2, "s).\n", sep = ""))
  cat("Boundary changes will be handled using population-weighted interpolation.\n\n")
}

# Build base parameters
base_params <- list(
  geography = geography,
  state = state,
  variables = variable,
  survey = survey,
  geometry = TRUE,
  output = "wide",
  moe_level = moe_level
)

if (!is.null(county)) base_params$county <- county

# Use full TIGER/Line files for ZCTA
if (geography == "zcta") {
  base_params$cb <- FALSE
}

# --- Download Year 1 Data ---
cat(paste("Downloading Year 1 data (", year1, ")...\n", sep = ""))
params_y1 <- base_params
params_y1$year <- year1

data_y1 <- tryCatch({
  do.call(get_acs, params_y1)
}, error = function(e) {
  err_msg <- e$message
  if (grepl("CB ZCTA file", err_msg, ignore.case = TRUE)) {
    stop(paste0("ZCTA geometry is not available for ", year1, ". Try using an earlier year."))
  }
  stop(paste("Error downloading Year 1 data:", err_msg))
})

cat(paste("Retrieved", nrow(data_y1), "records for Year 1\n\n"))

# --- Download Year 2 Data ---
cat(paste("Downloading Year 2 data (", year2, ")...\n", sep = ""))
params_y2 <- base_params
params_y2$year <- year2

data_y2 <- tryCatch({
  do.call(get_acs, params_y2)
}, error = function(e) {
  err_msg <- e$message
  if (grepl("CB ZCTA file", err_msg, ignore.case = TRUE)) {
    stop(paste0("ZCTA geometry is not available for ", year2, ". Try using an earlier year."))
  }
  stop(paste("Error downloading Year 2 data:", err_msg))
})

cat(paste("Retrieved", nrow(data_y2), "records for Year 2\n\n"))

# --- Handle Cross-Decade Boundary Changes ---
if (cross_decade && geography %in% c("tract", "block group")) {
  cat("Performing population-weighted interpolation for boundary alignment...\n")
  
  # Determine which decennial census to use for weights
  weight_year <- decade2
  
  cat(paste("Downloading", weight_year, "decennial census block population for weights...\n"))
  
  # Get block-level population weights
  weights <- tryCatch({
    get_decennial(
      geography = "block",
      variables = "P1_001N",
      state = state,
      county = county,
      year = weight_year,
      geometry = TRUE
    )
  }, error = function(e) {
    tryCatch({
      get_decennial(
        geography = "block",
        variables = "P001001",
        state = state,
        county = county,
        year = weight_year,
        geometry = TRUE
      )
    }, error = function(e2) {
      stop(paste("Could not download block population weights:", e2$message))
    })
  })
  
  cat(paste("Retrieved", nrow(weights), "blocks for weighting\n"))
  
  # Disable s2 spherical geometry (can cause issues with some Census geometries)
  sf_use_s2(FALSE)
  
  # Force repair all geometries (don't rely on st_is_valid check)
  cat("Repairing geometries...\n")
  data_y1 <- st_make_valid(data_y1)
  data_y2 <- st_make_valid(data_y2)
  weights <- st_make_valid(weights)
  
  # Remove any empty geometries
  data_y1 <- data_y1[!st_is_empty(data_y1), ]
  data_y2 <- data_y2[!st_is_empty(data_y2), ]
  weights <- weights[!st_is_empty(weights), ]
  
  cat(paste("  Year 1:", nrow(data_y1), "features\n"))
  cat(paste("  Year 2:", nrow(data_y2), "features\n"))
  cat(paste("  Weights:", nrow(weights), "features\n"))
  
  # Interpolate Year 1 data to Year 2 boundaries
  cat("Interpolating Year 1 data to Year 2 boundaries...\n")
  
  # Determine if extensive (count) or intensive (rate/median)
  is_extensive <- tolower(var_type) == "count"
  
  # Interpolate
  data_y1_interp <- tryCatch({
    interpolate_pw(
      from = data_y1,
      to = data_y2,
      to_id = "GEOID",
      extensive = is_extensive,
      weights = weights,
      weight_column = "value"
    )
  }, error = function(e) {
    stop(paste("Interpolation failed:", e$message))
  })
  
  cat("Interpolation complete.\n\n")
  
  # Use interpolated Y1 data
  data_y1 <- data_y1_interp
}

# --- Prepare Output Data ---
cat("Calculating change metrics...\n")

# Get column names
est_col <- paste0(variable, "E")
moe_col <- paste0(variable, "M")

# Rename Y1 columns
data_y1_renamed <- data_y1 %>%
  st_drop_geometry() %>%
  select(GEOID, any_of("NAME"), !!est_col, !!moe_col) %>%
  rename(
    Est_Y1 = !!est_col,
    MOE_Y1 = !!moe_col
  )

# Rename Y2 columns and keep geometry
data_y2_renamed <- data_y2 %>%
  select(GEOID, NAME, !!est_col, !!moe_col) %>%
  rename(
    Est_Y2 = !!est_col,
    MOE_Y2 = !!moe_col
  )

# Join datasets
output_data <- data_y2_renamed %>%
  left_join(
    data_y1_renamed %>% select(GEOID, Est_Y1, MOE_Y1),
    by = "GEOID"
  )

# Calculate change metrics
output_data <- output_data %>%
  mutate(
    Change = Est_Y2 - Est_Y1,
    PctChange = ifelse(Est_Y1 != 0 & !is.na(Est_Y1), 
                       round((Est_Y2 - Est_Y1) / Est_Y1 * 100, 2), 
                       NA),
    SigChange = mapply(calc_significance, 
                       Est_Y1, Est_Y2, MOE_Y1, MOE_Y2, 
                       MoreArgs = list(confidence = moe_level / 100))
  )

# Reorder columns
output_data <- output_data %>%
  select(GEOID, NAME, Est_Y1, MOE_Y1, Est_Y2, MOE_Y2, Change, PctChange, SigChange)

cat(paste("Processed", nrow(output_data), "records\n"))

# Summary statistics
sig_increase <- sum(output_data$SigChange == 1, na.rm = TRUE)
sig_decrease <- sum(output_data$SigChange == -1, na.rm = TRUE)
no_sig_change <- sum(output_data$SigChange == 0, na.rm = TRUE)

cat("\n=============================================================================\n")
cat("Change Summary:\n")
cat(paste("  Significant Increase:", sig_increase, "geographies\n"))
cat(paste("  Significant Decrease:", sig_decrease, "geographies\n"))
cat(paste("  No Significant Change:", no_sig_change, "geographies\n"))
cat("=============================================================================\n\n")

# --- Write Output ---
output_format <- detect_output_format(output_fc)
cat(paste("Output format:", output_format, "\n"))

if (output_format == "shapefile" && truncate_fields) {
  cat("Truncating field names to 10 characters...\n")
  output_data <- truncate_shapefile_fields(output_data)
}

cat("Writing output...\n")
if (output_format == "geopackage") {
  sf::st_write(output_data, output_fc, delete_dsn = TRUE, quiet = TRUE)
} else {
  sf::st_write(output_data, output_fc, delete_layer = TRUE, quiet = TRUE)
}

cat(paste("\nSuccessfully created:", output_fc, "\n"))
cat(paste("Total records:", nrow(output_data), "\n"))
cat("Done!\n")