# =============================================================================
# ACS Census Data Download Script
# Uses stored Census API key from .Renviron
# =============================================================================

# --- Helper Functions (must be defined before use) ---

detect_output_format <- function(output_path) {
  path_lower <- tolower(output_path)
  if (grepl("\\.shp$", path_lower)) return("shapefile")
  if (grepl("\\.csv$", path_lower)) return("csv")
  if (grepl("\\.gpkg$", path_lower)) return("geopackage")
  return("shapefile")
}

clean_field_name <- function(label, var_code) {
  label <- gsub("^Estimate!!", "", label)
  label <- gsub("^Annotation of ", "", label)
  replacements <- c(
    "Median household income in the past 12 months \\(in \\d{4} inflation-adjusted dollars\\)" = "Median_HH_Income",
    "Per capita income in the past 12 months \\(in \\d{4} inflation-adjusted dollars\\)" = "Per_Capita_Income",
    "Total population" = "Total_Pop",
    " in the past 12 months" = "",
    "\\(in \\d{4} inflation-adjusted dollars\\)" = ""
  )
  for (pattern in names(replacements)) {
    label <- gsub(pattern, replacements[pattern], label, perl = TRUE)
  }
  label <- gsub("[!:/-]", "_", label)
  label <- gsub("[\\(\\),']", "", label)
  label <- gsub(" ", "_", label)
  label <- gsub("_{2,}", "_", label)
  label <- gsub("^_+|_+$", "", label)
  if (nchar(label) == 0) return(var_code)
  if (nchar(label) > 64) label <- substr(label, 1, 64)
  return(label)
}

clean_field_names <- function(data, year, survey) {
  tryCatch({
    var_info <- suppressMessages(tidycensus::load_variables(year = year, dataset = survey))
    estimate_cols <- grep("E$", names(data), value = TRUE)
    var_codes <- unique(gsub("E$", "", estimate_cols))
    name_map <- list()
    for (var_code in var_codes) {
      match_idx <- which(var_info$name == var_code)
      if (length(match_idx) > 0) {
        label <- var_info$label[match_idx[1]]
        name_map[[var_code]] <- clean_field_name(label, var_code)
      } else {
        name_map[[var_code]] <- var_code
      }
    }
    new_names <- names(data)
    for (i in seq_along(new_names)) {
      col <- new_names[i]
      if (grepl("E$", col)) {
        var_code <- gsub("E$", "", col)
        if (var_code %in% names(name_map)) new_names[i] <- name_map[[var_code]]
      } else if (grepl("M$", col)) {
        var_code <- gsub("M$", "", col)
        if (var_code %in% names(name_map)) new_names[i] <- paste0(name_map[[var_code]], "_MOE")
      }
    }
    if (any(duplicated(new_names))) new_names <- make.unique(new_names, sep = "_")
    names(data) <- new_names
  }, error = function(e) {
    cat(paste("Warning: Could not load variable labels:", e$message, "\n"))
  })
  return(data)
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

is_empty <- function(x) {
  is.null(x) || is.na(x) || (is.character(x) && nchar(trimws(x)) == 0)
}

# --- Parse multi-value state parameter ---
parse_states <- function(state_str) {
  if (is_empty(state_str)) return(NULL)
  states <- trimws(strsplit(state_str, ",")[[1]])
  states <- states[nchar(states) > 0]
  if (length(states) == 0) return(NULL)
  if (length(states) == 1) return(states)
  return(states)
}

# --- Parse multi-value county parameter ---
parse_counties <- function(county_str) {
  if (is_empty(county_str)) return(NULL)
  counties <- trimws(strsplit(county_str, ",")[[1]])
  counties <- counties[nchar(counties) > 0]
  if (length(counties) == 0) return(NULL)
  if (length(counties) == 1) return(counties)
  return(counties)
}

# --- Parse multi-value ZCTA parameter ---
parse_zctas <- function(zcta_str) {
  if (is_empty(zcta_str)) return(NULL)
  zctas <- trimws(strsplit(zcta_str, ",")[[1]])
  zctas <- zctas[nchar(zctas) > 0]
  if (length(zctas) == 0) return(NULL)
  if (length(zctas) == 1) return(zctas)
  return(zctas)
}

# --- Main Script ---

args <- commandArgs(trailingOnly = TRUE)

geography <- args[1]
variables <- args[2]
table_param <- args[3]
state <- parse_states(args[4])
county <- parse_counties(args[5])
zcta <- parse_zctas(args[6])
year <- as.integer(args[7])
survey <- args[8]
include_geometry <- toupper(args[9]) %in% c("TRUE", "T", "1", "YES")
keep_geo_vars <- toupper(args[10]) %in% c("TRUE", "T", "1", "YES")
summary_var <- args[11]
output_wide <- toupper(args[12]) %in% c("TRUE", "T", "1", "YES")
cache_table <- toupper(args[13]) %in% c("TRUE", "T", "1", "YES")
show_call <- toupper(args[14]) %in% c("TRUE", "T", "1", "YES")
moe_level <- as.numeric(args[15])
output_fc <- args[16]
truncate_fields <- if (length(args) >= 17) toupper(args[17]) %in% c("TRUE", "T", "1", "YES") else TRUE

cat(paste("Output file:", output_fc, "\n"))
cat(paste("Truncate fields:", truncate_fields, "\n"))

cat("Loading required R packages...\n")
suppressPackageStartupMessages({
  library(tidycensus)
  library(sf)
  library(dplyr)
})

if (Sys.getenv("CENSUS_API_KEY") == "") {
  stop("Census API key not found. Run 'Set Census API Key' tool first.")
}
cat("Using stored Census API key.\n")

use_table <- !is_empty(table_param)
use_variables <- !is_empty(variables)

if (!use_table && !use_variables) stop("Must specify either Variables or Table")
if (use_table && use_variables) stop("Specify Variables OR Table, not both")

if (use_variables) {
  var_list <- trimws(strsplit(variables, ",")[[1]])
  cat(paste("Variables:", paste(var_list, collapse = ", "), "\n"))
} else {
  cat(paste("Table:", table_param, "\n"))
  var_list <- NULL
}

cat(paste("Downloading", toupper(survey), "data for", year, "\n"))
cat(paste("Geography:", geography, "\n"))
if (!is.null(state)) {
  cat(paste("State(s):", paste(state, collapse = ", "), "\n"))
}

acs_params <- list(
  geography = geography, state = state, year = year, survey = survey,
  geometry = include_geometry, keep_geo_vars = keep_geo_vars,
  output = ifelse(output_wide, "wide", "tidy"),
  show_call = show_call, moe_level = moe_level
)

# Use full TIGER/Line files for ZCTA (CB files not always available for recent years)
if (geography == "zcta" && include_geometry) {
  acs_params$cb <- FALSE
  cat("Using full TIGER/Line shapefiles for ZCTA geometry.\n")
}

if (use_table) {
  acs_params$table <- table_param
  acs_params$cache_table <- cache_table
} else {
  acs_params$variables <- var_list
}

if (!is.null(county)) { 
  acs_params$county <- county
  cat(paste("County(s):", paste(county, collapse = ", "), "\n")) 
}
if (!is.null(zcta)) { 
  acs_params$zcta <- zcta
  cat(paste("ZCTA(s):", paste(zcta, collapse = ", "), "\n")) 
}
if (!is_empty(summary_var)) { acs_params$summary_var <- summary_var }

cat("Calling Census API...\n")
acs_data <- tryCatch({
  suppressMessages(do.call(get_acs, acs_params))
}, error = function(e) {
  err_msg <- e$message
  
  # Provide user-friendly error messages for common issues
  if (grepl("CB ZCTA file", err_msg, ignore.case = TRUE)) {
    stop(paste0("ZCTA geometry is not available for ", year, ". ",
                "The Census Bureau releases ZCTA boundary files on a delayed schedule. ",
                "Try using an earlier year (2020-2022 typically work)."))
  }
  
  if (grepl("unknown/unsupported geography hierarchy", err_msg, ignore.case = TRUE)) {
    stop(paste0("The geography '", geography, "' is not supported for the selected state or area. ",
                "Some geographies are only available in certain states or require different parameters."))
  }
  
  if (grepl("rate limit", err_msg, ignore.case = TRUE)) {
    stop("Census API rate limit reached. Please wait a few minutes and try again.")
  }
  
  if (grepl("API key", err_msg, ignore.case = TRUE)) {
    stop("Census API key issue. Run the 'Set Census API Key' tool to configure your key.")
  }
  
  if (grepl("geometry data download failed", err_msg, ignore.case = TRUE)) {
    stop(paste0("Could not download geometry for ", year, " ", geography, ". ",
                "The Census Bureau may not have boundary files available for this year/geography combination. ",
                "Try an earlier year or check https://www2.census.gov/geo/tiger/ for availability."))
  }

  # Check for index errors that may indicate county name matching issues
  if (grepl("index", err_msg, ignore.case = TRUE) && !is.null(county)) {
    stop(paste0("Error calling Census API: ", err_msg, "\n\n",
                "This may be caused by an ambiguous county name (e.g., 'Yellowstone' conflicts with the national park). ",
                "Try using the 3-digit county FIPS code instead of the county name. ",
                "You can find FIPS codes at: https://www.census.gov/library/reference/code-lists/ansi.html"))
  }

  # Default: pass through original error
  stop(paste("Error calling Census API:", err_msg))
})

cat(paste("Retrieved", nrow(acs_data), "records\n"))

if (output_wide) {
  cat("Cleaning field names...\n")
  acs_data <- clean_field_names(acs_data, year, survey)
}

output_format <- detect_output_format(output_fc)
cat(paste("Output format:", output_format, "\n"))

if (output_format == "shapefile" && truncate_fields) {
  cat("Truncating field names to 10 characters...\n")
  acs_data <- truncate_shapefile_fields(acs_data)
} else if (!truncate_fields) {
  cat("Preserving full field names for geodatabase output.\n")
}

cat("Writing output...\n")
if (include_geometry) {
  if (output_format == "geopackage") {
    # Write with explicit layer name to avoid field name issues
    layer_name <- tools::file_path_sans_ext(basename(output_fc))
    sf::st_write(acs_data, output_fc, layer = layer_name, delete_dsn = TRUE, quiet = TRUE)

    # Verify NAME field wasn't truncated (GDAL/GeoPackage bug workaround)
    written_data <- sf::st_read(output_fc, quiet = TRUE)
    if ("NAM" %in% names(written_data) && !"NAME" %in% names(written_data)) {
      cat("Fixing truncated NAME field...\n")
      names(written_data)[names(written_data) == "NAM"] <- "NAME"
      sf::st_write(written_data, output_fc, layer = layer_name, delete_dsn = TRUE, quiet = TRUE)
    }
  } else {
    sf::st_write(acs_data, output_fc, delete_layer = TRUE, quiet = TRUE)
  }
} else {
  df_data <- sf::st_drop_geometry(acs_data)
  write.csv(df_data, output_fc, row.names = FALSE)
}

cat(paste("Successfully created:", output_fc, "\n"))
cat(paste("Total records:", nrow(acs_data), "\n"))
cat("Done!")