# =============================================================================
# ACS Join Data Script
# Joins ACS Census data to user's existing polygon features
# Supports area-weighted and population-weighted spatial joins
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

parse_variables <- function(var_str) {
  if (is_empty(var_str)) return(NULL)
  vars <- trimws(strsplit(var_str, ",")[[1]])
  vars <- vars[nchar(vars) > 0]
  if (length(vars) == 0) return(NULL)
  return(vars)
}

detect_output_format <- function(output_path) {
  path_lower <- tolower(output_path)
  if (grepl("\\.shp$", path_lower)) return("shapefile")
  if (grepl("\\.gpkg$", path_lower)) return("geopackage")
  return("shapefile")
}

# Detect input format and parse path
parse_input_path <- function(input_path) {
  # Check if it's a file geodatabase path (contains .gdb)
  if (grepl("\\.gdb", input_path, ignore.case = TRUE)) {
    # Split into gdb path and layer name
    # Pattern: something.gdb/layer_name or something.gdb\layer_name
    parts <- strsplit(input_path, "[\\/](?=[^\\/]*$)", perl = TRUE)[[1]]
    if (length(parts) == 2 && grepl("\\.gdb$", parts[1], ignore.case = TRUE)) {
      return(list(
        type = "filegdb",
        dsn = parts[1],
        layer = parts[2]
      ))
    }
    # Maybe it's just the gdb path without layer
    if (grepl("\\.gdb$", input_path, ignore.case = TRUE)) {
      return(list(
        type = "filegdb",
        dsn = input_path,
        layer = NULL
      ))
    }
  }
  
  # Check for GeoPackage
  if (grepl("\\.gpkg$", input_path, ignore.case = TRUE)) {
    return(list(
      type = "geopackage",
      dsn = input_path,
      layer = NULL
    ))
  }
  
  # Check for shapefile
  if (grepl("\\.shp$", input_path, ignore.case = TRUE)) {
    return(list(
      type = "shapefile",
      dsn = input_path,
      layer = NULL
    ))
  }
  
  # Default - treat as direct path
  return(list(
    type = "unknown",
    dsn = input_path,
    layer = NULL
  ))
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

# --- Main Script ---

args <- commandArgs(trailingOnly = TRUE)

# Parse arguments
input_fc <- args[1]
year <- as.integer(args[2])
survey <- args[3]
variables <- parse_variables(args[4])
var_type <- args[5]  # "count" or "rate"
geography <- args[6]
state <- parse_states(args[7])
join_method <- args[8]  # "area" or "population"
output_fc <- args[9]
truncate_fields <- if (length(args) >= 10) toupper(args[10]) %in% c("TRUE", "T", "1", "YES") else TRUE

cat("=============================================================================\n")
cat("ACS Join Data\n")
cat("=============================================================================\n")
cat(paste("Input Features:", input_fc, "\n"))
cat(paste("Year:", year, "\n"))
cat(paste("Survey:", survey, "\n"))
cat(paste("Variables:", paste(variables, collapse = ", "), "\n"))
cat(paste("Variable Type:", var_type, "\n"))
cat(paste("Census Geography:", geography, "\n"))
cat(paste("State(s):", paste(state, collapse = ", "), "\n"))
cat(paste("Join Method:", join_method, "\n"))
cat(paste("Output:", output_fc, "\n"))
cat("=============================================================================\n\n")

# Load required packages
cat("Loading required R packages...\n")
library(tidycensus)
library(sf)
library(dplyr)

# Disable s2 for geometry operations
sf_use_s2(FALSE)

if (Sys.getenv("CENSUS_API_KEY") == "") {
  stop("Census API key not found. Run 'Set Census API Key' tool first.")
}

# --- Read User's Feature Class ---
cat("Reading input features...\n")

# Parse the input path to handle file geodatabases
input_info <- parse_input_path(input_fc)
cat(paste("  Input type:", input_info$type, "\n"))
cat(paste("  DSN:", input_info$dsn, "\n"))
if (!is.null(input_info$layer)) {
  cat(paste("  Layer:", input_info$layer, "\n"))
}

user_data <- tryCatch({
  if (input_info$type == "filegdb" && !is.null(input_info$layer)) {
    # File geodatabase with layer name
    st_read(dsn = input_info$dsn, layer = input_info$layer, quiet = TRUE)
  } else if (input_info$type == "filegdb") {
    # File geodatabase without layer - read first layer
    layers <- st_layers(input_info$dsn)
    if (length(layers$name) == 0) {
      stop("No layers found in file geodatabase")
    }
    st_read(dsn = input_info$dsn, layer = layers$name[1], quiet = TRUE)
  } else {
    # Shapefile, GeoPackage, or other - read directly
    st_read(input_fc, quiet = TRUE)
  }
}, error = function(e) {
  stop(paste("Error reading input features:", e$message))
})

cat(paste("Read", nrow(user_data), "features\n"))

# Add unique ID for joining results back
user_data$join_id <- seq_len(nrow(user_data))

# Repair geometries
user_data <- st_make_valid(user_data)

# --- Download Census Data ---
cat(paste("\nDownloading", geography, "level Census data...\n"))

acs_params <- list(
  geography = geography,
  state = state,
  variables = variables,
  year = year,
  survey = survey,
  geometry = TRUE,
  output = "wide"
)

# Use full TIGER/Line files for ZCTA
if (geography == "zcta") {
  acs_params$cb <- FALSE
}

census_data <- tryCatch({
  do.call(get_acs, acs_params)
}, error = function(e) {
  stop(paste("Error downloading Census data:", e$message))
})

cat(paste("Downloaded", nrow(census_data), "Census features\n"))

# Repair Census geometries
census_data <- st_make_valid(census_data)
census_data <- census_data[!st_is_empty(census_data), ]

# --- Perform Spatial Join ---
cat(paste("\nPerforming", join_method, "weighted spatial join...\n"))

# Report CRS information
cat("\n--- Coordinate Reference Systems ---\n")
cat(paste("User data CRS:", st_crs(user_data)$input, "\n"))
cat(paste("Census data CRS:", st_crs(census_data)$input, "\n"))

# Transform Census data to match user's CRS
census_data <- st_transform(census_data, st_crs(user_data))
cat(paste("Census transformed to:", st_crs(census_data)$input, "\n"))

# Report bounding boxes
cat("\n--- Bounding Boxes ---\n")
user_bbox <- st_bbox(user_data)
census_bbox <- st_bbox(census_data)
cat(paste("User data extent:\n"))
cat(paste("  xmin:", round(user_bbox["xmin"], 4), "xmax:", round(user_bbox["xmax"], 4), "\n"))
cat(paste("  ymin:", round(user_bbox["ymin"], 4), "ymax:", round(user_bbox["ymax"], 4), "\n"))
cat(paste("Census data extent:\n"))
cat(paste("  xmin:", round(census_bbox["xmin"], 4), "xmax:", round(census_bbox["xmax"], 4), "\n"))
cat(paste("  ymin:", round(census_bbox["ymin"], 4), "ymax:", round(census_bbox["ymax"], 4), "\n"))

# Check for overlap
cat("\n--- Overlap Check ---\n")
# Quick intersection test on a sample
sample_size <- min(1000, nrow(user_data))
sample_indices <- sample(seq_len(nrow(user_data)), sample_size)
sample_data <- user_data[sample_indices, ]
sample_intersects <- st_intersects(sample_data, census_data)
num_with_match <- sum(lengths(sample_intersects) > 0)
cat(paste("Sample test:", num_with_match, "of", sample_size, "parcels intersect Census tracts\n"))
cat(paste("Estimated coverage:", round(num_with_match / sample_size * 100, 1), "%\n"))

if (num_with_match < sample_size * 0.5) {
  cat("\nWARNING: Less than 50% of parcels intersect Census tracts!\n")
  cat("This may indicate a CRS or extent mismatch.\n")
}
cat("\n")

is_extensive <- tolower(var_type) == "count"

if (join_method == "population") {
  # Population-weighted assignment
  # For small polygons like parcels, we assign the value from the Census unit
  # that contains the parcel centroid, or has the largest overlap
  cat("Using population-weighted assignment...\n")
  
  # Get estimate columns from census data
  est_cols <- grep("E$", names(census_data), value = TRUE)
  moe_cols <- grep("M$", names(census_data), value = TRUE)
  census_cols <- c(est_cols, moe_cols)
  
  # Method: Use centroids for point-in-polygon assignment (fast and appropriate for parcels)
  cat("Computing parcel centroids...\n")
  suppressWarnings({
    user_centroids <- st_centroid(user_data)
  })
  
  cat("Performing spatial join (centroid within Census unit)...\n")
  joined <- st_join(user_centroids, census_data[, c("GEOID", census_cols)], join = st_within, left = TRUE)
  
  # Check how many got matched
  matched <- sum(!is.na(joined$GEOID))
  cat(paste("Centroid join matched:", matched, "of", nrow(user_data), "features\n"))
  
  # For any unmatched (centroid outside all tracts), try nearest neighbor
  unmatched_idx <- which(is.na(joined$GEOID))
  if (length(unmatched_idx) > 0) {
    cat(paste("Finding nearest Census unit for", length(unmatched_idx), "unmatched features...\n"))
    
    unmatched_centroids <- user_centroids[unmatched_idx, ]
    nearest_idx <- st_nearest_feature(unmatched_centroids, census_data)
    
    for (col in census_cols) {
      joined[[col]][unmatched_idx] <- census_data[[col]][nearest_idx]
    }
    joined$GEOID[unmatched_idx] <- census_data$GEOID[nearest_idx]
    
    cat("Nearest neighbor assignment complete.\n")
  }
  
  # Transfer results back to original geometries (not centroids)
  result <- user_data
  for (col in census_cols) {
    result[[col]] <- joined[[col]]
  }
  result$GEOID_Census <- joined$GEOID
  
} else if (join_method == "area") {
  # Area-weighted interpolation
  cat("Using area-weighted interpolation...\n")
  
  # Calculate areas
  census_data$census_area <- as.numeric(st_area(census_data))
  
  # Perform intersection
  cat("Intersecting features...\n")
  intersected <- st_intersection(user_data, census_data)
  intersected$intersect_area <- as.numeric(st_area(intersected))
  
  # Calculate proportion of each census unit in intersection
  intersected$weight <- intersected$intersect_area / intersected$census_area
  
  # Get estimate columns
  est_cols <- grep("E$", names(census_data), value = TRUE)
  
  # Aggregate to user features
  cat("Aggregating values...\n")
  
  agg_data <- intersected %>%
    st_drop_geometry() %>%
    group_by(join_id)
  
  if (is_extensive) {
    # For counts: sum weighted values
    for (col in est_cols) {
      agg_data <- agg_data %>%
        mutate(!!col := get(col) * weight)
    }
    agg_data <- agg_data %>%
      summarize(across(all_of(est_cols), ~sum(.x, na.rm = TRUE)), .groups = "drop")
  } else {
    # For rates: weighted average
    agg_data <- agg_data %>%
      summarize(across(all_of(est_cols), ~weighted.mean(.x, weight, na.rm = TRUE)), .groups = "drop")
  }
  
  # Join back to user data
  result <- user_data %>%
    left_join(agg_data, by = "join_id")
}

# Remove temporary join_id
result$join_id <- NULL

cat(paste("\nProcessed", nrow(result), "features\n"))

# --- Write Output ---
output_format <- detect_output_format(output_fc)
cat(paste("Output format:", output_format, "\n"))

if (output_format == "shapefile" && truncate_fields) {
  cat("Truncating field names to 10 characters...\n")
  result <- truncate_shapefile_fields(result)
}

cat("Writing output...\n")
if (output_format == "geopackage") {
  st_write(result, output_fc, delete_dsn = TRUE, quiet = TRUE)
} else {
  st_write(result, output_fc, delete_layer = TRUE, quiet = TRUE)
}

cat(paste("\nSuccessfully created:", output_fc, "\n"))
cat(paste("Total features:", nrow(result), "\n"))
cat(paste("Census fields added:", length(grep("E$", names(result), value = TRUE)), "\n"))
cat("Done!\n")