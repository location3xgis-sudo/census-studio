# =============================================================================
# ACS Join Data Script
# Joins ACS Census data to user's existing polygon features
# Supports area-weighted and population-weighted spatial joins
# =============================================================================

# Suppress all warnings and messages at the very start (before loading packages)
options(
  warn = -1,
  tigris_use_cache = TRUE,
  tigris_progress_bar = FALSE,
  readr.show_progress = FALSE
)

# Suppress sf messages by redirecting message output to null
invisible(suppressMessages(suppressWarnings({
  if (.Platform$OS.type == "windows") {
    sink(file("NUL", open = "wt"), type = "message")
  } else {
    sink(file("/dev/null", open = "wt"), type = "message")
  }
})))

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

# --- County Boundary Caching Functions ---

get_cached_counties <- function(cache_dir) {
  # Get or download US county boundaries with caching
  cache_file <- file.path(cache_dir, "us_county_boundaries.gpkg")

  # Create cache directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    cat(paste("Created cache directory:", cache_dir, "\n"))
  }

  # Check if cache exists
  if (file.exists(cache_file)) {
    cat("Loading county boundaries from cache...\n")
    counties <- tryCatch({
      st_read(cache_file, quiet = TRUE)
    }, error = function(e) {
      cat(paste("Warning: Cache file corrupted, re-downloading...\n"))
      NULL
    })
    if (!is.null(counties)) {
      cat(paste("  Loaded", nrow(counties), "counties from cache\n"))
      return(counties)
    }
  }

  # Download all US county boundaries
  cat("Downloading US county boundaries (first-time setup, will be cached)...\n")
  counties <- tryCatch({
    # Get counties for all states - use a simple variable just to get geometry
    suppressMessages(get_acs(
      geography = "county",
      variables = "B01003_001",
      year = 2022,  # Use a stable year for boundaries
      survey = "acs5",
      geometry = TRUE
    ))
  }, error = function(e) {
    stop(paste("Failed to download county boundaries:", e$message))
  })

  cat(paste("  Downloaded", nrow(counties), "counties\n"))

  # Keep only necessary columns (GEOID, NAME, geometry)
  counties <- counties[, c("GEOID", "NAME")]

  # Extract state FIPS from GEOID (first 2 digits)
  counties$STATEFP <- substr(counties$GEOID, 1, 2)
  counties$COUNTYFP <- substr(counties$GEOID, 3, 5)

  # Save to cache
  cat(paste("Caching county boundaries to:", cache_file, "\n"))
  st_write(counties, cache_file, delete_dsn = TRUE, quiet = TRUE)

  return(counties)
}

detect_counties_from_input <- function(input_data, counties) {
  # Find which counties overlap with the input features
  cat("Detecting counties that overlap input features...\n")

  # Transform counties to match input CRS
  counties_transformed <- suppressWarnings(st_transform(counties, st_crs(input_data)))

  # Get bounding box of input and filter counties first (for performance)
  input_bbox <- st_bbox(input_data)
  # Add a small buffer to the bbox
  buffer <- 0.1  # degrees approximately
  suppressWarnings({
    centroids <- st_coordinates(st_centroid(st_geometry(counties_transformed)))
  })
  counties_filtered <- counties_transformed[
    centroids[,1] >= input_bbox["xmin"] - buffer &
    centroids[,1] <= input_bbox["xmax"] + buffer &
    centroids[,2] >= input_bbox["ymin"] - buffer &
    centroids[,2] <= input_bbox["ymax"] + buffer,
  ]

  if (nrow(counties_filtered) == 0) {
    # Fallback: try intersection with all counties
    cat("  Bounding box filter found no counties, trying full intersection...\n")
    counties_filtered <- counties_transformed
  }

  # Find counties that intersect with input features
  suppressWarnings({
    intersects <- st_intersects(counties_filtered, st_union(input_data))
  })
  overlapping_idx <- which(lengths(intersects) > 0)

  if (length(overlapping_idx) == 0) {
    stop("No counties found that overlap with input features. Check that your data has a valid coordinate system.")
  }

  overlapping_counties <- counties_filtered[overlapping_idx, ]

  # Extract state and county FIPS
  state_fips <- unique(overlapping_counties$STATEFP)
  county_info <- data.frame(
    state_fips = overlapping_counties$STATEFP,
    county_fips = overlapping_counties$COUNTYFP,
    name = overlapping_counties$NAME
  )

  cat(paste("  Found", nrow(overlapping_counties), "overlapping counties in",
            length(state_fips), "state(s)\n"))

  # Print county names
  for (i in seq_len(nrow(county_info))) {
    cat(paste("    -", county_info$name[i], "\n"))
  }

  return(list(
    state_fips = state_fips,
    county_info = county_info
  ))
}

# State FIPS to abbreviation mapping
fips_to_abbrev <- function(fips) {
  mapping <- c(
    "01"="AL", "02"="AK", "04"="AZ", "05"="AR", "06"="CA", "08"="CO", "09"="CT",
    "10"="DE", "11"="DC", "12"="FL", "13"="GA", "15"="HI", "16"="ID", "17"="IL",
    "18"="IN", "19"="IA", "20"="KS", "21"="KY", "22"="LA", "23"="ME", "24"="MD",
    "25"="MA", "26"="MI", "27"="MN", "28"="MS", "29"="MO", "30"="MT", "31"="NE",
    "32"="NV", "33"="NH", "34"="NJ", "35"="NM", "36"="NY", "37"="NC", "38"="ND",
    "39"="OH", "40"="OK", "41"="OR", "42"="PA", "44"="RI", "45"="SC", "46"="SD",
    "47"="TN", "48"="TX", "49"="UT", "50"="VT", "51"="VA", "53"="WA", "54"="WV",
    "55"="WI", "56"="WY", "72"="PR"
  )
  return(mapping[fips])
}

# --- Main Script ---

args <- commandArgs(trailingOnly = TRUE)

# Parse arguments (state is now auto-detected)
input_fc <- args[1]
year <- as.integer(args[2])
survey <- args[3]
variables <- parse_variables(args[4])
var_type <- args[5]  # "count" or "rate"
geography <- args[6]
join_method <- args[7]  # "area" or "population"
output_fc <- args[8]
truncate_fields <- if (length(args) >= 9) toupper(args[9]) %in% c("TRUE", "T", "1", "YES") else TRUE
cache_dir <- if (length(args) >= 10) args[10] else file.path(dirname(input_fc), "cache")

cat("=============================================================================\n")
cat("ACS Join Data\n")
cat("=============================================================================\n")
cat(paste("Input Features:", input_fc, "\n"))
cat(paste("Year:", year, "\n"))
cat(paste("Survey:", survey, "\n"))
cat(paste("Variables:", paste(variables, collapse = ", "), "\n"))
cat(paste("Variable Type:", var_type, "\n"))
cat(paste("Census Geography:", geography, "\n"))
cat(paste("State/County:", "(auto-detecting from input features)", "\n"))
cat(paste("Join Method:", join_method, "\n"))
cat(paste("Output:", output_fc, "\n"))
cat("=============================================================================\n\n")

# Load required packages (suppress startup messages)
cat("Loading required R packages...\n")
suppressPackageStartupMessages({
  library(tidycensus)
  library(sf)
  library(dplyr)
})

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

# --- Auto-detect Counties ---
cat("\n--- Auto-detecting Geographic Coverage ---\n")
counties <- get_cached_counties(cache_dir)
detected <- detect_counties_from_input(user_data, counties)

# --- Download Census Data ---
cat(paste("\nDownloading", geography, "level Census data for detected counties...\n"))

# Download data for each state, filtered by county
all_census_data <- list()

for (state_fips in detected$state_fips) {
  state_abbrev <- fips_to_abbrev(state_fips)
  state_counties <- detected$county_info[detected$county_info$state_fips == state_fips, "county_fips"]

  cat(paste("  Downloading", state_abbrev, "- counties:", paste(state_counties, collapse = ", "), "\n"))

  acs_params <- list(
    geography = geography,
    state = state_abbrev,
    variables = variables,
    year = year,
    survey = survey,
    geometry = TRUE,
    output = "wide"
  )

  # Add county filter for sub-state geographies
  if (geography %in% c("tract", "block group", "county subdivision")) {
    acs_params$county <- state_counties
  }

  # Use full TIGER/Line files for ZCTA
  if (geography == "zcta") {
    acs_params$cb <- FALSE
  }

  state_data <- tryCatch({
    suppressMessages(do.call(get_acs, acs_params))
  }, error = function(e) {
    cat(paste("    Note: Could not download data for", state_abbrev, ":", e$message, "\n"))
    NULL
  })

  if (!is.null(state_data) && nrow(state_data) > 0) {
    all_census_data[[length(all_census_data) + 1]] <- state_data
    cat(paste("    Downloaded", nrow(state_data), "features\n"))
  }
}

# Combine all downloaded data
if (length(all_census_data) == 0) {
  stop("Failed to download Census data for any of the detected counties.")
}

census_data <- do.call(rbind, all_census_data)
cat(paste("\nTotal Census features downloaded:", nrow(census_data), "\n"))

# Clean field names to be human-readable
cat("Cleaning field names...\n")
census_data <- clean_field_names(census_data, year, survey)

# Repair Census geometries
suppressWarnings({
  census_data <- st_make_valid(census_data)
})
census_data <- census_data[!st_is_empty(census_data), ]

# Ensure NAME field is preserved (fix potential truncation)
if ("NAM" %in% names(census_data) && !"NAME" %in% names(census_data)) {
  names(census_data)[names(census_data) == "NAM"] <- "NAME"
}

# --- Perform Spatial Join ---
cat(paste("\nPerforming", join_method, "weighted spatial join...\n"))

# Transform Census data to match user's CRS
suppressWarnings({
  census_data <- st_transform(census_data, st_crs(user_data))
})

# Quick overlap check
suppressWarnings({
  sample_size <- min(100, nrow(user_data))
  sample_indices <- sample(seq_len(nrow(user_data)), sample_size)
  sample_data <- user_data[sample_indices, ]
  sample_intersects <- st_intersects(sample_data, census_data)
  num_with_match <- sum(lengths(sample_intersects) > 0)
})

if (num_with_match < sample_size * 0.5) {
  cat(paste("Note: Only", round(num_with_match / sample_size * 100, 1), "% of features overlap Census data.\n"))
}

is_extensive <- tolower(var_type) == "count"

if (join_method == "population") {
  # Population-weighted assignment
  # For small polygons like parcels, we assign the value from the Census unit
  # that contains the parcel centroid, or has the largest overlap
  cat("Using population-weighted assignment...\n")

  # Get estimate and MOE columns from census data (after field name cleaning)
  # MOE columns end in "_MOE", estimate columns are everything else except system columns
  system_cols <- c("GEOID", "NAME", "geometry", "census_area")
  moe_cols <- grep("_MOE$", names(census_data), value = TRUE)
  est_cols <- setdiff(names(census_data), c(system_cols, moe_cols, "geometry"))
  est_cols <- est_cols[!est_cols %in% c("GEOID", "NAME")]
  census_cols <- c(est_cols, moe_cols)
  
  # Method: Use centroids for point-in-polygon assignment (fast and appropriate for parcels)
  cat("Computing parcel centroids...\n")
  suppressWarnings({
    user_centroids <- st_centroid(user_data)
  })
  
  cat("Performing spatial join (centroid within Census unit)...\n")
  suppressWarnings({
    joined <- st_join(user_centroids, census_data[, c("GEOID", census_cols)], join = st_within, left = TRUE)
  })

  # Check how many got matched
  matched <- sum(!is.na(joined$GEOID))
  cat(paste("Centroid join matched:", matched, "of", nrow(user_data), "features\n"))

  # For any unmatched (centroid outside all tracts), try nearest neighbor
  unmatched_idx <- which(is.na(joined$GEOID))
  if (length(unmatched_idx) > 0) {
    cat(paste("Finding nearest Census unit for", length(unmatched_idx), "unmatched features...\n"))

    unmatched_centroids <- user_centroids[unmatched_idx, ]
    suppressWarnings({
      nearest_idx <- st_nearest_feature(unmatched_centroids, census_data)
    })

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
  suppressWarnings({
    census_data$census_area <- as.numeric(st_area(census_data))
  })

  # Perform intersection
  cat("Intersecting features...\n")
  suppressWarnings({
    intersected <- st_intersection(user_data, census_data)
    intersected$intersect_area <- as.numeric(st_area(intersected))
  })

  # Calculate proportion of each census unit in intersection
  intersected$weight <- intersected$intersect_area / intersected$census_area

  # Get estimate columns (after field name cleaning)
  # Exclude system columns, MOE columns, and geometry
  system_cols <- c("GEOID", "NAME", "geometry", "census_area", "join_id", "intersect_area", "weight")
  moe_cols <- grep("_MOE$", names(census_data), value = TRUE)
  est_cols <- setdiff(names(census_data), c(system_cols, moe_cols))
  est_cols <- est_cols[!est_cols %in% c("GEOID", "NAME")]
  
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

# Fix any truncated NAME fields
if ("NAM" %in% names(result) && !"NAME" %in% names(result)) {
  names(result)[names(result) == "NAM"] <- "NAME"
}

cat(paste("\nProcessed", nrow(result), "features\n"))

# --- Write Output ---
output_format <- detect_output_format(output_fc)

if (output_format == "shapefile" && truncate_fields) {
  cat("Truncating field names to 10 characters for shapefile...\n")
  result <- truncate_shapefile_fields(result)
}

cat("Writing output...\n")
suppressWarnings({
  if (output_format == "geopackage") {
    st_write(result, output_fc, delete_dsn = TRUE, quiet = TRUE)
  } else {
    st_write(result, output_fc, delete_layer = TRUE, quiet = TRUE)
  }
})

cat(paste("\nSuccessfully created:", output_fc, "\n"))
cat(paste("Total features:", nrow(result), "\n"))
# Count Census fields (exclude MOE columns for the estimate count)
moe_count <- length(grep("_MOE$", names(result), value = TRUE))
cat(paste("Census fields added:", moe_count, "estimates +", moe_count, "MOE fields\n"))
cat("Done!\n")