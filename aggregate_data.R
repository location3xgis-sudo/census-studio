# =============================================================================
# Aggregate Data Script
# Aggregates numeric fields from source polygons to target zones
# Supports area-weighted and population-weighted aggregation with MOE propagation
# =============================================================================

# Suppress all warnings and messages at the very start
options(
  warn = -1,
  tigris_use_cache = TRUE,
  tigris_progress_bar = FALSE,
  readr.show_progress = FALSE
)

# Suppress sf messages
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

parse_fields <- function(field_str) {
  if (is_empty(field_str)) return(NULL)
  fields <- trimws(strsplit(field_str, ",")[[1]])
  fields <- fields[nchar(fields) > 0]
  if (length(fields) == 0) return(NULL)
  return(fields)
}

detect_output_format <- function(output_path) {
  path_lower <- tolower(output_path)
  if (grepl("\\.shp$", path_lower)) return("shapefile")
  if (grepl("\\.gpkg$", path_lower)) return("geopackage")
  return("shapefile")
}

# Parse input path to handle file geodatabases
parse_input_path <- function(input_path) {
  if (grepl("\\.gdb", input_path, ignore.case = TRUE)) {
    parts <- strsplit(input_path, "[\\/](?=[^\\/]*$)", perl = TRUE)[[1]]
    if (length(parts) == 2 && grepl("\\.gdb$", parts[1], ignore.case = TRUE)) {
      return(list(type = "filegdb", dsn = parts[1], layer = parts[2]))
    }
    if (grepl("\\.gdb$", input_path, ignore.case = TRUE)) {
      return(list(type = "filegdb", dsn = input_path, layer = NULL))
    }
  }
  if (grepl("\\.gpkg$", input_path, ignore.case = TRUE)) {
    return(list(type = "geopackage", dsn = input_path, layer = NULL))
  }
  if (grepl("\\.shp$", input_path, ignore.case = TRUE)) {
    return(list(type = "shapefile", dsn = input_path, layer = NULL))
  }
  return(list(type = "unknown", dsn = input_path, layer = NULL))
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

# --- MOE Propagation Functions ---

# Detect paired MOE columns for selected fields
detect_moe_pairs <- function(data, selected_fields) {
  all_fields <- names(data)
  moe_pairs <- list()

  for (field in selected_fields) {
    moe_field <- NULL

    # Strategy 1: field + "_MOE" (cleaned Census names)
    candidate <- paste0(field, "_MOE")
    if (candidate %in% all_fields) {
      moe_field <- candidate
    }

    # Strategy 2: field ends in 'E', look for 'M' version (raw Census)
    if (is.null(moe_field) && grepl("E$", field)) {
      candidate <- sub("E$", "M", field)
      if (candidate %in% all_fields) {
        moe_field <- candidate
      }
    }

    moe_pairs[[field]] <- moe_field
  }

  return(moe_pairs)
}

# MOE for weighted sum: sqrt(sum((weight * MOE)^2))
propagate_moe_weighted_sum <- function(moe_values, weights) {
  valid_idx <- !is.na(moe_values) & !is.na(weights)
  if (sum(valid_idx) == 0) return(NA_real_)
  sqrt(sum((weights[valid_idx] * moe_values[valid_idx])^2))
}

# MOE for weighted average: sqrt(sum((weight * MOE)^2)) / sum(weights)
propagate_moe_weighted_avg <- function(moe_values, weights) {
  valid_idx <- !is.na(moe_values) & !is.na(weights)
  if (sum(valid_idx) == 0) return(NA_real_)
  sum_weights <- sum(weights[valid_idx])
  if (sum_weights == 0) return(NA_real_)
  sqrt(sum((weights[valid_idx] * moe_values[valid_idx])^2)) / sum_weights
}

# --- Block Population Caching for Population-Weighted Aggregation ---

get_cached_block_populations <- function(state_fips, cache_dir) {
  cache_file <- file.path(cache_dir, paste0("block_pop_", state_fips, ".gpkg"))

  # Create cache directory if needed
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Check cache
  if (file.exists(cache_file)) {
    cat(paste("  Loading block populations from cache for state", state_fips, "...\n"))
    blocks <- tryCatch({
      st_read(cache_file, quiet = TRUE)
    }, error = function(e) {
      cat(paste("  Warning: Cache corrupted, re-downloading...\n"))
      NULL
    })
    if (!is.null(blocks)) return(blocks)
  }

  # Download 2020 Decennial Census blocks
  state_abbrev <- fips_to_abbrev(state_fips)
  cat(paste("  Downloading 2020 block populations for", state_abbrev, "(first-time, will be cached)...\n"))

  blocks <- tryCatch({
    suppressMessages(get_decennial(
      geography = "block",
      variables = "P1_001N",
      state = state_abbrev,
      year = 2020,
      sumfile = "pl",
      geometry = TRUE
    ))
  }, error = function(e) {
    cat(paste("  Warning: Could not download blocks:", e$message, "\n"))
    return(NULL)
  })

  if (is.null(blocks) || nrow(blocks) == 0) {
    return(NULL)
  }

  # Rename value column to population
  names(blocks)[names(blocks) == "value"] <- "population"

  # Cache for future use
  cat(paste("  Caching", nrow(blocks), "blocks to:", cache_file, "\n"))
  suppressWarnings(st_write(blocks, cache_file, delete_dsn = TRUE, quiet = TRUE))

  return(blocks)
}

detect_states_from_extent <- function(target_data, cache_dir) {
  # Use cached county boundaries to determine which states are needed
  county_cache <- file.path(cache_dir, "us_county_boundaries.gpkg")

  if (!file.exists(county_cache)) {
    # Download county boundaries
    cat("Downloading county boundaries for state detection...\n")
    counties <- tryCatch({
      suppressMessages(get_acs(
        geography = "county",
        variables = "B01003_001",
        year = 2022,
        survey = "acs5",
        geometry = TRUE
      ))
    }, error = function(e) {
      stop(paste("Failed to download county boundaries:", e$message))
    })
    counties$STATEFP <- substr(counties$GEOID, 1, 2)
    suppressWarnings(st_write(counties, county_cache, delete_dsn = TRUE, quiet = TRUE))
  } else {
    counties <- st_read(county_cache, quiet = TRUE)
    if (!"STATEFP" %in% names(counties)) {
      counties$STATEFP <- substr(counties$GEOID, 1, 2)
    }
  }

  # Transform and find overlapping states
  counties <- suppressWarnings(st_transform(counties, st_crs(target_data)))
  suppressWarnings({
    intersects <- st_intersects(counties, st_union(target_data))
  })
  overlapping_idx <- which(lengths(intersects) > 0)

  if (length(overlapping_idx) == 0) {
    stop("Could not detect states from target extent")
  }

  state_fips <- unique(counties$STATEFP[overlapping_idx])
  cat(paste("Detected states:", paste(state_fips, collapse = ", "), "\n"))

  return(state_fips)
}

# --- Read Feature Class ---

read_feature_class <- function(fc_path, name = "features") {
  input_info <- parse_input_path(fc_path)
  cat(paste("Reading", name, "from:", input_info$dsn, "\n"))

  data <- tryCatch({
    if (input_info$type == "filegdb" && !is.null(input_info$layer)) {
      st_read(dsn = input_info$dsn, layer = input_info$layer, quiet = TRUE)
    } else if (input_info$type == "filegdb") {
      layers <- st_layers(input_info$dsn)
      if (length(layers$name) == 0) stop("No layers found in file geodatabase")
      st_read(dsn = input_info$dsn, layer = layers$name[1], quiet = TRUE)
    } else {
      st_read(fc_path, quiet = TRUE)
    }
  }, error = function(e) {
    stop(paste("Error reading", name, ":", e$message))
  })

  cat(paste("  Read", nrow(data), "features\n"))
  return(data)
}

# --- Main Script ---

cat("Loading required R packages...\n")
suppressPackageStartupMessages({
  library(tidycensus)
  library(sf)
  library(dplyr)
})

# Disable s2 for geometry operations
sf_use_s2(FALSE)

# Parse arguments
args <- commandArgs(trailingOnly = TRUE)

source_fc <- args[1]
target_fc <- args[2]
fields_str <- args[3]
var_type <- args[4]  # "count" or "rate"
agg_method <- args[5]  # "area" or "population"
output_fc <- args[6]
cache_dir <- if (length(args) >= 7) args[7] else file.path(dirname(source_fc), "cache")

fields <- parse_fields(fields_str)

cat("=============================================================================\n")
cat("Aggregate Data\n")
cat("=============================================================================\n")
cat(paste("Source:", source_fc, "\n"))
cat(paste("Target:", target_fc, "\n"))
cat(paste("Fields:", paste(fields, collapse = ", "), "\n"))
cat(paste("Variable Type:", var_type, "\n"))
cat(paste("Method:", agg_method, "\n"))
cat(paste("Output:", output_fc, "\n"))
cat("=============================================================================\n\n")

# --- Read Input Data ---

source_data <- read_feature_class(source_fc, "source features")
target_data <- read_feature_class(target_fc, "target zones")

# Add unique IDs
source_data$source_id <- seq_len(nrow(source_data))
target_data$target_id <- seq_len(nrow(target_data))

# Repair geometries
source_data <- st_make_valid(source_data)
target_data <- st_make_valid(target_data)

# Transform source to target CRS if needed
if (!identical(st_crs(source_data), st_crs(target_data))) {
  cat("Transforming source data to target CRS...\n")
  source_data <- suppressWarnings(st_transform(source_data, st_crs(target_data)))
}

# Validate fields exist in source
missing_fields <- fields[!fields %in% names(source_data)]
if (length(missing_fields) > 0) {
  stop(paste("Fields not found in source:", paste(missing_fields, collapse = ", ")))
}

# Detect MOE pairs
moe_pairs <- detect_moe_pairs(source_data, fields)
moe_count <- sum(!sapply(moe_pairs, is.null))
cat(paste("Detected", moe_count, "MOE column pairs\n"))

is_extensive <- tolower(var_type) == "count"

# --- Perform Aggregation ---

if (agg_method == "population") {
  # Population-weighted aggregation using Census block populations
  cat("\nPerforming population-weighted aggregation...\n")

  # Detect required states
  state_fips_list <- detect_states_from_extent(target_data, cache_dir)

  # Load block populations for all required states
  all_blocks <- list()
  for (state_fips in state_fips_list) {
    blocks <- get_cached_block_populations(state_fips, cache_dir)
    if (!is.null(blocks)) {
      all_blocks[[length(all_blocks) + 1]] <- blocks
    }
  }

  if (length(all_blocks) == 0) {
    cat("WARNING: Could not download block populations. Falling back to area-weighted.\n")
    agg_method <- "area"
  } else {
    blocks <- do.call(rbind, all_blocks)
    blocks <- suppressWarnings(st_transform(blocks, st_crs(target_data)))
    blocks <- st_make_valid(blocks)
    cat(paste("Loaded", nrow(blocks), "Census blocks with population data\n"))

    # Calculate population in each source-target intersection
    cat("Computing intersections with population weights...\n")

    # First intersect blocks with source to get population per source unit
    suppressWarnings({
      blocks_source <- st_intersection(blocks, source_data[, c("source_id", fields, unlist(moe_pairs[!sapply(moe_pairs, is.null)]))])
      blocks_source$block_area <- as.numeric(st_area(blocks_source))
    })

    # Calculate block population proportions
    blocks$total_block_area <- as.numeric(st_area(blocks))
    blocks_source <- blocks_source %>%
      left_join(st_drop_geometry(blocks[, c("GEOID", "total_block_area")]), by = "GEOID")
    blocks_source$pop_weight <- blocks_source$population * (blocks_source$block_area / blocks_source$total_block_area)

    # Now intersect with target
    suppressWarnings({
      blocks_target <- st_intersection(blocks_source, target_data[, "target_id"])
    })

    # Aggregate by target zone
    cat("Aggregating values with population weights...\n")

    result <- target_data

    for (field in fields) {
      moe_field <- moe_pairs[[field]]

      # Calculate weighted values
      agg <- blocks_target %>%
        st_drop_geometry() %>%
        group_by(target_id)

      if (is_extensive) {
        # For counts: sum of (value * population_weight / total_source_population)
        # This allocates source values to targets based on population distribution
        agg_result <- agg %>%
          summarize(
            !!field := sum(get(field) * pop_weight / sum(pop_weight, na.rm = TRUE), na.rm = TRUE),
            .groups = "drop"
          )

        if (!is.null(moe_field)) {
          moe_agg <- blocks_target %>%
            st_drop_geometry() %>%
            group_by(target_id) %>%
            summarize(
              total_weight = sum(pop_weight, na.rm = TRUE),
              moe_sum = sum((pop_weight * get(moe_field))^2, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(!!paste0(field, "_MOE") := sqrt(moe_sum) / total_weight) %>%
            select(target_id, !!paste0(field, "_MOE"))
          agg_result <- left_join(agg_result, moe_agg, by = "target_id")
        }
      } else {
        # For rates: population-weighted average
        agg_result <- agg %>%
          summarize(
            !!field := weighted.mean(get(field), pop_weight, na.rm = TRUE),
            .groups = "drop"
          )

        if (!is.null(moe_field)) {
          moe_agg <- blocks_target %>%
            st_drop_geometry() %>%
            group_by(target_id) %>%
            summarize(
              total_weight = sum(pop_weight, na.rm = TRUE),
              moe_sum = sum((pop_weight * get(moe_field))^2, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(!!paste0(field, "_MOE") := sqrt(moe_sum) / total_weight) %>%
            select(target_id, !!paste0(field, "_MOE"))
          agg_result <- left_join(agg_result, moe_agg, by = "target_id")
        }
      }

      result <- left_join(st_drop_geometry(result) %>% select(-any_of(c(field, paste0(field, "_MOE")))),
                          agg_result, by = "target_id") %>%
        st_sf(geometry = st_geometry(result))
    }
  }
}

if (agg_method == "area") {
  # Area-weighted aggregation
  cat("\nPerforming area-weighted aggregation...\n")

  # Calculate source areas
  suppressWarnings({
    source_data$source_area <- as.numeric(st_area(source_data))
  })

  # Perform intersection
  cat("Computing intersections...\n")
  suppressWarnings({
    intersected <- st_intersection(source_data, target_data[, "target_id"])
    intersected$intersect_area <- as.numeric(st_area(intersected))
  })

  # Calculate weights
  # For extensive (counts): weight = proportion of source that overlaps (to allocate counts)
  # For intensive (rates): weight = intersection area (to weight by area contribution)
  intersected$weight_extensive <- intersected$intersect_area / intersected$source_area
  intersected$weight_intensive <- intersected$intersect_area

  cat(paste("Created", nrow(intersected), "intersection fragments\n"))

  # Initialize result with target data
  result <- target_data

  # Aggregate each field
  for (field in fields) {
    moe_field <- moe_pairs[[field]]
    cat(paste("  Aggregating:", field, if(!is.null(moe_field)) paste0("(with ", moe_field, ")") else "", "\n"))

    if (is_extensive) {
      # For counts: sum weighted values (allocate proportionally from source)
      agg <- intersected %>%
        st_drop_geometry() %>%
        group_by(target_id) %>%
        summarize(
          !!field := sum(get(field) * weight_extensive, na.rm = TRUE),
          .groups = "drop"
        )

      # Propagate MOE for sums
      if (!is.null(moe_field)) {
        moe_agg <- intersected %>%
          st_drop_geometry() %>%
          group_by(target_id) %>%
          summarize(
            !!paste0(field, "_MOE") := propagate_moe_weighted_sum(get(moe_field), weight_extensive),
            .groups = "drop"
          )
        agg <- left_join(agg, moe_agg, by = "target_id")
      }
    } else {
      # For rates/medians: area-weighted average (weight by intersection area)
      agg <- intersected %>%
        st_drop_geometry() %>%
        group_by(target_id) %>%
        summarize(
          !!field := weighted.mean(get(field), weight_intensive, na.rm = TRUE),
          .groups = "drop"
        )

      # Propagate MOE for averages
      if (!is.null(moe_field)) {
        moe_agg <- intersected %>%
          st_drop_geometry() %>%
          group_by(target_id) %>%
          summarize(
            !!paste0(field, "_MOE") := propagate_moe_weighted_avg(get(moe_field), weight_intensive),
            .groups = "drop"
          )
        agg <- left_join(agg, moe_agg, by = "target_id")
      }
    }

    # Join to result
    result <- result %>%
      left_join(agg, by = "target_id")
  }
}

# Clean up temporary columns
result$target_id <- NULL
result$source_id <- NULL

cat(paste("\nProcessed", nrow(result), "target zones\n"))

# --- Write Output ---

output_format <- detect_output_format(output_fc)

if (output_format == "shapefile") {
  cat("Truncating field names for shapefile output...\n")
  result <- truncate_shapefile_fields(result)
}

cat(paste("Writing output to:", output_fc, "\n"))
suppressWarnings({
  layer_name <- tools::file_path_sans_ext(basename(output_fc))
  st_write(result, output_fc, layer = layer_name, delete_dsn = TRUE, quiet = TRUE)
})

cat("\n=============================================================================\n")
cat(paste("Successfully created:", output_fc, "\n"))
cat(paste("Target zones:", nrow(result), "\n"))
cat(paste("Fields aggregated:", length(fields), "\n"))
if (moe_count > 0) {
  cat(paste("MOE columns propagated:", moe_count, "\n"))
}
cat("=============================================================================\n")
cat("Done!\n")
