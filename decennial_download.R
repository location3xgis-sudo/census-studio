# =============================================================================
# Decennial Census Data Download Script
# Uses tidycensus get_decennial() to download Census data
# =============================================================================

# Suppress warnings, messages, and progress bars BEFORE loading any packages
options(
  warn = -1,
  tigris_use_cache = TRUE,
  readr.show_progress = FALSE
)

# Set environment variable to disable progress bars
Sys.setenv("TIDYCENSUS_PROGRESS" = "FALSE")

# Suppress sf messages by redirecting to null
invisible(suppressMessages(suppressWarnings({
  if (.Platform$OS.type == "windows") {
    sink(file("NUL", open = "wt"), type = "message")
  } else {
    sink(file("/dev/null", open = "wt"), type = "message")
  }
})))

cat("Loading required R packages...\n")
suppressPackageStartupMessages({
  library(tidycensus)
  library(sf)
  library(dplyr)
})

# Disable tigris progress bar after loading
options(tigris_progress_bar = FALSE)

# Check API key
if (Sys.getenv("CENSUS_API_KEY") == "") {
  stop("Census API key not found. Run 'Set Census API Key' tool first.")
}
cat("Using stored Census API key.\n")

# --- Helper Functions ---

is_empty <- function(x) {
  is.null(x) || is.na(x) || (is.character(x) && nchar(trimws(x)) == 0)
}

parse_list <- function(str) {
  if (is_empty(str)) return(NULL)
  items <- trimws(strsplit(str, ",")[[1]])
  items <- items[nchar(items) > 0]
  if (length(items) == 0) return(NULL)
  return(items)
}

# Clean field names to be more readable
clean_field_name <- function(name, year) {
  # Map common variable codes to friendly names
  mappings_2020 <- list(
    "P1_001N" = "Total_Pop",
    "P1_003N" = "Pop_White",
    "P1_004N" = "Pop_Black",
    "P1_005N" = "Pop_AmerInd_AKNat",
    "P1_006N" = "Pop_Asian",
    "P1_007N" = "Pop_NatHaw_PacIsl",
    "P1_008N" = "Pop_Other",
    "P1_009N" = "Pop_TwoOrMore",
    "P2_002N" = "Pop_Hispanic",
    "P2_003N" = "Pop_Not_Hispanic",
    "P3_001N" = "Pop_18Plus",
    "P3_003N" = "Pop_18Plus_White",
    "P3_004N" = "Pop_18Plus_Black",
    "P3_005N" = "Pop_18Plus_AmerInd",
    "P3_006N" = "Pop_18Plus_Asian",
    "P3_007N" = "Pop_18Plus_NatHaw",
    "P3_008N" = "Pop_18Plus_Other",
    "P3_009N" = "Pop_18Plus_TwoMore",
    "P4_002N" = "Pop_18Plus_Hisp",
    "P4_003N" = "Pop_18Plus_NotHisp",
    "H1_001N" = "Housing_Units",
    "H1_002N" = "Housing_Occupied",
    "H1_003N" = "Housing_Vacant",
    "P5_001N" = "GQ_Total",
    "P5_002N" = "GQ_Institutional",
    "P5_003N" = "GQ_Noninstitutional"
  )

  mappings_2010_2000 <- list(
    "P001001" = "Total_Pop",
    "P003002" = "Pop_White",
    "P003003" = "Pop_Black",
    "P003004" = "Pop_AmerInd_AKNat",
    "P003005" = "Pop_Asian",
    "P003006" = "Pop_NatHaw_PacIsl",
    "P003007" = "Pop_Other",
    "P003008" = "Pop_TwoOrMore",
    "P004002" = "Pop_Hispanic",
    "P004003" = "Pop_Not_Hispanic",
    "P010001" = "Pop_18Plus",
    "P006001" = "Pop_18Plus",
    "P010003" = "Pop_18Plus_White",
    "P010004" = "Pop_18Plus_Black",
    "P010005" = "Pop_18Plus_AmerInd",
    "P010006" = "Pop_18Plus_Asian",
    "P010007" = "Pop_18Plus_NatHaw",
    "P010008" = "Pop_18Plus_Other",
    "P010009" = "Pop_18Plus_TwoMore",
    "P011002" = "Pop_18Plus_Hisp",
    "P011003" = "Pop_18Plus_NotHisp",
    "H001001" = "Housing_Units",
    "H003002" = "Housing_Occupied",
    "H003003" = "Housing_Vacant",
    "H004002" = "Housing_Owner",
    "H004003" = "Housing_Renter",
    "P029001" = "GQ_Total",
    "P029002" = "GQ_Institutional",
    "P029003" = "GQ_Noninstitutional"
  )

  if (year == "2020") {
    mappings <- mappings_2020
  } else {
    mappings <- mappings_2010_2000
  }

  if (name %in% names(mappings)) {
    return(mappings[[name]])
  }
  return(name)
}

clean_all_field_names <- function(data, year) {
  new_names <- names(data)
  for (i in seq_along(new_names)) {
    col <- new_names[i]
    # Skip standard fields
    if (col %in% c("GEOID", "NAME", "geometry", "variable", "value")) next
    new_names[i] <- clean_field_name(col, year)
  }
  # Handle duplicates
  if (any(duplicated(new_names))) {
    new_names <- make.unique(new_names, sep = "_")
  }
  names(data) <- new_names
  return(data)
}

# --- Parse Arguments ---
args <- commandArgs(trailingOnly = TRUE)

year <- args[1]
sumfile <- args[2]
variables_str <- args[3]
geography <- args[4]
state_str <- args[5]
county_str <- args[6]
output_path <- args[7]

variables <- parse_list(variables_str)
states <- parse_list(state_str)
counties <- parse_list(county_str)

cat("=============================================================================\n")
cat("Decennial Census Download\n")
cat("=============================================================================\n")
cat(paste("Year:", year, "\n"))
cat(paste("Dataset:", sumfile, "\n"))
cat(paste("Geography:", geography, "\n"))
if (!is.null(states)) cat(paste("State(s):", paste(states, collapse = ", "), "\n"))
if (!is.null(counties)) cat(paste("County:", paste(counties, collapse = ", "), "\n"))
cat(paste("Variables:", length(variables), "\n"))
cat("=============================================================================\n\n")

# Build parameters for get_decennial (geometry always included)
decennial_params <- list(
  geography = geography,
  variables = variables,
  year = as.integer(year),
  sumfile = sumfile,
  geometry = TRUE,
  output = "wide"
)

cat(paste("Parameters: geography =", geography, ", year =", year, ", sumfile =", sumfile, "\n"))
cat(paste("Variables:", paste(variables, collapse = ", "), "\n"))

# Add state if specified
if (!is.null(states)) {
  if (length(states) == 1) {
    decennial_params$state <- states
  } else {
    # Multiple states - need to download separately and combine
    decennial_params$state <- states[1]
  }
}

# Add county if specified
if (!is.null(counties)) {
  decennial_params$county <- counties
}

# Download data
cat("Calling Census API...\n")
flush.console()

if (!is.null(states) && length(states) > 1) {
  # Multiple states - download and combine
  all_data <- list()
  for (st in states) {
    cat(paste("  Downloading", st, "...\n"))
    decennial_params$state <- st

    state_data <- tryCatch({
      suppressWarnings(suppressMessages(do.call(get_decennial, decennial_params)))
    }, error = function(e) {
      cat(paste("  Warning: Error downloading", st, ":", e$message, "\n"))
      NULL
    })

    if (!is.null(state_data) && nrow(state_data) > 0) {
      all_data[[st]] <- state_data
    }
  }

  if (length(all_data) == 0) {
    stop("Could not download data for any of the specified states.")
  }

  decennial_data <- suppressWarnings(do.call(rbind, all_data))
} else {
  # Single state or no state
  cat("Attempting single-state download...\n")
  decennial_data <- tryCatch({
    result <- do.call(get_decennial, decennial_params)
    cat("Download successful.\n")
    result
  }, error = function(e) {
    err_msg <- e$message
    cat(paste("ERROR:", err_msg, "\n"))
    flush.console()

    if (grepl("unknown/unsupported geography", err_msg, ignore.case = TRUE)) {
      stop(paste0("The geography '", geography, "' is not supported for this state/dataset combination."))
    }

    if (grepl("API key", err_msg, ignore.case = TRUE)) {
      stop("Census API key issue. Run the 'Set Census API Key' tool to configure your key.")
    }

    stop(paste("Error calling Census API:", err_msg))
  })
}

cat(paste("Downloaded", nrow(decennial_data), "records\n"))

# Clean field names
cat("Cleaning field names...\n")
decennial_data <- clean_all_field_names(decennial_data, year)

# Fix NAME field if truncated
if ("NAM" %in% names(decennial_data) && !"NAME" %in% names(decennial_data)) {
  names(decennial_data)[names(decennial_data) == "NAM"] <- "NAME"
}

# Write output
cat(paste("Writing output to:", output_path, "\n"))

# Ensure valid geometries
decennial_data <- suppressWarnings(st_make_valid(decennial_data))

# Write as GeoPackage
layer_name <- tools::file_path_sans_ext(basename(output_path))
suppressWarnings(st_write(decennial_data, output_path, layer = layer_name, delete_dsn = TRUE, quiet = TRUE))

cat("\n=============================================================================\n")
cat(paste("Successfully created:", output_path, "\n"))
cat(paste("Total records:", nrow(decennial_data), "\n"))
cat("=============================================================================\n")
cat("Done!\n")
