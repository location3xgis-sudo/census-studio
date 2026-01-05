# =============================================================================
# ACS Demographic Profile Report
# Generates comparison report for study area vs county/state/nation
# =============================================================================

library(tidycensus)
library(sf)
library(dplyr)
library(jsonlite)

# Disable s2
sf_use_s2(FALSE)

# --- Parse Arguments ---
args <- commandArgs(trailingOnly = TRUE)

study_area_path <- args[1]
year <- as.integer(args[2])
survey <- args[3]
variables_str <- args[4]
geography <- args[5]
state <- args[6]
county <- if (args[7] == "") NULL else args[7]
compare_county <- toupper(args[8]) == "TRUE"
compare_state <- toupper(args[9]) == "TRUE"
compare_nation <- toupper(args[10]) == "TRUE"
study_area_name <- args[11]
config_file <- args[12]
output_file <- args[13]

variables <- trimws(strsplit(variables_str, ",")[[1]])

cat("=============================================================================\n")
cat("Demographic Profile Report\n")
cat("=============================================================================\n")
cat(paste("Study Area:", study_area_name, "\n"))
cat(paste("Year:", year, survey, "\n"))
cat(paste("Geography:", geography, "\n"))
cat(paste("State:", state, "\n"))
if (!is.null(county)) cat(paste("County:", county, "\n"))
cat(paste("Variables:", length(variables), "\n"))
cat("=============================================================================\n\n")

# Check API key
if (Sys.getenv("CENSUS_API_KEY") == "") {
  stop("Census API key not found. Run 'Set Census API Key' tool first.")
}

# Load indicator config
cat("Loading indicator configuration...\n")
indicator_config <- fromJSON(config_file)

# --- Read Study Area ---
cat("Reading study area boundary...\n")
study_area <- st_read(study_area_path, quiet = TRUE)
study_area <- st_make_valid(study_area)
study_area <- st_transform(study_area, 4326)

# --- Download Census Data for Study Area ---
cat(paste("Downloading", geography, "data for", state, "...\n"))

census_params <- list(
  geography = geography,
  state = state,
  variables = variables,
  year = year,
  survey = survey,
  geometry = TRUE,
  output = "wide"
)

if (!is.null(county)) {
  census_params$county <- county
}

census_data <- tryCatch({
  do.call(get_acs, census_params)
}, error = function(e) {
  stop(paste("Error downloading Census data:", e$message))
})

cat(paste("Downloaded", nrow(census_data), "Census units\n"))

# Repair and transform
census_data <- st_make_valid(census_data)

# --- Intersect with Study Area ---
cat("Intersecting with study area...\n")

# Transform both to a projected CRS for accurate area calculations
# Use USA Contiguous Albers Equal Area (EPSG:5070) for CONUS
census_data <- st_transform(census_data, 5070)
study_area <- st_transform(study_area, 5070)

census_data$original_area <- as.numeric(st_area(census_data))
clipped <- st_intersection(census_data, study_area)
clipped$clipped_area <- as.numeric(st_area(clipped))
clipped$area_weight <- clipped$clipped_area / clipped$original_area

cat(paste("Found", nrow(clipped), "Census units intersecting study area\n"))

# --- Helper function to calculate indicator value ---
calc_indicator <- function(data, ind, weight_col = "area_weight") {
  if (ind$aggregation == "sum") {
    var_col <- paste0(ind$variable, "E")
    if (var_col %in% names(data)) {
      return(sum(data[[var_col]] * data[[weight_col]], na.rm = TRUE))
    }
  } else if (ind$aggregation == "weighted_median") {
    var_col <- paste0(ind$variable, "E")
    wt_col <- paste0(ind$weight_var, "E")
    if (var_col %in% names(data) && wt_col %in% names(data)) {
      weights <- data[[wt_col]] * data[[weight_col]]
      values <- data[[var_col]]
      valid <- !is.na(values) & !is.na(weights) & weights > 0
      if (sum(valid) > 0) {
        return(sum(values[valid] * weights[valid]) / sum(weights[valid]))
      }
    }
  } else if (ind$aggregation == "percent") {
    denom_col <- paste0(ind$denominator, "E")
    
    # Handle numerator (single or list)
    num_vars <- unlist(ind$numerator)
    num_sum <- 0
    for (nv in num_vars) {
      nc <- paste0(nv, "E")
      if (nc %in% names(data)) {
        num_sum <- num_sum + sum(data[[nc]] * data[[weight_col]], na.rm = TRUE)
      }
    }
    
    denom_sum <- if (denom_col %in% names(data)) {
      sum(data[[denom_col]] * data[[weight_col]], na.rm = TRUE)
    } else NA
    
    if (!is.na(denom_sum) && denom_sum > 0) {
      return((num_sum / denom_sum) * 100)
    }
  }
  return(NA)
}

# Helper for comparison geographies (no area weighting)
calc_indicator_direct <- function(data, ind) {
  if (ind$aggregation == "sum" || ind$aggregation == "weighted_median") {
    var_col <- paste0(ind$variable, "E")
    if (var_col %in% names(data)) {
      return(data[[var_col]][1])
    }
  } else if (ind$aggregation == "percent") {
    denom_col <- paste0(ind$denominator, "E")
    
    num_vars <- unlist(ind$numerator)
    num_sum <- 0
    for (nv in num_vars) {
      nc <- paste0(nv, "E")
      if (nc %in% names(data)) {
        num_sum <- num_sum + data[[nc]][1]
      }
    }
    
    denom_val <- if (denom_col %in% names(data)) data[[denom_col]][1] else NA
    
    if (!is.na(denom_val) && denom_val > 0) {
      return((num_sum / denom_val) * 100)
    }
  }
  return(NA)
}

# --- Aggregate Study Area Values ---
cat("Aggregating study area values...\n")

study_area_results <- list()
for (ind_name in names(indicator_config)) {
  ind <- indicator_config[[ind_name]]
  study_area_results[[ind_name]] <- calc_indicator(clipped, ind, "area_weight")
}

# --- Fetch Comparison Data ---
comparison_results <- list()

# County level
if (compare_county && !is.null(county)) {
  cat(paste("Fetching county data for", county, "...\n"))
  
  county_data <- tryCatch({
    get_acs(geography = "county", state = state, county = county,
            variables = variables, year = year, survey = survey, output = "wide")
  }, error = function(e) { warning(paste("Could not fetch county data:", e$message)); NULL })
  
  if (!is.null(county_data)) {
    comparison_results[["County"]] <- list()
    for (ind_name in names(indicator_config)) {
      comparison_results[["County"]][[ind_name]] <- calc_indicator_direct(county_data, indicator_config[[ind_name]])
    }
  }
}

# State level
if (compare_state) {
  cat(paste("Fetching state data for", state, "...\n"))
  
  state_data <- tryCatch({
    get_acs(geography = "state", state = state,
            variables = variables, year = year, survey = survey, output = "wide")
  }, error = function(e) { warning(paste("Could not fetch state data:", e$message)); NULL })
  
  if (!is.null(state_data)) {
    comparison_results[["State"]] <- list()
    for (ind_name in names(indicator_config)) {
      comparison_results[["State"]][[ind_name]] <- calc_indicator_direct(state_data, indicator_config[[ind_name]])
    }
  }
}

# National level
if (compare_nation) {
  cat("Fetching national data...\n")
  
  nation_data <- tryCatch({
    get_acs(geography = "us", variables = variables, year = year, survey = survey, output = "wide")
  }, error = function(e) { warning(paste("Could not fetch national data:", e$message)); NULL })
  
  if (!is.null(nation_data)) {
    comparison_results[["United States"]] <- list()
    for (ind_name in names(indicator_config)) {
      comparison_results[["United States"]][[ind_name]] <- calc_indicator_direct(nation_data, indicator_config[[ind_name]])
    }
  }
}

# --- Format Values ---
format_value <- function(val, format_type) {
  if (is.null(val) || is.na(val)) return("N/A")
  
  if (format_type == "number") {
    return(format(round(val), big.mark = ",", scientific = FALSE))
  } else if (format_type == "currency") {
    return(paste0("$", format(round(val), big.mark = ",", scientific = FALSE)))
  } else if (format_type == "percent") {
    return(paste0(round(val, 1), "%"))
  } else if (format_type == "decimal1") {
    return(format(round(val, 1), nsmall = 1))
  } else {
    return(as.character(round(val, 2)))
  }
}

# --- Build Output Table ---
cat("Building report...\n")

report_data <- data.frame(Indicator = names(indicator_config), stringsAsFactors = FALSE)

# Study area column
report_data[[study_area_name]] <- sapply(report_data$Indicator, function(ind) {
  format_value(study_area_results[[ind]], indicator_config[[ind]]$format)
})

# Comparison columns
for (geo_name in names(comparison_results)) {
  report_data[[geo_name]] <- sapply(report_data$Indicator, function(ind) {
    format_value(comparison_results[[geo_name]][[ind]], indicator_config[[ind]]$format)
  })
}

# --- Write Output ---
cat(paste("Writing output to:", output_file, "\n"))

output_ext <- tolower(tools::file_ext(output_file))

if (output_ext == "csv") {
  write.csv(report_data, output_file, row.names = FALSE)
  
} else if (output_ext == "xlsx") {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warning("openxlsx package not installed. Falling back to CSV.")
    csv_file <- sub("\\.xlsx$", ".csv", output_file)
    write.csv(report_data, csv_file, row.names = FALSE)
    cat(paste("Wrote CSV instead:", csv_file, "\n"))
  } else {
    library(openxlsx)
    
    wb <- createWorkbook()
    addWorksheet(wb, "Demographic Profile")
    
    writeData(wb, 1, paste("Demographic Profile:", study_area_name), startRow = 1, startCol = 1)
    writeData(wb, 1, paste("Data Source: ACS", year, survey), startRow = 2, startCol = 1)
    writeData(wb, 1, report_data, startRow = 4)
    
    title_style <- createStyle(fontSize = 14, textDecoration = "bold")
    header_style <- createStyle(textDecoration = "bold", border = "bottom", fgFill = "#4472C4", fontColour = "white")
    
    addStyle(wb, 1, title_style, rows = 1, cols = 1)
    addStyle(wb, 1, header_style, rows = 4, cols = 1:ncol(report_data), gridExpand = TRUE)
    setColWidths(wb, 1, cols = 1:ncol(report_data), widths = c(30, rep(18, ncol(report_data) - 1)))
    
    saveWorkbook(wb, output_file, overwrite = TRUE)
  }
  
} else if (output_ext == "html") {
  html_content <- paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n",
    "<title>Demographic Profile: ", study_area_name, "</title>\n",
    "<style>\n",
    "body { font-family: Arial, sans-serif; margin: 20px; }\n",
    "h1 { color: #333; }\n",
    "table { border-collapse: collapse; width: 100%; margin-top: 20px; }\n",
    "th, td { border: 1px solid #ddd; padding: 10px; text-align: left; }\n",
    "th { background-color: #4472C4; color: white; }\n",
    "tr:nth-child(even) { background-color: #f9f9f9; }\n",
    "tr:hover { background-color: #f1f1f1; }\n",
    ".source { color: #666; font-size: 0.9em; margin-top: 5px; }\n",
    "</style>\n</head>\n<body>\n",
    "<h1>Demographic Profile: ", study_area_name, "</h1>\n",
    "<p class='source'>Data Source: American Community Survey ", year, " (", survey, ")</p>\n",
    "<table>\n<tr>"
  )
  
  for (col in names(report_data)) {
    html_content <- paste0(html_content, "<th>", col, "</th>")
  }
  html_content <- paste0(html_content, "</tr>\n")
  
  for (i in 1:nrow(report_data)) {
    html_content <- paste0(html_content, "<tr>")
    for (j in 1:ncol(report_data)) {
      html_content <- paste0(html_content, "<td>", report_data[i, j], "</td>")
    }
    html_content <- paste0(html_content, "</tr>\n")
  }
  
  html_content <- paste0(html_content, "</table>\n</body>\n</html>")
  writeLines(html_content, output_file)
}

cat("\n=============================================================================\n")
cat("Report Summary:\n")
cat(paste("  Indicators:", nrow(report_data), "\n"))
cat(paste("  Study area census units:", nrow(clipped), "\n"))
cat(paste("  Comparison geographies:", length(comparison_results), "\n"))
cat("=============================================================================\n")
cat("Done!\n")