# =============================================================================
# Batch Generate All ACS Variable Lookup Files
# 
# Generates lookup files for all supported year/survey combinations:
# - ACS 5-Year: 2009-2023
# - ACS 1-Year: 2005-2019, 2021-2023 (2020 not released due to COVID)
#
# Run this script once to generate all files for toolbox distribution.
# =============================================================================

# Source the main generate function
source("generate_acs_lookup.R")

# Define all year/survey combinations
acs5_years <- 2009:2023  # 15 years
acs1_years <- c(2005:2019, 2021:2023)  # 18 years (skip 2020)

# Create output directory if needed
output_dir <- "lookups"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  cat(paste("Created output directory:", output_dir, "\n\n"))
}

# Track results
results <- data.frame(
  year = integer(),
  survey = character(),
  status = character(),
  variables = integer(),
  stringsAsFactors = FALSE
)

# Generate ACS 5-Year lookups
cat("=============================================================================\n")
cat("Generating ACS 5-Year lookup files (2009-2023)\n")
cat("=============================================================================\n\n")

for (year in acs5_years) {
  output_file <- file.path(output_dir, paste0("acs_variable_lookup_", year, "_acs5.json"))
  cat(paste("\n--- Processing:", year, "acs5 ---\n"))
  
  tryCatch({
    result <- generate_lookup(year, "acs5", output_file)
    results <- rbind(results, data.frame(
      year = year,
      survey = "acs5",
      status = "Success",
      variables = result$metadata$total_variables
    ))
  }, error = function(e) {
    cat(paste("ERROR:", e$message, "\n"))
    results <<- rbind(results, data.frame(
      year = year,
      survey = "acs5",
      status = paste("Failed:", e$message),
      variables = 0
    ))
  })
  
  # Brief pause to avoid overwhelming the API
  Sys.sleep(2)
}

# Generate ACS 1-Year lookups
cat("\n\n=============================================================================\n")
cat("Generating ACS 1-Year lookup files (2005-2019, 2021-2023)\n")
cat("=============================================================================\n\n")

for (year in acs1_years) {
  output_file <- file.path(output_dir, paste0("acs_variable_lookup_", year, "_acs1.json"))
  cat(paste("\n--- Processing:", year, "acs1 ---\n"))
  
  tryCatch({
    result <- generate_lookup(year, "acs1", output_file)
    results <- rbind(results, data.frame(
      year = year,
      survey = "acs1",
      status = "Success",
      variables = result$metadata$total_variables
    ))
  }, error = function(e) {
    cat(paste("ERROR:", e$message, "\n"))
    results <<- rbind(results, data.frame(
      year = year,
      survey = "acs1",
      status = paste("Failed:", e$message),
      variables = 0
    ))
  })
  
  # Brief pause to avoid overwhelming the API
  Sys.sleep(2)
}

# Final summary
cat("\n\n=============================================================================\n")
cat("BATCH GENERATION COMPLETE\n")
cat("=============================================================================\n\n")

successful <- sum(results$status == "Success")
failed <- sum(results$status != "Success")

cat(paste("Total files attempted:", nrow(results), "\n"))
cat(paste("Successful:", successful, "\n"))
cat(paste("Failed:", failed, "\n\n"))

if (failed > 0) {
  cat("Failed combinations:\n")
  print(results[results$status != "Success", c("year", "survey", "status")])
}

cat(paste("\nLookup files saved to:", normalizePath(output_dir), "\n"))
cat("\nCopy the contents of the 'lookups' folder to your toolbox directory.\n")