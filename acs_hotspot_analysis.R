# =============================================================================
# Hot Spot Analysis (Getis-Ord Gi*)
# MOE-aware spatial clustering analysis
# =============================================================================

library(sf)
library(spdep)
library(dplyr)

# Disable s2
sf_use_s2(FALSE)

# --- Parse Arguments ---
args <- commandArgs(trailingOnly = TRUE)

input_fc <- args[1]
analysis_field <- args[2]
moe_field <- if (args[3] == "") NULL else args[3]
spatial_method <- args[4]  # "contiguity", "distance", "knn"
distance_band <- as.numeric(args[5])
num_neighbors <- as.integer(args[6])
significance_level <- as.numeric(args[7])
apply_moe_adjustment <- toupper(args[8]) == "TRUE"
output_file <- args[9]

cat("=============================================================================\n")
cat("Hot Spot Analysis (Getis-Ord Gi*)\n")
cat("=============================================================================\n")
cat(paste("Input:", input_fc, "\n"))
cat(paste("Analysis Field:", analysis_field, "\n"))
if (!is.null(moe_field)) cat(paste("MOE Field:", moe_field, "\n"))
cat(paste("Spatial Method:", spatial_method, "\n"))
if (spatial_method == "distance") cat(paste("Distance Band:", distance_band, "meters\n"))
if (spatial_method == "knn") cat(paste("K Neighbors:", num_neighbors, "\n"))
cat(paste("Significance Level:", significance_level, "\n"))
cat(paste("MOE Adjustment:", apply_moe_adjustment, "\n"))
cat("=============================================================================\n\n")

# --- Read Input ---
cat("Reading input features...\n")

# Parse file geodatabase path
if (grepl("\\.gdb", input_fc, ignore.case = TRUE)) {
  parts <- strsplit(input_fc, "[\\/](?=[^\\/]*$)", perl = TRUE)[[1]]
  if (length(parts) == 2 && grepl("\\.gdb$", parts[1], ignore.case = TRUE)) {
    data <- st_read(dsn = parts[1], layer = parts[2], quiet = TRUE)
  } else {
    data <- st_read(input_fc, quiet = TRUE)
  }
} else {
  data <- st_read(input_fc, quiet = TRUE)
}

cat(paste("Read", nrow(data), "features\n"))

# Validate analysis field
if (!analysis_field %in% names(data)) {
  stop(paste("Analysis field not found:", analysis_field))
}

# Check for valid values
data$analysis_value <- as.numeric(data[[analysis_field]])
valid_count <- sum(!is.na(data$analysis_value))
cat(paste("Valid values:", valid_count, "\n"))

if (valid_count < 10) {
  stop("Not enough valid values for hot spot analysis (minimum 10 required)")
}

# --- Project to Planar CRS ---
cat("Projecting to planar coordinate system...\n")
data <- st_transform(data, 5070)  # USA Contiguous Albers Equal Area

# --- Handle MOE ---
if (!is.null(moe_field) && moe_field %in% names(data)) {
  cat("Processing MOE field...\n")
  data$moe_value <- as.numeric(data[[moe_field]])
  
  # Calculate CV (Coefficient of Variation)
  # CV = (MOE / 1.645) / Estimate * 100
  data$CV <- ifelse(
    !is.na(data$analysis_value) & data$analysis_value != 0 & !is.na(data$moe_value),
    (data$moe_value / 1.645) / abs(data$analysis_value) * 100,
    NA
  )
  
  # Classify reliability
  data$Reliability <- case_when(
    is.na(data$CV) ~ "Unknown",
    data$CV < 12 ~ "High",
    data$CV <= 40 ~ "Medium",
    TRUE ~ "Low"
  )
  
  reliability_table <- table(data$Reliability)
  cat("Reliability distribution:\n")
  print(reliability_table)
  cat("\n")
} else {
  data$CV <- NA
  data$Reliability <- NA
  apply_moe_adjustment <- FALSE
}

# --- Create Spatial Weights ---
cat(paste("Creating spatial weights (", spatial_method, ")...\n", sep = ""))

# Handle missing values - create subset for analysis
data$row_id <- seq_len(nrow(data))
analysis_subset <- data[!is.na(data$analysis_value), ]

if (spatial_method == "contiguity") {
  # Queen contiguity (shared edge or vertex)
  nb <- poly2nb(analysis_subset, queen = TRUE)
  
} else if (spatial_method == "distance") {
  # Distance band
  coords <- st_coordinates(st_centroid(analysis_subset))
  nb <- dnearneigh(coords, 0, distance_band)
  
} else if (spatial_method == "knn") {
  # K nearest neighbors
  coords <- st_coordinates(st_centroid(analysis_subset))
  nb <- knn2nb(knearneigh(coords, k = num_neighbors))
}

# Check for islands (features with no neighbors)
n_islands <- sum(card(nb) == 0)
if (n_islands > 0) {
  cat(paste("Warning:", n_islands, "features have no neighbors\n"))
}

# Convert to weights list (row standardized)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

cat(paste("Average number of neighbors:", round(mean(card(nb)), 1), "\n\n"))

# --- Run Getis-Ord Gi* ---
cat("Calculating Getis-Ord Gi* statistics...\n")

# localG returns z-scores
gi_result <- localG(analysis_subset$analysis_value, lw, zero.policy = TRUE)

# Store results
analysis_subset$Gi_Zscore <- as.numeric(gi_result)

# Calculate p-values (two-tailed)
analysis_subset$Gi_Pvalue <- 2 * pnorm(-abs(analysis_subset$Gi_Zscore))

# --- Classify Hot/Cold Spots ---
cat("Classifying hot spots and cold spots...\n")

classify_hotspot <- function(zscore, pvalue, sig_level) {
  if (is.na(zscore) || is.na(pvalue)) return("Not Significant")
  
  if (pvalue <= 0.01) {
    conf <- "99% Confidence"
  } else if (pvalue <= 0.05) {
    conf <- "95% Confidence"
  } else if (pvalue <= 0.10) {
    conf <- "90% Confidence"
  } else {
    return("Not Significant")
  }
  
  # Check against user's significance threshold
  if (pvalue > sig_level) {
    return("Not Significant")
  }
  
  if (zscore > 0) {
    return(paste("Hot Spot -", conf))
  } else {
    return(paste("Cold Spot -", conf))
  }
}

analysis_subset$Gi_Bin <- mapply(
  classify_hotspot,
  analysis_subset$Gi_Zscore,
  analysis_subset$Gi_Pvalue,
  MoreArgs = list(sig_level = significance_level)
)

# --- Apply MOE Adjustment ---
if (apply_moe_adjustment) {
  cat("Applying MOE reliability adjustment...\n")
  
  adjust_classification <- function(gi_bin, reliability, pvalue) {
    if (is.na(reliability) || reliability == "Unknown") {
      return(gi_bin)
    }
    
    if (reliability == "Low") {
      return("Unreliable Estimate")
    }
    
    if (reliability == "Medium") {
      # Require stronger significance for medium reliability
      # Downgrade 90% confidence results to Not Significant
      if (grepl("90% Confidence", gi_bin)) {
        return("Not Significant (Medium Reliability)")
      }
    }
    
    return(gi_bin)
  }
  
  analysis_subset$Gi_Bin_Adjusted <- mapply(
    adjust_classification,
    analysis_subset$Gi_Bin,
    analysis_subset$Reliability,
    analysis_subset$Gi_Pvalue
  )
} else {
  analysis_subset$Gi_Bin_Adjusted <- analysis_subset$Gi_Bin
}

# --- Summary Statistics ---
cat("\n=============================================================================\n")
cat("Results Summary:\n")
cat("=============================================================================\n")

# Raw classification
cat("\nRaw Classification:\n")
print(table(analysis_subset$Gi_Bin))

if (apply_moe_adjustment) {
  cat("\nMOE-Adjusted Classification:\n")
  print(table(analysis_subset$Gi_Bin_Adjusted))
}

# Count significant clusters
hot_spots <- sum(grepl("Hot Spot", analysis_subset$Gi_Bin_Adjusted))
cold_spots <- sum(grepl("Cold Spot", analysis_subset$Gi_Bin_Adjusted))
not_sig <- sum(analysis_subset$Gi_Bin_Adjusted == "Not Significant" | 
                 grepl("Not Significant", analysis_subset$Gi_Bin_Adjusted))

cat(paste("\nHot Spots:", hot_spots, "\n"))
cat(paste("Cold Spots:", cold_spots, "\n"))
cat(paste("Not Significant:", not_sig, "\n"))

# --- Merge Results Back to Full Dataset ---
cat("\nMerging results...\n")

# Initialize result columns in full dataset
data$Gi_Zscore <- NA
data$Gi_Pvalue <- NA
data$Gi_Bin <- "Not Analyzed"
data$Gi_Bin_Adjusted <- "Not Analyzed"

# Match by row_id
match_idx <- match(analysis_subset$row_id, data$row_id)
data$Gi_Zscore[match_idx] <- analysis_subset$Gi_Zscore
data$Gi_Pvalue[match_idx] <- analysis_subset$Gi_Pvalue
data$Gi_Bin[match_idx] <- analysis_subset$Gi_Bin
data$Gi_Bin_Adjusted[match_idx] <- analysis_subset$Gi_Bin_Adjusted

# Clean up temp columns
data$row_id <- NULL
data$analysis_value <- NULL
data$moe_value <- NULL

# Round numeric fields
data$Gi_Zscore <- round(data$Gi_Zscore, 4)
data$Gi_Pvalue <- round(data$Gi_Pvalue, 6)
data$CV <- round(data$CV, 2)

# --- Write Output ---
cat(paste("\nWriting output to:", output_file, "\n"))

st_write(data, output_file, layer = "output", delete_dsn = TRUE, quiet = TRUE)

cat("\n=============================================================================\n")
cat("Hot spot analysis complete!\n")
cat("=============================================================================\n")
cat("Output fields:\n")
cat("  Gi_Zscore - Getis-Ord Gi* z-score\n")
cat("  Gi_Pvalue - P-value\n")
cat("  Gi_Bin - Hot/Cold spot classification\n")
if (apply_moe_adjustment) {
  cat("  Gi_Bin_Adjusted - MOE-adjusted classification\n")
  cat("  CV - Coefficient of Variation\n")
  cat("  Reliability - Estimate reliability (High/Medium/Low)\n")
}
cat("=============================================================================\n")
cat("Done!\n")