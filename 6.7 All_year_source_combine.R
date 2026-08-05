# 6.7 All_year_source_input.R

# rm(list = ls())

library(fst)
library(dplyr)
library(purrr)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
getwd()
base_dir = getwd()

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
source_name <- args[1]

# Function replacing NAs only with overall geometric mean
# add a small value to 0
replace_na_with_geomean <- function(x, small_value = 1e-6) {
  # Calculate geometric mean of non-NA values
  non_na_values <- x[!is.na(x)]
  
  # Check if there are any non-NA values
  if(length(non_na_values) == 0) {
    warning("All values are NA, returning original vector")
    return(x)
  }
  
  if(any(non_na_values <= 0)) {
    # Add small value to zeros and negative values
    non_na_values[non_na_values <= 0] <- small_value
    warning("Non-positive values found. Replaced with small positive value for calculation.")
  }
  
  # Calculate geometric mean
  geom_mean <- exp(mean(log(non_na_values)))
  
  # Replace NAs with geometric mean
  x[is.na(x)] <- geom_mean
  
  return(x)
}

# Handle the NAs for columns used for prediction
handle_nas <- function(df) {
  # Define columns to exclude from NA handling
  exclude_cols <- c("Longitude", "Latitude", "Dataset", "Source_aftermanual", 
                    "SiteCode", "Date", "grid_ID")
  
  # Identify columns to process
  process_cols <- setdiff(names(df), exclude_cols)
  
  # Check NAs before
  na_counts <- colSums(is.na(df[, process_cols]))
  na_cols <- na_counts[na_counts > 0]
  
  if(length(na_cols) > 0) {
    cat("Columns with NAs (before replacement):\n")
    print(na_cols)
    
    # Apply geometric mean replacement to columns with NAs
    for(col in names(na_cols)) {
      cat("Replacing NAs in:", col, "\n")
      df[[col]] <- replace_na_with_geomean(df[[col]])
    }
    
    # Verify no NAs remain
    remaining_nas <- colSums(is.na(df[, process_cols]))
    remaining_nas <- remaining_nas[remaining_nas > 0]
    if(length(remaining_nas) > 0) {
      warning("NAs still remain in: ", paste(names(remaining_nas), collapse = ", "))
    } else {
      cat("All NAs successfully replaced.\n")
    }
    
  } else {
    cat("No NAs found in any processed columns.\n")
  }
  
  return(df)
}

## Parameter settings
# Define the years
# cmaq_years = 2011:2019
cmaq_years = 2011:2020
# Create period strings for each year
cmaq_periods = paste0(cmaq_years, "-01_", cmaq_years, "-12")
cat(cmaq_periods, "\n")

# Create strings for titles and file names
included_years = paste(cmaq_years, collapse = "&")
included_year_title = paste(min(cmaq_years), max(cmaq_years), sep = "-")

print(paste("Study periods:", paste(cmaq_periods, collapse = ", ")))
print(paste("Included years:", included_years))
cat(included_year_title, "\n")

# midfix_dfs = c("_ML_input_mainlandUS_", "_ML_input_only_PMF_sites_")
# midfix_dfs = c("_ML_Daily_mainlandUS_", "_ML_Daily_only_PMF_sites_")
midfix_dfs = c("_ML_Daily_only_PMF_sites_")

#### Process each source 
for(midfix_data in midfix_dfs) { 
  # midfix_data = midfix_dfs[1]; source_name = "Traffic"
  
  cat("Processing source:", source_name, "\n")
  cat("Processing dataset:", midfix_data, "\n")
  
  #### Combine each source all year data into a list
  # Create the list
  all_data_OneSource <- list()
  
  for (i in seq_along(cmaq_periods)) {
    process_period <- cmaq_periods[i]
    process_year <- cmaq_years[i]
    
    # Find files for this source from all years
    source_file_name_pattern <- 
      paste0(source_name, midfix_data, ".*_days_", 
             process_period, "\\.fst$")
    source_file_list <- 
      list.files(pattern = source_file_name_pattern, full.names = TRUE)
    
    if(length(source_file_list) > 0){
      cat("Files to process:", "\n", source_file_list, "\n")
      
      # Read and combine all files from all cmaq_years
      source_year_data <- map_dfr(source_file_list, read_fst)
      source_year_data$year = process_year
      
      all_data_OneSource[[as.character(process_year)]] <- source_year_data
      # all_data_OneSource[[as.character(2017)]]
    }
  }
  
  #### Identify common columns and combine data
  # Find common columns
  common_cols <- Reduce(intersect, map(all_data_OneSource, names))
  
  # Subset and combine
  combined_OneSource <- map_dfr(all_data_OneSource, ~.x[, common_cols])
  
  # Sort columns (Year at the end)
  col_order <- c(setdiff(common_cols, "year"), "year")
  combined_OneSource <- combined_OneSource[, col_order]
  cat("dim of combined_OneSource:", dim(combined_OneSource), "\n")
  cat("All included years:", unique(combined_OneSource$year), "\n")
  
  # Handle NAs, mostly in vs columns
  combined_OneSource = handle_nas(combined_OneSource)
  
  # Save
  output_file <- paste0(source_name, midfix_data, 
                        included_year_title, ".fst")
  write_fst(combined_OneSource, output_file)
  
  cat("Saved:", output_file, "\n")
}
