# 6.7 All_year_source_input.R

# rm(list = ls())

library(fst)
library(dplyr)
library(purrr)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
getwd()
base_dir = getwd()


## Parameter settings
pred_sources = c("Sulfate", "Dust", "Biomass", "Traffic", "Nitrate", "PM25") # , "Industry",
# pred_sources = c("Traffic") # , "Industry",

# Define the years
cmaq_years = 2011:2019
# cmaq_years = 2011:2020
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
midfix_dfs = c("_ML_Daily_mainlandUS_", "_ML_Daily_only_PMF_sites_")

#### Process each source 
for(midfix_data in midfix_dfs) {
  for (source_name in pred_sources) { # source_name = "Biomass"
    cat("Processing source:", source_name, "\n")
    
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
    
    
    ######## fix the NAs in vs (to be updated later)
    
    
    # Save
    output_file <- paste0(source_name, midfix_data, 
                          included_year_title, ".fst")
    write_fst(combined_OneSource, output_file)
    
    cat("Saved:", output_file, "\n")
  }
}


###### Special for PM2.5 #########
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

# Mainland
# mainland_pm = read_fst("PM25_ML_input_mainlandUS_2011-2020.fst")
mainland_pm = read_fst("PM25_ML_Daily_mainlandUS_2011-2019.fst")
mainland_pm$O3 =replace_na_with_geomean(mainland_pm$O3)
mainland_pm$OTA =replace_na_with_geomean(mainland_pm$OTA)
mainland_pm$NRD =replace_na_with_geomean(mainland_pm$NRD)
mainland_pm$ASEA =replace_na_with_geomean(mainland_pm$ASEA)
mainland_pm$ARS =replace_na_with_geomean(mainland_pm$ARS)
mainland_pm$vs =replace_na_with_geomean(mainland_pm$vs)
# write_fst(mainland_pm, "PM25_ML_input_mainlandUS_2011-2020.fst")
write_fst(mainland_pm, "PM25_ML_Daily_mainlandUS_2011-2019.fst")

# PMF sites, all points
# pmf_site_pm = read_fst("PM25_ML_input_only_PMF_sites_2011-2020.fst")
pmf_site_pm = read_fst("PM25_ML_Daily_only_PMF_sites_2011-2019.fst")
summary(pmf_site_pm)
pmf_site_pm$O3 =replace_na_with_geomean(pmf_site_pm$O3)
pmf_site_pm$OTA =replace_na_with_geomean(pmf_site_pm$OTA)
pmf_site_pm$vs =replace_na_with_geomean(pmf_site_pm$vs)
# pmf_site_pm = read_fst("PM25_ML_input_only_PMF_sites_2011-2020.fst")
pmf_site_pm = read_fst("PM25_ML_Daily_only_PMF_sites_2011-2019.fst")

