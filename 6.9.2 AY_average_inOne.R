# AY_average_inOne.R

library(dplyr)
library(fst)
library(stringr)
library(lubridate)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/Annual_combine")
getwd()

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
source_name <- args[1]

###### Combine single source averages to that of all sources ######
# Define the categories to combine
categories <- c("allinOne", "overall", "annual", "year-month", "month")

for (cat in categories) { # cat = categories[1]
  # Find all files matching the current category
  # This looks for files ending in e.g., "_overall.fst"
  pattern <- paste0("_Pred_US_01_", cat, "\\.fst$")
  cat_files <- list.files(pattern = pattern, full.names = TRUE)
  
  if (length(cat_files) > 0) {
    message("Combining ", length(cat_files), " files for category: ", cat)
    
    # Read all files in the category and stack them
    # as.data.table = TRUE ensures we keep the speed advantage
    combined_dt <- rbindlist(lapply(cat_files, read_fst, as.data.table = TRUE), use.names = TRUE)
    
    # Save the master file
    output_name <- paste0("Combined_US_01_", cat, ".fst")
    write_fst(combined_dt, output_name) #
    
    message("Saved: ", output_name)
    
    # Clean up memory before next category
    rm(combined_dt)
    gc()
  }
}

