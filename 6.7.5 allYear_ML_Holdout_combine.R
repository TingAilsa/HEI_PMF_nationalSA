# allYear_ML_Holdout_combine.R

library(dplyr)
library(fst)
library(data.table)
library(pryr)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout")
getwd()

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
source.test <- args[1] 
cmaq_year <- args[2]
iteration_group <- as.integer(args[3]) # Ensure numeric format
time_period <- args[4] 

# source.test = "Traffic"; cmaq_year = "2013-01_2013-12"

cat("Processing source:", source.test, "\n")
cat("Period:", cmaq_year, "\n")
cat("iteration_group:", iteration_group, "\n")

# Function to filter data by year column
filter_data_by_year <- function(data, time_period) {
  if(time_period == "2011-2015") {
    return(data[year <= 2015])
  } else {
    return(data[year > 2015])
  }
}

# Directory of annual combined file from all iterations
combine_path <- "Annual_combine/"

# Determine iteration range
if(iteration_group == 1) {
  iter_start <- 1
  iter_end <- 25
  group_suffix <- "_1"
} else {
  iter_start <- 26
  iter_end <- 50
  group_suffix <- "_2"
}

# Determine year threshold for filtering
if(time_period == "2011-2015") {
  period_suffix <- "_2011-2015"
  cat("Will filter for years <= 2015\n")
} else if(time_period == "2016-2020") {
  period_suffix <- "_2016-2020"
  cat("Will filter for years > 2015\n")
} else {
  stop("Invalid time_period. Must be '2011-2015' or '2016-2020'")
}

cat("Processing iterations", iter_start, "to", iter_end, "\n")

# Initiate a list to store predictions from all iterations
iteration_predictions_list <- vector("list", 25)

###### Process iterations and add data into the list ###### 
for(iteration in iter_start:iter_end) { # 25, iteration number
  list_index <- iteration - iter_start + 1 
  cat("Processing iteration:", iteration, "(list index:", list_index, ")\n")
  
  # Get all prediction files for this source and period
  prediction_dir <- 
    paste0(source.test, "/", cmaq_year, "/Iteration_", iteration)
  prediction_files <-
    list.files(prediction_dir,
               pattern = "_holdout_pred_\\d{4}-\\d{2}-\\d{2}\\.fst$",
               full.names = TRUE)
  
  print("The first five files from the current Iteration")
  prediction_files[1:5]
  print("Number of files found: for each Iteration")
  length(prediction_files)
  
  # Store combined predictions for this iteration, use data.table for more storage-efficient process
  iteration_predictions_list[[list_index]] <- 
    filter_data_by_year(
      rbindlist(lapply(prediction_files, function(x) {
        as.data.table(read_fst(x))
      })), 
      time_period
    )[, Iteration := iteration]
  
  # Clean up memory
  gc()
}

print("Start to combine all results and get the averages")

# Combine all iterations efficiently using bind_rows
# For scripts include large datasets, clean memory from those no longer needed
all_combined_iteration_pred <- rbindlist(iteration_predictions_list)
rm(iteration_predictions_list)
gc()

# Save combined results
write_fst(all_combined_iteration_pred,
          paste0(combine_path,
                 "RF_Pred_HD_US_01_allDaily_", 
                 source.test, period_suffix, group_suffix, ".fst"))
rm(all_combined_iteration_pred)
gc()

