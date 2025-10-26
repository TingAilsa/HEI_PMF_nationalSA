# allYear_ML_HD_combine_joinOrgInput.R

library(dplyr)
library(fst)
library(data.table)
library(pryr)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds")
getwd()

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
source.test <- args[1]
cmaq_year <- args[2]

# source.test = "Traffic"; cmaq_year = "2011-2020"

cat("Processing source:", source.test, "\n")
cat("Period:", cmaq_year, "\n")

# Directory of annual combined file from all iterations
combine_path <- "ml_daily_pred_holdout/Annual_combine/"

#### Get averaged predictions for each day at each point ####
years <- 2011:2020

# file name pattern
file_paths <- paste0(combine_path,
                     "RF_Pred_HD_US_01_allDaily_",
                     source.test, "_", years, ".fst")
cat("Looking for files:\n")
print(file_paths)

# Read and combine all yearly files
avg_all_iteration_pred <- 
  rbindlist(lapply(file_paths, read_fst))

# Save combined result
output_file <- 
  paste0(combine_path,
         "RF_Pred_HD_US_01_allDaily_mean_Predictions_",
         source.test, "_2011-2020.fst")
write_fst(avg_all_iteration_pred, output_file)

#### Join the other variables ####

# Read the input file for predictions
file_name <- 
  paste0(source.test, "_ML_input_mainlandUS_", cmaq_year, ".fst")
model_input_all_grid = 
  read_fst(
    paste0("machine_learning_source_input/", file_name))

# Add predictions into the file
model_input_all_grid_pred =
  merge(model_input_all_grid, avg_all_iteration_pred,
        by = c("Date", "Longitude", "Latitude"))

# Save combined results
write_fst(model_input_all_grid_pred,
          paste0("ml_daily_pred_holdout/Annual_combine/",
                 "RF_Pred_HD_US_01_", 
                 source.test, "_", cmaq_year, ".fst"))

rm(model_input_all_grid_pred)
rm(avg_all_iteration_pred)
gc()

