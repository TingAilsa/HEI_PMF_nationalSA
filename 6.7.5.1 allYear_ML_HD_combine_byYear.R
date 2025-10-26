# allYear_ML_HD_combine_byYear.R

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
time_year <- args[3]

# source.test = "PM25"; cmaq_year = "2011-2020"; time_year = 2017

cat("Processing source:", source.test, "\n")
cat("Processing iterations 1 to 50 for year", time_year, "\n")

# Directory of annual combined file from all iterations
combine_path <- "Annual_combine/"

# Initiate a list to store predictions from all iterations
iteration_predictions_list <- vector("list", 50)

###### Process iterations and add data into the list ###### 
for(iteration in 1:50) { # all 50 iterations
  cat("Processing iteration:", iteration, "\n")
  
  # Get all prediction files for this source and period
  prediction_dir <-
    paste0(source.test, "/", cmaq_year, "/Iteration_", iteration)
  
  # Filter files by specific year in filename
  all_files <- list.files(prediction_dir,
                          pattern = "_holdout_pred_\\d{4}-\\d{2}-\\d{2}\\.fst$",
                          full.names = TRUE)
  
  # Filter for specific year
  year_pattern <- paste0("_holdout_pred_", time_year, "-\\d{2}-\\d{2}\\.fst$")
  prediction_files <- all_files[grepl(year_pattern, all_files)]
  
  cat("Total files found:", length(all_files), "\n")
  cat("Files for year", time_year, ":", length(prediction_files), "\n")
  
  # Store combined predictions for this iteration, use data.table for more storage-efficient process
  iteration_predictions_list[[iteration]] <-
    rbindlist(lapply(prediction_files, function(x) {
      as.data.table(read_fst(x))
    }))[, Iteration := iteration]
  
  # Clean up memory
  gc()
}

print("Start to combine all results and get the averages")

# Combine all iterations efficiently using bind_rows
# For scripts include large datasets, clean memory from those no longer needed
all_combined_iteration_pred <- rbindlist(iteration_predictions_list)
rm(iteration_predictions_list)
gc()

# head(all_combined_iteration_pred); dim(all_combined_iteration_pred); summary(all_combined_iteration_pred)
write_fst(all_combined_iteration_pred,
          paste0(combine_path,
                 "RF_Pred_HD_US_01_allDaily_allIterations_",
                 source.test, "_", time_year, ".fst"))

# Get the mean and sd of each day at each grid
all_combined_iteration_mean_pred <-
  all_combined_iteration_pred %>%
  group_by(Date, Longitude, Latitude) %>%
  summarise(
    Pred.sd = sd(Predictions),
    Predictions = mean(Predictions),
    .groups = "drop"
  )

# rm(all_combined_iteration_pred)

# head(all_combined_iteration_mean_pred); dim(all_combined_iteration_mean_pred); summary(all_combined_iteration_m$
write_fst(all_combined_iteration_mean_pred,
          paste0(combine_path,
                 "RF_Pred_HD_US_01_allDaily_",
                 source.test, "_", time_year, ".fst"))

rm(all_combined_iteration_mean_pred)
gc()