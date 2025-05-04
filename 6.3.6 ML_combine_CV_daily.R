# ML_combine_CV_daily.R

library(dplyr)
library(fst)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_prediction")
getwd()

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
source_idx <- as.numeric(args[1])  # Array task ID for source
cmaq_period <- args[2]

# Define source list
source_list <- c("Sulfate", "Traffic", "Dust", "Biomass")
source.test <- source_list[source_idx]

# Extract study year
cmaq_year = substring(cmaq_period, 1, 4)

print(paste("Processing source:", source.test))
print(paste("Period:", cmaq_period))
print(paste("Year:", cmaq_year))

# Get all prediction files for this source and period
prediction_dir <- paste0(source.test, "/", cmaq_year)
prediction_files <- 
  list.files(prediction_dir, 
             pattern = "_CV_predictions_\\d{4}-\\d{2}-\\d{2}\\.fst$", 
             full.names = TRUE)

print("The first five files in prediction_files")
prediction_files[1:5]
print("Total file numbers in prediction_files")
length(prediction_files)

# Combine all files
combined_predictions <- 
  lapply(prediction_files, read_fst) %>%
  bind_rows()

print("Check the combined predictions")
head(combined_predictions); dim(combined_predictions)
# summary(combined_predictions)

# Directory of annual combined file
combine_path <- "Annual_combine/"

# Save combined results
write_fst(combined_predictions,
          paste0(combine_path, 
                 "RF_Pred_mainland_US_01_grids_noUnc_noCoords_", source.test, "_", cmaq_period, ".fst"))

