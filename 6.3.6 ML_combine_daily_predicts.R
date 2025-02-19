# combine_predictions.R
library(dplyr)
library(fst)

args <- commandArgs(trailingOnly = TRUE)
source.test <- args[1]
cmaq_period <- args[2]

# Get all prediction files for this source and period
prediction_dir <- paste0("predictions_", source.test, "_", cmaq_period)
prediction_files <- list.files(prediction_dir, pattern = "predictions_\\d{8}\\.fst$", full.names = TRUE)

# Combine all files
combined_predictions <- lapply(prediction_files, read_fst) %>%
  bind_rows()

# Save combined results
write_fst(combined_predictions,
          paste0("RF_mainland_grids_No_coords_", source.test, "_", cmaq_period, ".fst"))