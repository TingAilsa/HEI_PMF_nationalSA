# module load gnu10 openmpi r
# 6.7.2.1 allYear_ML_HD_parameter_combine.R

library(dplyr)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
getwd()

# Get all CSV files in tuning_results folder
parameter_csvs <- 
  list.files("tuning_results", pattern = "\\.csv$", full.names = TRUE)

cat("Found", length(parameter_csvs), "CSV files\n")

# Read and combine all CSV files
all_parameter <- 
  do.call(rbind, 
          lapply(parameter_csvs,
                 function(file) {
                   cat("Reading:", basename(file), "\n")
                   read.csv(file, stringsAsFactors = FALSE)
                 }))
head(all_parameter); dim(all_parameter)

# Combined ranking based on normalize RMSE and R2
# To get a lower is better combined_score, flip the R2 so that higher R2 leads to lower r2_normalized
all_parameter_marked <- 
  all_parameter %>%
  group_by(source) %>%
  mutate(
    # Normalize metrics
    rmse_normalized = 
      (test_rmse - min(test_rmse)) / (max(test_rmse) - min(test_rmse)),
    r2_normalized = 
      (max(test_r2) - test_r2) / (max(test_r2) - min(test_r2)),
    
    # Get combined score
    combined_score = 0.7*rmse_normalized + 0.3*r2_normalized,  # Lower is better
    
    # Mark best for each metric
    lowest_RMSE = ifelse(test_rmse == min(test_rmse), TRUE, FALSE), # lowest RMSE (best)
    highest_R2 = ifelse(test_r2 == max(test_r2), TRUE, FALSE), # highest R² (best)
    lowest_combined = ifelse(combined_score == min(combined_score), TRUE, FALSE) # lowest combined score (best)
  ) %>%
  ungroup()
head(all_parameter_marked)
names(all_parameter_marked)

# Detect the best mtry and ntree combination for each source
best_params_combined <- 
  all_parameter_marked %>%
  filter(lowest_RMSE | highest_R2 | lowest_combined)

# Save files
write.csv(all_parameter_marked, row.names = FALSE, 
          "Source_RF_2011-2020_hyperparameter_all.csv")
write.csv(best_params_combined, row.names = FALSE, 
          "Source_RF_2011-2020_hyperparameter_BEST.csv")

cat("\nFiles saved to:", getwd(), "\n")