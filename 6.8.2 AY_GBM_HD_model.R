# 6.8.2 AY_GBM_HD_model.R

library(base)
library(stringr)
library(dplyr)
library(tidyr)
library(data.table)
library(fst)
library(caret)
library(gbm)

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
iteration <- as.integer(args[1])  # Array task ID will be the iteration number, ensure integer format
source.test <- args[2]
included_years <- args[3]
n_trees_best <- as.integer(args[4])
interaction_depth_best <- as.integer(args[5])
shrinkage_best <- as.numeric(args[6])

# iteration=1; source.test="PM25"; included_years="2011-2020"; n_trees_best = 500; interaction_depth_best = 5

# Set working directory
setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
base_dir <- getwd()

# Function definition remains the same
#### Functions to use ####
gbm_holdout_predictions_noUnc <-
  function(model_withPMF_data, iteration, n_trees_use, interaction_depth_use) {
    
    # Randomly select 80% of sites for training
    unique_sites <- unique(model_withPMF_data$grid_ID)
    train_sites <- sample(unique_sites, size = floor(0.8 * length(unique_sites)))
    
    # Create training and test datasets
    train_data <- model_withPMF_data %>% filter(grid_ID %in% train_sites)
    test_data <- model_withPMF_data %>% filter(!grid_ID %in% train_sites)
    
    # Remove columns not to use
    train_data_use <- dplyr::select(train_data, -Date, -grid_ID)
    test_data_use <- dplyr::select(test_data, -Date, -grid_ID)
    
    # Fit the GBM model on training data
    gbm_model_hold <- gbm(
      PMF_conc ~ .,
      data = train_data_use,
      distribution = "gaussian",
      n.trees = n_trees_best,
      interaction.depth = interaction_depth_use,
      shrinkage = shrinkage_best,
      #n.minobsinnode = 10,
      #bag.fraction = 0.8,
      #train.fraction = 1.0,
      #cv.folds = 0
      verbose = FALSE
    )
    
    # Get predictions
    train_data$Predictions <- predict(gbm_model_hold, train_data_use, n.trees = n_trees_use)
    test_data$Predictions <- predict(gbm_model_hold, test_data_use, n.trees = n_trees_use)
    
    # Add data group
    train_data$group = "Train"
    test_data$group = "Test"
    
    # Combine all predictions
    gbm_all_predictions <- bind_rows(train_data, test_data)
    
    # Variable importance info
    gbm_hold_var_imp <- summary(gbm_model_hold, plotit = FALSE)
    gbm_hold_var_imp_df <- as.data.frame(gbm_hold_var_imp)
    names(gbm_hold_var_imp_df) <- c("Variable", "Relative_Influence")
    rownames(gbm_hold_var_imp_df) <- NULL
    
    # Store results
    results <- list(
      iteration = iteration,
      predictions = gbm_all_predictions,
      gbm_model_holdout = gbm_model_hold,
      gbm_var_imp_holdout = gbm_hold_var_imp_df
    )
    
    return(results)
  }

# Read model input using pattern
model_input_ini <- read_fst(paste0(source.test, "_ML_Daily_only_PMF_sites_", included_years, ".fst"))

# Print info
print(paste0("Processing iteration: ", iteration))
print(paste0("Study period: ", included_years))
print(paste0("Modeled source: ", source.test))
print(paste0("Total site number: ", length(unique(model_input_ini$SiteCode))))

# Prepare input data
columns_to_remove <- c("Longitude", "Latitude", "Dataset",
                       "Source_aftermanual", "SiteCode")
model_holdout_input <- model_input_ini %>%
  dplyr::select(-any_of(columns_to_remove))

# Set seed based on iteration for reproducibility
set.seed(123 + iteration)

# Run single iteration with the selected n_trees and interaction_depth settings
iteration_result <-
  gbm_holdout_predictions_noUnc(model_holdout_input, iteration,
                                n_trees_best, interaction_depth_best)

# Output file directory
out_path = paste0("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/",
                  source.test, "/GBM_", included_years, "/")
# Create the path if it does not exist
if(!dir.exists(out_path)) {
  dir.create(out_path, recursive = TRUE)
  cat("Created directory:", out_path, "\n")
}

## Save results
# Save predictions
write_fst(iteration_result$predictions,
          paste0(out_path, "GBM_predictions_AllYear_", source.test, "_",
                 included_years, "_iteration_", iteration, ".fst"))

# Save model
saveRDS(iteration_result$gbm_model_holdout,
        paste0(out_path, "GBM_model_AllYear_", source.test, "_",
               included_years, "_iteration_", iteration, ".rds"))

# Save variable importance
write_fst(as.data.frame(iteration_result$gbm_var_imp_holdout),
          paste0(out_path, "GBM_var_imp_AllYear_", source.test, "_",
                 included_years, "_iteration_", iteration, ".fst"))


