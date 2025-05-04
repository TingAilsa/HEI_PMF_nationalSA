# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran

library(base)
library(stringr)
library(dplyr)
library(tidyr)
library(data.table)
library(fst)
library(caret)
library(randomForest)
library(doParallel)

# Setup parallel processing
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
iteration <- as.integer(args[1])  # Array task ID will be the iteration number, ensure integer format
source.test <- args[2]
cmaq_period <- args[3]

# iteration = 35; source.test = "Traffic"; cmaq_period = "2013-01_2013-12"

# Extract study year
cmaq_year = substring(cmaq_period, 1, 4)

# Set working directory
setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
base_dir <- getwd()

# Function definition remains the same
#### Functions to use ####
rf_holdout_predictions_noUnc <- 
  function(model_withPMF_data, iteration) { # prediction_noPMF_data, 
    
    # Randomly select 80% of sites for training
    unique_sites <- unique(model_withPMF_data$grid_ID)
    train_sites <- sample(unique_sites, size = floor(0.8 * length(unique_sites)))
    
    # Create training and test datasets
    train_data <- model_withPMF_data %>% filter(grid_ID %in% train_sites)
    test_data <- model_withPMF_data %>% filter(!grid_ID %in% train_sites)
    
    # Remove columns not to use
    train_data_use <- dplyr::select(train_data, -Date, -grid_ID)
    test_data_use <- dplyr::select(test_data, -Date, -grid_ID)
    # summary(train_data_use); dim(train_data_use)
    
    # Fit the Random Forest model on training data, without grid_ID
    # Disable resampling for the holdout analyses
    train_control_holdout <- trainControl(method = "none")
    
    rf_model_hold <- caret::train(
      PMF_conc ~ ., 
      data = train_data_use, 
      method = "rf",
      trControl = train_control_holdout,  
      importance = TRUE,
      ntree = 500
    )
    
    # Get predictions
    train_data$Predictions <- predict(rf_model_hold, train_data_use)
    test_data$Predictions <- predict(rf_model_hold, test_data_use)
    
    # Add data group
    train_data$group = "Train"
    test_data$group = "Test"

    # Combine all predictions
    rf_all_predictions <- bind_rows(train_data, test_data) 
    # head(rf_all_predictions)
    rf_all_predictions$Iteration = iteration
    
    # Variable importance info
    rf_hold_var_imp <- varImp(rf_model_hold, scale = TRUE)
    rf_hold_var_imp_df <- rf_hold_var_imp$importance
    rf_hold_var_imp_df$Variable <- rownames(rf_hold_var_imp_df)
    rownames(rf_hold_var_imp_df) <- NULL
    names(rf_hold_var_imp_df) <- c("Relative_Influence", "Variable")
    
    # Store results
    results <- list(
      iteration = iteration,
      predictions = rf_all_predictions,
      rf_model_holdout = rf_model_hold,
      rf_var_imp_holdout = rf_hold_var_imp_df
    )
    
    return(results)
  }

# Read model input using pattern
file_pmf_pattern <- paste0(source.test, "_ML_input_only_PMF_sites_.*_days_", cmaq_period, "\\.fst$")
file_path <- list.files(path = ".", pattern = file_pmf_pattern, full.names = TRUE)[1]
model_input_ini <- read_fst(file_path)
# summary(model_input_ini)

# Print info
print(paste0("Processing iteration: ", iteration))
print(paste0("Study period: ", cmaq_period))
print(paste0("Study year: ", cmaq_year))
print(paste0("Modeled source: ", source.test))
print(paste0("Total site number: ", length(unique(model_input_ini$SiteCode))))

# Prepare input data
columns_to_remove <- c("Longitude", "Latitude", "Dataset", "year", 
                       "Source_aftermanual", "SiteCode")
model_holdout_input <- model_input_ini %>% 
  dplyr::select(-all_of(columns_to_remove))

# Set seed based on iteration for reproducibility
set.seed(123 + iteration)

# Run single iteration
iteration_result <- 
  rf_holdout_predictions_noUnc(model_holdout_input, iteration)

# Output file directory
out_path = paste0("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/",
                  source.test, "/", cmaq_year, "/")

## Save results
# Save predictions
write_fst(iteration_result$predictions,
          paste0(out_path, "RF_predictions_", source.test, "_",
                 cmaq_period, "_iteration_", iteration, ".fst"))

# Save model
saveRDS(iteration_result$rf_model_holdout,
        paste0(out_path, "RF_model_", source.test, "_",
               cmaq_period, "_iteration_", iteration, ".rds"))

# Save variable importance
write_fst(as.data.frame(iteration_result$rf_var_imp_holdout),
          paste0(out_path, "RF_var_imp_", source.test, "_",
                 cmaq_period, "_iteration_", iteration, ".fst"))

# Stop cluster
stopCluster(cl)

