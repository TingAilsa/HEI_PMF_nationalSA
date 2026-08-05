# 6.8.3 AY_GBM_daily_prediction.R

# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran

library(dplyr)
library(fst)
library(caret)
library(gbm)

# Set working directory
setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
base_dir <- getwd()
base_output_path = "/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/"

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
task_id <- as.integer(args[1]) # Array task ID for the day, make sure it is numeric/integer
source.test <- args[2] # source.test="Biomass"
cmaq_year <- args[3] # cmaq_year = 2014
n_trees_best <- as.integer(args[3]) # n_trees_best = 8000

# Constants
n_iterations <- 50
days_per_task <- 10

# Get file name pattern and read the input file for prediction
process_period = paste0(cmaq_year, "-01_", cmaq_year, "-12")
file_name_pattern <-
  paste0(source.test, "_ML_Daily_mainlandUS_", ".*_days_",
         process_period, "\\.fst$")
file_name <- list.files(pattern = file_name_pattern, full.names = TRUE)[1]
model_input_all_grid <- read_fst(file_name)
file_name; names(model_input_all_grid)

# Get actual number of days and day blocks
# Every run, make predictions for 10 days
actual_days <- length(unique(model_input_all_grid$Date))
day_blocks <- ceiling(actual_days / days_per_task)

# Calculate day_blocks and iteration from task_id
iteration <- ((task_id - 1) %% n_iterations) + 1
day_block <- ((task_id - 1) %/% n_iterations) + 1
# iteration; day_block

# Check if this task should proceed
if(iteration > n_iterations) {
  message(paste("Task", task_id, "exceeds needed iterations - exiting"))
  quit(save = "no")
}

if(day_block > day_blocks) {
  message(paste("Task", task_id, "exceeds available day blocks (", day_blocks, ") - exiting"))
  quit(save = "no")
}

# Calculate the range of days for this task
start_day_index <- (day_block - 1) * days_per_task + 1
end_day_index <- min(day_block * days_per_task, actual_days)

# Print parameters
cat("Task ID:", task_id, "\n")
cat("Model iteration:", iteration, "\n")
cat("Source:", source.test, "\n")
cat("Period:", cmaq_year, "\n")
cat("Total days in period:", actual_days, "\n")
cat("Date range:", start_day_index, " to ", end_day_index, "\n")

# Load the GBM model for this iteration
cmaq_year_model = "2011-2020"
iteration_model_path <-
  paste0(base_output_path,
         source.test, "/GBM_", cmaq_year_model, "/")

gbm_model_hd <-
  readRDS(paste0(iteration_model_path,
                 "GBM_model_AllYear_", source.test, "_",
                 cmaq_year_model, "_iteration_", iteration, ".rds"))

# Create folder for the prediction data
iteration_prediction_path <-
  paste0(base_output_path,
         source.test, "/GBM_", cmaq_year_model, "/")
if (!dir.exists(iteration_prediction_path)) {
  dir.create(iteration_prediction_path, recursive = TRUE)
}

# Define columns to extract, for light combination of results from all iterations
use_var_list <- c("Iteration", "Longitude", "Latitude", "Date", "Predictions")

# Get all unique dates and sort them
all_dates <- sort(unique(model_input_all_grid$Date))

# Create output directory structure
pred_output_dir <-
  paste0(iteration_prediction_path,
         "Iteration_", iteration, "/")
dir.create(pred_output_dir, recursive = TRUE, showWarnings = FALSE)

# Make predictions for each day in this block
for(day_index in start_day_index:end_day_index) { # day_index = start_day_index
  
  target_date <- all_dates[day_index]
  cat("Processing date:", as.character(target_date), "(day", day_index, ")\n")
  
  # Define columns to remove - only include ones that exist
  cols_to_remove <- c("Dataset", "Source_aftermanual", "grid_ID")
  cols_to_remove <-
    cols_to_remove[cols_to_remove %in% names(model_input_all_grid)]
  
  # Filter data for target date
  model_daily_input <-
    model_input_all_grid %>%
    filter(Date == target_date) %>%
    dplyr::select(-all_of(cols_to_remove))
  
  # Input for predictions
  cols_to_remove_predInp <- 
    c("Date", "PMF_conc", "SiteCode", "Longitude", "Latitude", "grid_ID")
  cols_to_remove_predInp <-
    cols_to_remove_predInp[cols_to_remove_predInp %in% names(model_daily_input)]
  prediction_input <-
    model_daily_input %>%
    dplyr::select(-all_of(cols_to_remove_predInp))

  # Make predictions
  daily_predictions <- 
    predict(gbm_model_hd, 
            prediction_input,
            n.trees = n_trees_best)
  
  # Combine predictions with metadata
  holdout_daily_pred <- model_daily_input
  holdout_daily_pred$Predictions <- daily_predictions
  holdout_daily_pred$Iteration <- iteration
  
  # Extract available columns
  column_extract <-
    intersect(names(holdout_daily_pred), use_var_list)
  holdout_daily_pred_use <-
    holdout_daily_pred[, column_extract, drop = FALSE]
  
  # Save results
  holdout_pred_fullVar_output <-
    paste0(pred_output_dir, source.test, "_GBM_Daily_pred_fullVar_", target_date, ".fst")
  write_fst(holdout_daily_pred, holdout_pred_fullVar_output)
  
  holdout_pred_output <-
    paste0(pred_output_dir, source.test, "_GBM_Daily_pred_", target_date, ".fst")
  write_fst(holdout_daily_pred_use, holdout_pred_output)
}

  
