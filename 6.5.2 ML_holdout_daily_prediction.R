library(dplyr)
library(fst)
library(caret)

# Set working directory
setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
base_dir <- getwd()

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
task_id <- as.integer(args[1]) # Array task ID for the day, make sure it is numeric/integer
source.test <- args[2] 
cmaq_period <- args[3] 

# Find and read the input file for predictions
file_pattern <- paste0(source.test, "_ML_input_mainlandUS_.*_days_", cmaq_period, "\\.fst$")
file_path <- list.files(path = ".", pattern = file_pattern, full.names = TRUE)[1]
model_input_all_grid <- read_fst(file_path)

# Get actual number of days
actual_days <- length(unique(model_input_all_grid$Date))
n_iterations <- 50

# Calculate day_index and iteration from task_id
day_index <- ((task_id - 1) %% actual_days) + 1
iteration <- ((task_id - 1) %/% actual_days) + 1

# Check if this task should proceed
if(iteration > n_iterations) {
  message(paste("Task", task_id, "exceeds needed iterations - exiting"))
  quit(save = "no")
}

# Print parameters
print(paste("Task ID:", task_id))
print(paste("Processing day index:", day_index))
print(paste("Model iteration:", iteration))
print(paste("Source:", source.test))
print(paste("Period:", cmaq_period))
print(paste("Total days in period:", actual_days))

# Extract study year
cmaq_year = substring(cmaq_period, 1, 4)

# Load the RF model for this iteration
iteration_model_path <- 
  paste0("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout/",
         source.test, "/", cmaq_year, "/")
rf_model_holdout <- 
  readRDS(paste0(iteration_model_path, 
                 "RF_model_", source.test, "_",
                 cmaq_period, "_iteration_", iteration, ".rds"))

# Get all unique dates and sort them
all_dates <- sort(unique(model_input_all_grid$Date))
target_date <- all_dates[day_index]

print(paste("Processing date:", target_date))

# Filter data for target date
model_daily_input <- 
  model_input_all_grid %>%
  filter(Date == target_date) %>%
  dplyr::select(-Dataset, -Source_aftermanual, -grid_ID, -year) 

# Make predictions
prediction_input <- 
  dplyr::select(model_daily_input, 
                -Date, -PMF_conc, SiteCode, -Longitude, -Latitude)
daily_predictions <- predict(rf_model_holdout, prediction_input)

# Combine predictions with metadata
holdout_daily_pred <- model_daily_input
holdout_daily_pred$Predictions <- daily_predictions
holdout_daily_pred$Iteration <- iteration

# Create output directory structure
pred_output_dir <-
  paste0(iteration_model_path,
         "Iteration_", iteration, "/")
dir.create(pred_output_dir, recursive = TRUE, showWarnings = FALSE)

# Save results
holdout_pred_output <- 
  paste0(pred_output_dir, source.test, "_holdout_pred_", target_date, ".fst")
write_fst(holdout_daily_pred, holdout_pred_output)
