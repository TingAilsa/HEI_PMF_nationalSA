# predict_single_day.R
library(dplyr)
library(lubridate)
library(fst)
library(parallel)
library(stringr)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/")
getwd()

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
day_index <- args[1] # This will be the SLURM_ARRAY_TASK_ID
source.test <- args[2]
cmaq_period <- args[3]
day_count <- args[4]

# Load the RF model
rf_model_noGrid_cv <- 
  readRDS(
    paste0("machine_learning_source_input/", 
           "RF_No_coords_", source.test, "_", cmaq_period, ".rds"))

# Load data for the specific date and source
model_input_all_grid <- 
  read_fst(
    paste0("machine_learning_source_input/", 
           source.test, "_ML_input_mainlandUS_", day_count, "_days_", cmaq_period, ".fst"))

# Extract study year
cmaq_year = substring(cmaq_period, 1, 4)

# Get the target date using index
target_date <- sort(unique(model_input_all_grid$Date))[day_index]

# # Remove longitude and latitude
# model_input_all_grid$Longitude = model_input_all_grid$Latitude = NULL

# Filter for specific date
model_all_grid_daily <- 
  model_input_all_grid %>%
  filter(Date == target_date) %>%
  dplyr::select(-Dataset, -Source_aftermanual, -year)

# Prediction input, column selection
model_all_grid_daily_input <-
  dplyr::select(model_all_grid_daily, 
                -Date, -PMF_conc, -grid_ID, -Longitude, -Latitude, -SiteCode)
print("Check the daily input for prediction, summary and dim")
summary(model_all_grid_daily_input)
dim(model_all_grid_daily_input)

# Make prediction
daily_rf_predictions <- predict(rf_model_noGrid_cv, model_all_grid_daily_input)

# Generate output data
model_all_grid_daily_predictions = model_all_grid_daily
model_all_grid_daily_predictions$Predictions = daily_rf_predictions

print("Check the daily prediction, summary and dim")
summary(model_all_grid_daily_predictions)
dim(model_all_grid_daily_predictions)

print("Correlation between PMF_conc and Predictions")
cor(model_all_grid_daily_predictions$PMF_conc, 
    model_all_grid_daily_predictions$Predictions, 
    use = "complete.obs")

# Save results
output_file <- 
  paste0("ml_daily_prediction/", 
         source.test, "/", cmaq_year,
         "/predictions_", target_date, ".fst")
write_fst(model_all_grid_daily_predictions, output_file)
