# allYear_ML_hold_train_test.R

library(dplyr)
library(fst)
library(stringr)

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ml_daily_pred_holdout")
getwd()

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
source.test <- args[1]
cmaq_period <- args[2]

print(paste("Processing source:", source.test))
print(paste("Period:", cmaq_period))

#### Detect files to combine ####

# Detect predictions for trains and tests
print("Detect predictions for trains and tests from iterations")
train_test_dir = paste0(source.test, "/", cmaq_period)
train_test_pred_pattern = 
  paste0("RF_predictions_AllYear_", source.test, "_", cmaq_period, "_iteration_\\d+\\.fst$")
train_test_pred_files <-
  list.files(train_test_dir,
             pattern = train_test_pred_pattern,
             full.names = TRUE)

print("The first five files from train_test_pred_files")
train_test_pred_files[1:5]
print("Number of train_test_pred_files found:")
length(train_test_pred_files)

# Detect predictor performance from iterations
print("Detect predictor performance from iterations")
train_test_dir = paste0(source.test, "/", cmaq_period)
pred_perform_pattern = 
  paste0("RF_var_imp_AllYear_", source.test, "_", cmaq_period, "_iteration_\\d+\\.fst$")
pred_perform_files <-
  list.files(train_test_dir,
             pattern = pred_perform_pattern,
             full.names = TRUE)

print("The first five files from pred_perform_files")
pred_perform_files[1:5]
print("Number of pred_perform_files found:")
length(pred_perform_files)

#### Combine and save files ####
# Create a list with both data and iteration number, and bind_rows()
combined_train_test <- 
  Map(function(file, iter) {
    read_fst(file) %>%
      mutate(Iteration = iter)
  }, train_test_pred_files, 1:50) %>%
  bind_rows()

combined_pred_perform <- 
  Map(function(file, iter) {
    read_fst(file) %>%
      mutate(Iteration = iter)
  }, pred_perform_files, 1:50) %>%
  bind_rows()

# Directory of annual combined file from all iterations
combine_path <- "Annual_combine/"

# Save combined results
write_fst(combined_train_test,
          paste0(combine_path,
                 "RF_Pred_HD_US_01_train_test_", 
                 source.test, "_", cmaq_period, ".fst"))

write_fst(combined_pred_perform,
          paste0(combine_path,
                 "RF_Pred_HD_US_01_predictor_perform_", 
                 source.test, "_", cmaq_period, ".fst"))

