# aY_ML_HD_001_daily_predict.R

library(dplyr)
library(fst)
library(caret)
library(randomForest)

# Set working directory
setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/ML_Final_Input_001/")
base_dir <- getwd()

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
task_id <- as.integer(args[1]) # Array task ID for the day, make sure it is numeric/integer
source.test <- args[2] # source.test="Biomass"

# task_id = 109; source.test = "bbsoa"                  

# Constants
n_iterations <- 50
days_per_task <- 10
model_year_tag <- "2011-20"          # tag used in model file names

# ---- Locate daily prediction input files (one file per day, all years) ----
pred_input_dir <- file.path(base_dir, "Pred_Inputs")

# Grab every daily file for this source, across all years present.
file_pattern <- paste0("^ML_Predict_001_", source.test,
                       "_[0-9]{8}\\.fst$")
daily_files <- list.files(path = pred_input_dir,
                          pattern = file_pattern,
                          full.names = TRUE)

if (length(daily_files) == 0) {
  stop("No daily input files found in ", pred_input_dir,
       " matching pattern ", file_pattern)
}

# Pull YYYYMMDD out of each file name and sort chronologically
file_date_str <- sub(paste0("^ML_Predict_001_", source.test,
                            "_([0-9]{8})\\.fst$"),
                     "\\1", basename(daily_files))
all_dates <- as.Date(file_date_str, format = "%Y%m%d")

ord         <- order(all_dates)
daily_files <- daily_files[ord]
all_dates   <- all_dates[ord]

actual_days <- length(daily_files)
day_blocks  <- ceiling(actual_days / days_per_task)

# ---- Figure out which iteration and which day block this task handles ----
iteration <- ((task_id - 1) %%  n_iterations) + 1
day_block <- ((task_id - 1) %/% n_iterations) + 1

if (iteration > n_iterations) {
  message(paste("Task", task_id, "exceeds needed iterations - exiting"))
  quit(save = "no")
}
if (day_block > day_blocks) {
  message(paste("Task", task_id, "exceeds available day blocks (",
                day_blocks, ") - exiting"))
  quit(save = "no")
}
# iteration; day_block

start_day_index <- (day_block - 1) * days_per_task + 1
end_day_index   <- min(day_block * days_per_task, actual_days)

cat("Task ID:",              task_id,     "\n")
cat("Model iteration:",      iteration,   "\n")
cat("Source:",               source.test, "\n")
cat("Total days available:", actual_days, "\n")
cat("Date range in files:",  as.character(min(all_dates)),
    "to", as.character(max(all_dates)), "\n")
cat("Day-index range:", start_day_index, "to", end_day_index, "\n")

# ---- Load the RF model for this iteration --------------------------------
# New layout: ML_Final_Input_001/TrainTest_Results/<source>/RF_model_<source>_2011-20_001_iteration_<i>.rds
model_dir  <- file.path(base_dir, "TrainTest_Results", source.test)
model_file <- file.path(
  model_dir,
  paste0("RF_model_", source.test, "_", model_year_tag,
         "_001_iteration_", iteration, ".rds")
)

if (!file.exists(model_file)) {
  stop("Model file not found: ", model_file)
}
rf_model_holdout <- readRDS(model_file)

# ---- Output directory ----------------------------------------------------
# ML_Final_Input_001/Prediction_results/<source.text>/Iteration_<i>/
pred_output_dir <- 
  file.path(base_dir, "Prediction_results", source.test,
            paste0("Iteration_", iteration))
dir.create(pred_output_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Predict for every day in this block ---------------------------------
for (day_index in start_day_index:end_day_index) {
  
  # day_index = start_day_index
  target_date <- all_dates[day_index]
  target_file <- daily_files[day_index]
  cat("Processing date:", as.character(target_date),
      "(day", day_index, ") -- file:", basename(target_file), "\n")
  
  # Read only this day's file
  model_daily_input <- read_fst(target_file)
  
  # Drop columns that not used for modeling, if present
  cols_to_remove <- c("cell_id", "Date", "PMF_conc", "Longitude", "Latitude")
  cols_to_remove <- cols_to_remove[cols_to_remove %in% names(model_daily_input)]
  prediction_input <- 
    model_daily_input %>%
    dplyr::select(-all_of(cols_to_remove))
  
  # Get predictions
  daily_predictions <- predict(rf_model_holdout, prediction_input)
  holdout_daily_pred <- data.frame(Predictions = daily_predictions)
  
  # Output file
  out_path <- file.path(
    pred_output_dir,
    paste0(source.test, "_RF_HD_Daily_pred_001_", target_date, ".fst")
  )
  write_fst(holdout_daily_pred, out_path)
  
  # # Clear memory
  # rm(model_daily_input, prediction_input); gc()
}
