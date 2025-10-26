# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran
# 6.7.2.0 allYear_ML_HD_parameter_detect.R

library(base)
library(stringr)
library(dplyr)
library(tidyr)
library(data.table)
library(fst)
library(caret)
library(randomForest)

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
source.test <- args[1]
included_years <- args[2]
mtry_use <- as.numeric(args[3])
ntree_use <- as.numeric(args[4])

# source.test="PM25"; included_years="2011-2020"; mtry_use = 3; ntree_use = 500

# Set working directory
setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
base_dir <- getwd()


#### Prepare the input data ####
# Print info
cat("========================================\n")
cat("Processing hyperparameter combination:\n")
cat("Source:", source.test, "\n")
cat("Study period:", included_years, "\n")
cat("mtry:", mtry_use, "\n")
cat("ntree:", ntree_use, "\n")

# Read model input using pattern
model_input_ini <- read_fst(paste0(source.test, "_ML_input_only_PMF_sites_", included_years, ".fst"))

# Prepare input data, remove columns not to use
columns_to_remove <- c("Longitude", "Latitude", "Dataset", 
                       "Source_aftermanual", "SiteCode")
model_holdout_input <- 
  model_input_ini %>%
  dplyr::select(-any_of(columns_to_remove))

#### Get train & test dataset by Date ####
# Get every Nth day
unique_dates <- sort(unique(model_holdout_input$Date))
tune_dates_systematic <- 
  unique_dates[seq(1, length(unique_dates), by = 4)]  
cat("Tuning dates selected:", length(tune_dates_systematic), "\n")

# Get dataset for tuning hyperparameters
tune_train <- 
  model_holdout_input %>% 
  filter(Date %in% tune_dates_systematic) %>%
  select(-Date, -grid_ID)
tune_test <-
  model_holdout_input %>% 
  filter(!Date %in% tune_dates_systematic) %>%
  select(-Date, -grid_ID)

#### Run the model, and validation ####
# Run RF with the selected mtry and ntree combination
rf_temp <- randomForest(
  PMF_conc ~ .,
  data = tune_train,
  mtry = mtry_use,
  ntree = ntree_use,
  importance = TRUE
)

# Evaluate on test set
test_pred <- predict(rf_temp, newdata = tune_test)
test_rmse <- sqrt(mean((tune_test$PMF_conc - test_pred)^2, na.rm = TRUE))
test_mae <- mean(abs(tune_test$PMF_conc - test_pred), na.rm = TRUE)
test_r2 <- cor(tune_test$PMF_conc, test_pred, use = "complete.obs")^2

#### Save the results ####
# Combine the info
hyperparameter_results <- 
  data.frame(
    source = source.test,
    years = included_years,
    mtry = mtry_use,
    ntree = ntree_use,
    test_rmse = test_rmse,
    test_mae = test_mae,
    test_r2 = test_r2,
    train_n = nrow(tune_train),
    test_n = nrow(tune_test)
  )

# Save output file
hyperparameter_file =
  paste0("tuning_results/", 
         source.test, "_", included_years, 
         "_mtry_", mtry_use, "_ntree_", ntree_use, "_results.csv")
cat(hyperparameter_file, "\n")

# Create output directory if it doesn't exist
dir.create("tuning_results", showWarnings = FALSE, recursive = TRUE)

# Save results
write.csv(hyperparameter_results, 
          hyperparameter_file, 
          row.names = FALSE)
