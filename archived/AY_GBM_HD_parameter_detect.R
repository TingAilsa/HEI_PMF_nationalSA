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
source.test <- args[1]
included_years <- args[2]
n_trees_use <- as.integer(args[3])
interaction_depth_use <- as.integer(args[4])
shrinkage_use <- as.numeric(args[5])

# source.test="PM25"; included_years="2011-2020"; n_trees_use=500; interaction_depth_use=3

# Set working directory
setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
base_dir <- getwd()

#### Prepare the input data ####
cat("========================================\n")
cat("Processing hyperparameter combination:\n")
cat("Source:", source.test, "\n")
cat("Study period:", included_years, "\n")
cat("n.trees:", n_trees_use, "\n")
cat("interaction.depth:", interaction_depth_use, "\n")

# Read model input
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
tune_dates_systematic <- unique_dates[seq(1, length(unique_dates), by = 4)]
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

#### Run the GBM model ####
gbm_temp <- gbm(
  PMF_conc ~ .,
  data = tune_train,
  distribution = "gaussian",
  n.trees = n_trees_use,
  interaction.depth = interaction_depth_use,
  shrinkage = shrinkage_use, 
  verbose = FALSE
)

#### Evaluate on test set ####
test_pred <- predict(gbm_temp, newdata = tune_test, n.trees = n_trees_use)
test_rmse <- sqrt(mean((tune_test$PMF_conc - test_pred)^2, na.rm = TRUE))
test_mae  <- mean(abs(tune_test$PMF_conc - test_pred), na.rm = TRUE)
test_r2   <- cor(tune_test$PMF_conc, test_pred, use = "complete.obs")^2

cat("Test RMSE:", test_rmse, "\n")
cat("Test MAE:", test_mae, "\n")
cat("Test R2:", test_r2, "\n")

#### Save the results ####
hyperparameter_results <-
  data.frame(
    source = source.test,
    years = included_years,
    n_trees = n_trees_use,
    interaction_depth = interaction_depth_use,
    shrinkage = shrinkage_use, 
    test_rmse = test_rmse,
    test_mae = test_mae,
    test_r2 = test_r2,
    train_n = nrow(tune_train),
    test_n = nrow(tune_test)
  )

# Save output file
hyperparameter_file <-
  paste0("tuning_results/",
         source.test, "_", included_years,
         "_ntrees_", n_trees_use,
         "_depth_", interaction_depth_use, "_results.csv")

# Create output directory if it doesn't exist
dir.create("tuning_results", showWarnings = FALSE, recursive = TRUE)

# Save results
write.csv(hyperparameter_results,
          hyperparameter_file,
          row.names = FALSE)