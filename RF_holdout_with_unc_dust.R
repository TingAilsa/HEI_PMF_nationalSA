# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran

library(base)
library(dplyr)
library(tidyr)
library(data.table)
library(raster) 
library(ncdf4) 
library(sf)
library(lubridate)
library(timeDate) #holidayNYSE{}
library(fst)
library(terra)
library(caret) # trainControl
library(randomForest)
library(ggplot2)
library(patchwork)
library(randomForest)
library(doParallel) # parallel backend
# library(pdp) # partial dependence in RF

# Detect the number of cores available on the server
num_cores <- detectCores() - 1  # Use all but one core
num_cores

# Create a parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/MLresult_RF/")

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
getwd()
base_dir = getwd()


#### 1. Modeling Input ####

cmaq_period = "2011-01_2011-12"

# All date related predictors
date_use = 
  read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Date_DOW_Holiday_2011-20.fst")
head(date_use)

us_point_coord = 
  read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Long_lat_Mainland_US_0.1_degree.fst")

###### 1.2 Dust input ######
model_input_ini = read_fst(paste0("Dust_only_PMF_points_input_", cmaq_period, ".fst"))
model_input_all_grid = read_fst(paste0("Dust_all_CMAQ_points_input_", cmaq_period, ".fst"))
source.test = "Dust"
length(unique(model_input_ini$SiteCode))

dim(model_input_ini); dim(model_input_all_grid)

# Remove the extreme values in 2011.06
model_input_ini =
  subset(model_input_ini,
         PM25_TOT_DUST <= quantile(model_input_ini$PM25_TOT_DUST, 0.995))
summary(model_input_ini); dim(model_input_ini)

# Remove extremes in data for prediction, and use the sum of PM25_TOT_ONR and PM25_TOT_ONR
dim(model_input_all_grid)
model_input_all_grid =
  subset(model_input_all_grid,
         PM25_TOT_DUST <= max(model_input_ini$PM25_TOT_DUST))
summary(model_input_all_grid); dim(model_input_all_grid)

# Get PMF_conc & CMAQ_conc for use
model_input_ini <-
  plyr::rename(model_input_ini,
               c("Concentration" = "PMF_conc",
                 "PM25_TOT_DUST" = "CMAQ_conc"))

model_input_all_grid <-
  plyr::rename(model_input_all_grid,
               c("Concentration" = "PMF_conc",
                 "PM25_TOT_DUST" = "CMAQ_conc"))

# Get grid_ID
model_input_ini$grid_ID =
  paste0("grid_", model_input_ini$Longitude, "_", model_input_ini$Latitude)
model_input_ini$grid_ID = as.factor(model_input_ini$grid_ID)

model_input_all_grid$grid_ID =
  paste0("grid_", model_input_all_grid$Longitude, "_", model_input_all_grid$Latitude)
model_input_all_grid$grid_ID = as.factor(model_input_all_grid$grid_ID)
head(model_input_all_grid$grid_ID); head(model_input_ini$grid_ID)

# Remove original PM25_TOT_ variables
model_input_ini$PM25_TOT_NRD = model_input_ini$PM25_TOT_ONR = NULL
model_input_all_grid$PM25_TOT_NRD = model_input_all_grid$PM25_TOT_ONR = NULL

# Get grid_ID
model_input_ini$grid_ID = 
  paste0("grid_", model_input_ini$Longitude, "_", model_input_ini$Latitude)
model_input_ini$grid_ID = as.factor(model_input_ini$grid_ID)

model_input_all_grid$grid_ID = 
  paste0("grid_", model_input_all_grid$Longitude, "_", model_input_all_grid$Latitude)
model_input_all_grid$grid_ID = as.factor(model_input_all_grid$grid_ID)
head(model_input_all_grid$grid_ID); head(model_input_ini$grid_ID)


###### 1.N Data for modeling - shared preparations ######

# Data for modeling
model_input_use = model_input_ini

# Extract full grid dataset that having no PMF_conc
model_input_grid_pred = 
  subset(model_input_all_grid, is.na(PMF_conc))
summary(model_input_grid_pred$PMF_conc)

# Update the year month day of week info
model_input_grid_pred$year = model_input_grid_pred$month = 
  model_input_grid_pred$day_of_week = model_input_grid_pred$is_holiday = NULL

date_model_use = subset(date_use, Date %in% model_input_grid_pred$Date)
model_input_grid_pred =
  merge(model_input_grid_pred, date_model_use, all.x = TRUE)
summary(model_input_grid_pred); dim(model_input_grid_pred)

# Remove extremes in model_input_grid_pred
model_input_grid_pred = 
  subset(model_input_grid_pred, 
         CMAQ_conc <= max(model_input_use$CMAQ_conc))

# Remove column not to use & potential NAs
model_input_use$SiteCode = model_input_grid_pred$SiteCode = NULL
model_input_use = na.omit(model_input_use)  
model_input_grid_pred = # still some NAs (1%) in meteorology data
  subset(model_input_grid_pred, !is.na(th))

summary(model_input_use); dim(model_input_use)
summary(model_input_grid_pred); dim(model_input_grid_pred)

# Get column order from model_input_use
col_order <- names(model_input_use)

# Reorder model_input_grid_pred columns
model_input_grid_pred <- 
  model_input_grid_pred %>%
  dplyr::select(all_of(col_order))
all(names(model_input_use) == names(model_input_grid_pred))

# Get the number of unique site input
site_input_count = length(unique(model_input_use$grid_ID))
site_input_count

model_input_grid_pred <- 
  merge(model_input_grid_pred, us_point_coord, all.y = TRUE)
dim(model_input_grid_pred); dim(model_input_grid_pred)
length(unique(model_input_grid_pred$Longitude)); length(unique(model_input_grid_pred$Latitude))
length(unique(model_input_grid_pred$Longitude)); length(unique(model_input_grid_pred$Latitude))
length(unique(model_input_grid_pred$Date)); length(unique(model_input_grid_pred$Date))

model_input_grid_pred =
  subset(model_input_grid_pred, !is.na(grid_ID))
summary(model_input_grid_pred)

model_input_grid_pred$long = model_input_grid_pred$lat =
  model_input_use$long = model_input_use$lat = NULL

# b = table(model_input_grid_pred_us$Longitude, model_input_grid_pred_us$Latitude, model_input_grid_pred_us$Date)
# a = table(model_input_grid_pred$Longitude, model_input_grid_pred$Latitude, model_input_grid_pred$Date)
# length(a); length(b)

#### 2. Random Forest: Predict with Uncertainty #### 

model.method = "Random Forest"

###### 2 Random Forest with uncertainty - Holdout ######

# Function to calculate performance metrics
modeling_perform_metrics <- function(observed, predicted) {
  # Correlation coefficient
  r <- cor(observed, predicted)
  
  # Coefficient of determination (R squared)
  SS_tot <- sum((observed - mean(observed))^2)
  SS_res <- sum((observed - predicted)^2)
  R2 <- 1 - (SS_res/SS_tot)
  
  # Error metrics
  RMSE <- sqrt(mean((observed - predicted)^2))
  MAE <- mean(abs(observed - predicted))
  MB <- mean(predicted - observed)
  NMB <- mean(predicted - observed) / mean(observed) * 100
  
  return(list(
    r = r,
    R2 = R2,
    RMSE = RMSE,
    MAE = MAE,
    MB = MB,
    NMB = NMB
  ))
}

# Function for known locations (with grid_ID)
make_predictions_with_uncertainty_knownID <- 
  function(new_data, rf_model_with_grid, grid_effect_sd, within_grid_sd, n_sims = 1000) {
    # Get base RF predictions
    rf_predictions <- predict(rf_model_with_grid, new_data)
    
    # For each prediction, simulate possible grid effects
    rf_simul_predictions <- matrix(nrow = nrow(new_data), ncol = n_sims)
    
    for(i in 1:n_sims) {
      # Simulate grid effects - location-specific random effects
      grid_effects <- rnorm(nrow(new_data), 0, grid_effect_sd)
      # Simulate residual variation - remaining uncertainty
      total_effect <- grid_effects + rnorm(nrow(new_data), 0, within_grid_sd)
      # Final prediction for this simulation
      rf_simul_predictions[,i] <- rf_predictions + total_effect
    }
    
    # Calculate prediction intervals
    pred_summary <- t(apply(rf_simul_predictions, 1, function(x) {
      c(mean = mean(x),
        median = median(x),
        sd = sd(x),
        q025 = quantile(x, 0.025),
        q975 = quantile(x, 0.975))
    }))
    
    # Combine with original data
    predictions <- cbind(new_data, as.data.frame(pred_summary))
    
    return(predictions)
  }

# Function for new locations (without grid_ID)
# make_predictions_with_uncertainty_newID <- 
#   function(new_data, rf_model_no_grid, grid_effect_sd, within_grid_sd, n_sims = 1000) {
#     # Get base RF predictions using model without grid_ID
#     rf_predictions <- predict(rf_model_no_grid, new_data)
#     
#     # For each prediction, simulate possible grid effects
#     rf_simul_predictions <- matrix(nrow = nrow(new_data), ncol = n_sims)
#     
#     for(i in 1:n_sims) {
#       # Use average uncertainty for new locations
#       grid_effects <- rnorm(nrow(new_data), 0, mean(grid_effect_sd))
#       # Simulate residual variation
#       total_effect <- grid_effects + rnorm(nrow(new_data), 0, within_grid_sd)
#       # Final prediction for this simulation
#       rf_simul_predictions[,i] <- rf_predictions + total_effect
#     }
#     
#     # Calculate prediction intervals
#     pred_summary <- t(apply(rf_simul_predictions, 1, function(x) {
#       c(mean = mean(x),
#         median = median(x),
#         sd = sd(x),
#         q025 = quantile(x, 0.025),
#         q975 = quantile(x, 0.975))
#     }))
#     
#     # Combine with original data
#     predictions <- cbind(new_data, as.data.frame(pred_summary))
#     
#     return(predictions)
#   }


## Function to perform one iteration of RF modeling with uncertainty
# model_withPMF_data = subset(model_input_use_holdout, month <= 3)
# prediction_noPMF_data = subset(model_input_grid_pred_holdout, month <= 3)
# iteration = 3

rf_holdout_predictions_with_uncertainty <- 
  function(model_withPMF_data, iteration) { # prediction_noPMF_data, 
    
    # Randomly select 80% of sites for training
    unique_sites <- unique(model_withPMF_data$grid_ID)
    train_sites <- sample(unique_sites, size = floor(0.8 * length(unique_sites)))
    
    # Create training and test datasets
    train_data <- model_withPMF_data %>% filter(grid_ID %in% train_sites)
    test_data <- model_withPMF_data %>% filter(!grid_ID %in% train_sites)
    
    # Remove columns not to use
    train_data_use <- dplyr::select(train_data, -Date)
    test_data_use <- dplyr::select(test_data, -Date)
    summary(train_data_use); dim(train_data_use)
    
    # Fit the Random Forest model on training data, with and without grid_ID
    # Disable resampling
    train_control <- trainControl(method = "none")
    # train_control <- trainControl(method = "cv", number = 5)
    
    rf_model_hold_cv <- caret::train(
      PMF_conc ~ ., 
      data = train_data_use, 
      method = "rf",
      trControl = train_control,  
      importance = TRUE,
      ntree = 500
    )
    
    # rf_model_hold_cv_no_grid <- caret::train(
    #   PMF_conc ~ ., 
    #   data = dplyr::select(train_data_use, -grid_ID), 
    #   method = "rf",
    #   trControl = train_control,
    #   importance = TRUE,
    #   ntree = 500
    # )
    
    # Get base predictions and residuals
    train_data$rf_pred <- predict(rf_model_hold_cv, train_data_use)
    train_data$residuals <- train_data$PMF_conc - train_data$rf_pred
    
    # Analyze residuals by grid
    rf_grid_residual <- 
      train_data %>%
      group_by(grid_ID) %>%
      summarize(
        mean_residual = mean(residuals, na.rm = TRUE),
        sd_residual = sd(residuals, na.rm = TRUE),
        n_obs = n()
      ) %>%
      ungroup()
    
    # Get distribution of grid effects for simulation
    rf_grid_residual_effect_mean_sd <- sd(rf_grid_residual$mean_residual, na.rm = TRUE)
    rf_within_grid_residual_sd_mean <- mean(rf_grid_residual$sd_residual, na.rm = TRUE)
    
    # Get predictions with uncertainty for dataset with and without new grid_IDs
    predictions_train <- make_predictions_with_uncertainty_knownID(
      train_data_use, 
      rf_model_hold_cv,
      rf_grid_residual_effect_mean_sd,
      rf_within_grid_residual_sd_mean
    )
    
    predictions_test <- make_predictions_with_uncertainty_knownID(
      test_data_use, 
      rf_model_hold_cv,
      rf_grid_residual_effect_mean_sd,
      rf_within_grid_residual_sd_mean
    )
    
    # prediction_noPMF_data_use = dplyr::select(prediction_noPMF_data, -Date, -PMF_conc, -grid_ID)
    # summary(names(prediction_noPMF_data_use) == names(rf_model_hold_cv_no_grid$trainingData)[-1])
    # summary(names(prediction_noPMF_data_use) == names(test_data_use))
    
    # predictions_grid_pred <- make_predictions_with_uncertainty_newID(
    #   prediction_noPMF_data_use,
    #   rf_model_hold_cv_no_grid,
    #   rf_grid_residual_effect_mean_sd,
    #   rf_within_grid_residual_sd_mean
    # )
    
    # Add date, ensure proper row matching
    # predictions_train$Date = train_data$Date
    # predictions_test$Date = test_data$Date
    predictions_train$Date <- 
      train_data$Date[match(predictions_train$grid_ID, train_data$grid_ID)]
    predictions_test$Date <- 
      test_data$Date[match(predictions_test$grid_ID, test_data$grid_ID)]
    
    # predictions_grid_pred$Date = prediction_noPMF_data$Date
    # 
    # # Other columns the no grid_ID dataset would need
    # predictions_grid_pred$grid_ID = prediction_noPMF_data$grid_ID
    # predictions_grid_pred$PMF_conc = NA
    # 
    # # Reorder the columns
    # predictions_grid_pred = 
    #   dplyr:selct(predictions_grid_pred, names(predictions_test))
    
    # Calculate performance metrics
    train_metrics <- 
      modeling_perform_metrics(predictions_train$PMF_conc, predictions_train$mean)
    test_metrics <- 
      modeling_perform_metrics(predictions_test$PMF_conc, predictions_test$mean)
    
    # Prepare detailed predictions dataframe
    train_predictions <- 
      predictions_train %>%
      dplyr::select(grid_ID, Date, PMF_conc, mean, sd, q025, q975) %>%
      mutate(dataset = "train", iteration = iteration)
    
    test_predictions <- 
      predictions_test %>%
      dplyr::select(grid_ID, Date, PMF_conc, mean, sd, q025, q975) %>%
      mutate(dataset = "test", iteration = iteration)
    
    # grid_pred_predictions <- 
    #   predictions_grid_pred %>%
    #   dplyr::select(grid_ID, Date, PMF_conc, mean, sd, q025, q975) %>%
    #   mutate(dataset = "pred", iteration = iteration)
    
    # Combine all predictions
    rf_all_predictions <- bind_rows(train_predictions, test_predictions) # , grid_pred_predictions
    head(rf_all_predictions)
    
    # Store results
    results <- list(
      iteration = iteration,
      predictions = rf_all_predictions,
      performance = list(
        train = train_metrics,
        test = test_metrics
      ),
      # model_noID = rf_model_hold_cv_no_grid,
      model_ID = rf_model_hold_cv
    )
    
    return(results)
  }

# Remove columns not to be used
columns_to_remove <- c("Dataset", "Source_aftermanual", "year", "is_holiday", "long", "lat")

model_input_use_holdout <- 
  model_input_use %>% dplyr::select(-all_of(columns_to_remove))
# model_input_grid_pred_holdout <- 
#   model_input_grid_pred %>% dplyr::select(-all_of(columns_to_remove))

# Perform iterations
set.seed(123)
n_iterations <- 3

# holdout_results <- list()
# for(i in 1:n_iterations) {
#   message(sprintf("Processing iteration %d of %d", i, n_iterations))
#   holdout_results[[i]] <-
#     rf_holdout_predictions_with_uncertainty(
#       model_input_use_holdout, i) # , model_input_grid_pred_holdou
# }

# Initialize progress tracking
pb <- txtProgressBar(min = 0, max = n_iterations, style = 3)

holdout_results <- lapply(1:n_iterations, function(i) {
  message(sprintf("Processing iteration %d of %d", i, n_iterations))
  result <- rf_holdout_predictions_with_uncertainty(
    model_input_use_holdout, i
  )
  # Optional: Write progress to a log file
  cat(sprintf("Completed iteration %d at %s\n", i, Sys.time()), 
      file = "rf_progress.log", 
      append = TRUE)
  return(result)
})
close(pb)


# Combine all predictions and performance metrics
rf_all_predictions <- bind_rows(lapply(holdout_results, function(x) x$predictions))

rf_performance_summary <- data.frame(
  iteration = 1:n_iterations,
  train_r = sapply(holdout_results, function(x) x$performance$train$r), 
  train_RMSE = sapply(holdout_results, function(x) x$performance$train$RMSE),
  train_R2 = sapply(holdout_results, function(x) x$performance$train$R2),
  train_MAE = sapply(holdout_results, function(x) x$performance$train$MAE),
  train_MB = sapply(holdout_results, function(x) x$performance$train$MB),
  train_NMB = sapply(holdout_results, function(x) x$performance$train$NMB),
  
  test_r = sapply(holdout_results, function(x) x$performance$test$r),   
  test_RMSE = sapply(holdout_results, function(x) x$performance$test$RMSE),
  test_R2 = sapply(holdout_results, function(x) x$performance$test$R2),
  test_MAE = sapply(holdout_results, function(x) x$performance$test$MAE),
  test_MB = sapply(holdout_results, function(x) x$performance$test$MB),
  test_NMB = sapply(holdout_results, function(x) x$performance$test$NMB)
)

# Calculate summary statistics for each grid_ID across iterations
rf_grid_summary <- rf_all_predictions %>%
  group_by(grid_ID, Date) %>%
  summarize(
    n_predictions = n(),
    mean_pred = mean(mean),
    sd_pred = sd(mean),
    mean_uncertainty = mean(sd),
    observed = first(PMF_conc),
    pred_lower = quantile(mean, 0.025),
    pred_upper = quantile(mean, 0.975)
  )

# # Calculate summary statistics for grid predictions
# rf_grid_NA_pred_summary <- 
#   rf_all_predictions %>%
#   filter(dataset == "pred") %>%
#   group_by(grid_ID, Date) %>%
#   summarize(
#     n_predictions = n(),
#     mean_pred = mean(mean),
#     sd_pred = sd(mean),
#     mean_uncertainty = mean(sd),
#     pred_lower = quantile(mean, 0.025),
#     pred_upper = quantile(mean, 0.975)
#   )

# Output results
write_fst(rf_all_predictions, 
          paste0("machine_learning_output/RF_uncertainty_holdout_", source.test, "_", 
                 cmaq_period, "_all_predictions.fst"))
write_fst(rf_performance_summary, 
          paste0("machine_learning_output/RF_uncertainty_holdout_", source.test, "_", 
                 cmaq_period, "_performance_summary.fst"))
write_fst(rf_grid_summary, 
          paste0("machine_learning_output/RF_uncertainty_holdout_", source.test, "_", 
                 cmaq_period, "_grid_summary.fst"))
# write_fst(rf_grid_NA_pred_summary,
#           paste0("machine_learning_output/RF_uncertainty_holdout_", source.test, "_",
#                  cmaq_period, "_grid_NA_pred_summary.fst"))
