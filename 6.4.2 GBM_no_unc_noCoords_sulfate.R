# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran

library(base)
library(stringr)
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
# library(pdp) # partial dependence in gbm
# library(USAboundaries)

# Detect the number of cores available on the server
num_cores <- detectCores() - 1  # Use all but one core
num_cores

# Create a parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/MLresult_gbm/")

setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
getwd()
base_dir = getwd()

#### 0. Extract long & points with the continental US ####
#us_states = USAboundaries::us_states()
#us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

#### 1. Modeling Input ####

cmaq_period = "2011-01_2011-12"
# cmaq_period = "2011-02_2011-12"

# All date related predictors 
date_use = read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Date_DOW_Holiday_2011-20.fst")
us_point_coord = read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Long_lat_Mainland_US_0.1_degree.fst")
head(date_use)

###### 1.1 Sulfate input ######
model_input_ini = read_fst(paste0("Sulfate_only_PMF_points_input_", cmaq_period, ".fst"))
model_input_all_grid = read_fst(paste0("Sulfate_all_CMAQ_points_input_", cmaq_period, ".fst"))
source.test = "Secondary_Sulfate"

paste0(source.test, "_no_Longitude_Latitude_run")

length(unique(model_input_ini$SiteCode))

dim(model_input_ini); dim(model_input_all_grid)
head(model_input_ini); head(model_input_all_grid)

head(data.frame(table(model_input_ini$Longitude)))
head(data.frame(table(model_input_ini$Latitude)))

# Filter points within mainland US
model_input_all_grid_us <- 
  merge(model_input_all_grid, us_point_coord, all.y = TRUE)
dim(model_input_all_grid); dim(model_input_all_grid_us)
length(unique(model_input_all_grid_us$Longitude)); length(unique(model_input_all_grid_us$Latitude))
length(unique(model_input_all_grid$Longitude)); length(unique(model_input_all_grid$Latitude))
summary(model_input_all_grid_us)

model_input_all_grid_us = 
  subset(model_input_all_grid_us, is.na(Concentration))
summary(model_input_all_grid_us)

# Remove the extreme values in 2011.06
model_input_ini =
  subset(model_input_ini, 
         PM25_TOT_EGU <= quantile(model_input_ini$PM25_TOT_EGU, 0.995))
summary(model_input_ini); dim(model_input_ini)

# Remove extremes in data for prediction, and use the sum of PM25_TOT_ONR and PM25_TOT_ONR
dim(model_input_all_grid_us)
model_input_all_grid_us =
  subset(model_input_all_grid_us, 
         PM25_TOT_EGU <= max(model_input_ini$PM25_TOT_EGU))
summary(model_input_all_grid_us); dim(model_input_all_grid_us)

# Get PMF_conc & CMAQ_conc for use
model_input_ini <-
  plyr::rename(model_input_ini,
               c("Concentration" = "PMF_conc",
                 "PM25_TOT_EGU" = "CMAQ_conc"))

model_input_all_grid_us <-
  plyr::rename(model_input_all_grid_us,
               c("Concentration" = "PMF_conc",
                 "PM25_TOT_EGU" = "CMAQ_conc"))

# Get grid_ID
model_input_ini$grid_ID = 
  paste0("grid_", model_input_ini$Longitude, "_", model_input_ini$Latitude)
model_input_ini$grid_ID = as.factor(model_input_ini$grid_ID)

model_input_all_grid_us$grid_ID = 
  paste0("grid_", model_input_all_grid_us$Longitude, "_", model_input_all_grid_us$Latitude)
model_input_all_grid_us$grid_ID = as.factor(model_input_all_grid_us$grid_ID)
head(model_input_all_grid_us$grid_ID); head(model_input_ini$grid_ID)

# Check if there is potential NAs, and remove
summary(model_input_all_grid_us); dim(model_input_all_grid_us)
model_input_all_grid_us = subset(model_input_all_grid_us, !is.na(land_type))
dim(model_input_all_grid_us)

###### 1.N Data for modeling - shared preparations ######

# Data for modeling
model_input_use = model_input_ini

# Extract full grid dataset that having no PMF_conc
model_input_all_grid_us = 
  subset(model_input_all_grid_us, is.na(PMF_conc))
summary(model_input_all_grid_us$PMF_conc)

# Update the year month day of week info
model_input_all_grid_us$year = model_input_all_grid_us$month = 
  model_input_all_grid_us$day_of_week = model_input_all_grid_us$is_holiday = NULL

date_model_use = subset(date_use, Date %in% model_input_all_grid_us$Date)
model_input_all_grid_us =
  merge(model_input_all_grid_us, date_model_use, all.x = TRUE)
summary(model_input_all_grid_us); dim(model_input_all_grid_us)

# Remove extremes in model_input_all_grid_us
model_input_all_grid_us = 
  subset(model_input_all_grid_us, 
         CMAQ_conc <= max(model_input_use$CMAQ_conc))
dim(model_input_all_grid_us)

# Remove column not to use & potential NAs
model_input_use$SiteCode = model_input_all_grid_us$SiteCode = NULL
model_input_use = na.omit(model_input_use)  
model_input_all_grid_us = # still some NAs (1%) in meteorology data
  subset(model_input_all_grid_us, !is.na(th))

summary(model_input_use); dim(model_input_use)
summary(model_input_all_grid_us); dim(model_input_all_grid_us)

# Get column order from model_input_use
col_order <- names(model_input_use)

# Reorder model_input_all_grid_us columns
model_input_all_grid_us <- 
  model_input_all_grid_us %>%
  dplyr::select(all_of(col_order))
all(names(model_input_use) == names(model_input_all_grid_us))

# Get the number of unique site input
site_input_count = length(unique(model_input_use$grid_ID))
site_input_count

dim(model_input_use); dim(model_input_all_grid_us)

model_input_use$long = model_input_use$lat = 
  model_input_all_grid_us$long = model_input_all_grid_us$lat = NULL
summary(model_input_use); summary(model_input_all_grid_us)

#### 2. Gradient Boosting: Predict with Uncertainty #### 

model.method = "Gradient_Boosting"
unique_sites <- unique(model_input_use$grid_ID)

#### Use dataset WITHOUT Longitude or Latitude!!!!!
model_input_use$Longitude = model_input_use$Latitude = 
  model_input_all_grid_us$Longitude = model_input_all_grid_us$Latitude = NULL


###### 2.1 Gradient Boosting with uncertainty, all data, cross validation ###### 
# The tuneGrid parameter in caret's train function, specific combinations of hyperparameters to try during model training. 
# For GBM (Gradient Boosting Machine), these parameters are:
# n.trees: Number of trees/iterations in the boosting process
# interaction.depth: Maximum depth of each tree
# shrinkage: Learning rate (how much each tree contributes)
# n.minobsinnode: Minimum number of observations in terminal nodes

# Get data subsets to check the code
# model_input_use_full = model_input_use
# model_input_all_grid_us_full = model_input_all_grid_us
# dim(model_input_use_full); dim(model_input_all_grid_us_full)
# 
# model_input_use = model_input_use_full
# model_input_all_grid_us = model_input_all_grid_us_full
# 
# unique_nopmf_sites <- unique(model_input_all_grid_us$grid_ID)
# train_nopmf_sites <- sample(unique_nopmf_sites, size = floor(0.05 * length(unique_nopmf_sites)))
# model_input_all_grid_us <- model_input_all_grid_us %>% filter(grid_ID %in% train_nopmf_sites)
# model_input_all_grid_us = subset(model_input_all_grid_us, month %in% c(1, 4, 7))
# summary(model_input_all_grid_us); dim(model_input_all_grid_us)
# 
# unique_sites <- unique(model_input_use$grid_ID)
# train_sites <- sample(unique_sites, size = floor(0.05 * length(unique_sites)))
# model_input_use <- model_input_use %>% filter(grid_ID %in% train_sites)
# model_input_use = subset(model_input_use, month %in% c(1, 4, 7))
# summary(model_input_use); dim(model_input_use)

# Set up 5-fold cross-validation, define hyperparameter grid for tuning
gbm_grid <- expand.grid(
  n.trees = 500,
  interaction.depth = c(3, 5, 7),
  shrinkage = c(0.01, 0.1),
  n.minobsinnode = c(10, 20)
)

# Define training control
train_control <- trainControl(
  method = "cv",          # Use cross-validation
  number = 5,            # Number of folds
  verboseIter = FALSE,   # Don't print iteration output
  allowParallel = TRUE   # Allow parallel processing
)

# Fit the Gradient Boosting model with and without grid_ID
gbm_model_cv <- caret::train(
  PMF_conc ~ ., 
  data = dplyr::select(model_input_use, 
                       -Dataset, -Date, -Source_aftermanual, -year), 
  method = "gbm",
  trControl = train_control,
  tuneGrid = gbm_grid,   # Add tuning grid
  verbose = FALSE,
  metric = "RMSE"  # Optimize for RMSE
)

gbm_model_noGrid_cv <- caret::train(
  PMF_conc ~ ., 
  data = dplyr::select(model_input_use, 
                       -Dataset, -Date, -Source_aftermanual, -year, -grid_ID), 
  method = "gbm",
  trControl = train_control,
  tuneGrid = gbm_grid,   # Add tuning grid
  verbose = FALSE,
  metric = "RMSE"  # Optimize for RMSE
)

### Get distribution of grid effects for simulation

# Get base predictions and residuals
model_input_use$gbm_pred <- predict(gbm_model_cv, model_input_use)
model_input_use$residuals <- model_input_use$PMF_conc - model_input_use$gbm_pred

# Analyze residuals by grid
gbm_grid_residual <- 
  model_input_use %>%
  group_by(grid_ID) %>%
  dplyr::summarize(
    mean_residual = mean(residuals, na.rm = TRUE),
    sd_residual = sd(residuals, na.rm = TRUE),
    n_obs = n()
  ) %>%
  ungroup()

# Grid-level variation, random effects specific to each location
gbm_grid_residual_effect_mean_sd <- 
  sd(gbm_grid_residual$mean_residual, na.rm = TRUE)

# Within-grid variation: remaining unexplained variation
gbm_within_grid_residual_sd_mean <- 
  mean(gbm_grid_residual$sd_residual, na.rm = TRUE)

gbm_grid_residual_effect_mean_sd; gbm_within_grid_residual_sd_mean

# Function to make predictions with uncertainty
gbm_cv_predictions_with_uncertainty <- 
  function(new_data, n_sims = 1000) {
    # Get base gbm predictions
    gbm_predictions <- predict(gbm_model_cv, new_data)
    
    # For each prediction, simulate possible grid effects
    gbm_simul_predictions <- matrix(nrow = nrow(new_data), ncol = n_sims)
    
    for(i in 1:n_sims) {
      # Simulate grid effects - location-specific random effects
      grid_effects <- rnorm(nrow(new_data), 0, gbm_grid_residual_effect_mean_sd)
      
      # Simulate residual variation - remaining uncertainty
      res_effects <- rnorm(nrow(new_data), 0, gbm_within_grid_residual_sd_mean)
      
      # Final prediction for this simulation
      gbm_simul_predictions[,i] <- gbm_predictions + grid_effects + res_effects
    }
    
    # Calculate prediction intervals
    pred_summary <- t(apply(gbm_simul_predictions, 1, function(x) {
      c(mean = mean(x),
        median = median(x),
        sd = sd(x),
        q025 = quantile(x, 0.025),
        q975 = quantile(x, 0.975))
    }))
    
    # Combine with original data
    predictions <- cbind(new_data, 
                         as.data.frame(pred_summary))
    
    return(predictions)
  }

# Function to make predictions for new locations (without grid_ID) in parallel chunks
gbm_cv_predictions_parallel <- 
  function(full_pred_data, chunk_size) {
    
    n_sims <- 1000 # number of simulated possible values
    
    # Get unique grid_IDs
    unique_grids <- unique(full_pred_data$grid_ID)
    
    # Split grid_IDs into chunks
    n_chunks <- ceiling(length(unique_grids) / chunk_size)
    grid_chunks <- split(unique_grids, 
                         cut(seq_along(unique_grids), n_chunks, labels = FALSE))
    n_chunks
    
    counter <- 0
    
    # Define the chunk processing function
    process_chunk <- function(chunk_grids) {
      # Add diagnostic prints
      counter <<- counter + 1
      print(paste("Processing chunk", counter))
      paste("Number of grids in chunk:", length(chunk_grids))
      
      # Select data for current chunk of grid_IDs
      chunk_data <- 
        subset(full_pred_data, grid_ID %in% unlist(chunk_grids))
      dim(chunk_data)
      
      # Prepare data for prediction
      chunk_data_use <- 
        dplyr::select(chunk_data, -PMF_conc, -Date, -grid_ID)
      
      # Get base gbm predictions
      names(gbm_model_noGrid_cv$trainingData); names(chunk_data_use)
      gbm_predictions <- predict(gbm_model_noGrid_cv, chunk_data_use)
      
      # Return results as data frame
      data.frame(
        Date = chunk_data$Date,
        grid_ID = chunk_data$grid_ID,
        PMF_conc = NA,
        gbm_predictions = gbm_predictions
      )
    }
    
    # Process chunks in parallel, add error handling for mclapply in case it cannot be used
    tryCatch({
      n_cores <- detectCores() - 1
      chunk_predictions <- mclapply(
        grid_chunks,
        process_chunk,
        mc.cores = n_cores)
        
    }, error = function(e) {
      message("Error in parallel processing: ", e$message)
      # Fall back to sequential processing
      chunk_predictions <- lapply(grid_chunks, process_chunk)
    })
    
    # Combine all chunks
    all_predictions <- do.call(rbind, chunk_predictions)
    
    # Ensure predictions are ordered by Date and grid_ID
    all_predictions <- all_predictions %>%
      arrange(Date, grid_ID)
    
    return(all_predictions)
  }

# Generate predictions, using data with andn without Longitude or Latitude
gbm_with_pmf_predictions <- 
  gbm_cv_predictions_with_uncertainty(
    dplyr::select(model_input_use, -Dataset, -Date, -Source_aftermanual, -year))
    # dplyr::select(model_input_use, -Longitude, -Latitude, -Dataset, -Date, -Source_aftermanual, -year))

# ATTENTION!! Date is included in the input!!!
gbm_no_pmf_predictions <- 
  gbm_cv_predictions_parallel(
    dplyr::select(model_input_all_grid_us, -Dataset, -Source_aftermanual, -year), 200)
    # dplyr::select(model_input_all_grid_us, -Longitude, -Latitude, -Dataset, -Date, -Source_aftermanual, -year, -grid_ID), 100)
summary(gbm_no_pmf_predictions); dim(gbm_no_pmf_predictions)

cor(gbm_with_pmf_predictions$PMF_conc, gbm_with_pmf_predictions$mean)

# Add Date and rename columns
gbm_with_pmf_predictions$Date = model_input_use$Date
gbm_with_pmf_predictions = 
  plyr::rename(gbm_with_pmf_predictions, 
               c("mean" = "gbm_predictions"))

# Merge two predictions
gbm_with_pmf_predictions_sharedCol =
  dplyr::select(gbm_with_pmf_predictions, names(gbm_no_pmf_predictions))

gbm_all_predictions = 
  bind_rows(gbm_with_pmf_predictions_sharedCol, gbm_no_pmf_predictions)

# Get month, long and lat
gbm_all_predictions$Month = month(gbm_all_predictions$Date)

# grid_id_example = gbm_all_predictions$grid_ID[1]
# as.numeric(str_extract(grid_id_example, "-?\\d+\\.\\d+"))
# as.numeric(str_extract(grid_id_example, "\\d+\\.\\d+$"))
gbm_all_predictions_coords =
  gbm_all_predictions %>%
  dplyr::mutate(
    Longitude = as.numeric(str_extract(grid_ID, "-?\\d+\\.\\d+")),
    Latitude = as.numeric(str_extract(grid_ID, "\\d+\\.\\d+$"))
  )

head(gbm_all_predictions_coords); dim(gbm_all_predictions_coords)

# Output data
write_fst(gbm_all_predictions_coords,
          paste0("GBM_overall_No_coords_", source.test, "_", cmaq_period, ".fst"))
write_fst(gbm_with_pmf_predictions,
          paste0("GBM_SiteID_Uncertainty_No_coords_", source.test, "_", cmaq_period, ".fst"))

