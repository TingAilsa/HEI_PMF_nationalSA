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
# library(pdp) # partial dependence in RF
#library(USAboundaries)

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

#### 0. Extract long & points with the continental US ####
#us_states = USAboundaries::us_states()
#us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

#### 1. Modeling Input ####

#cmaq_period = "2011-01_2011-12"; cmaq_year = 2011
cmaq_period = "2017-01_2017-12"; cmaq_year = 2017
print(paste0("Study period: ", cmaq_period, " & year ", cmaq_year))

# All date related predictors 
#date_use = read.fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Date_DOW_Holiday_2011-20.fst")
#us_point_coord = read.fst("Long_lat_Mainland_US_0.1_degree.fst")
#dim(us_point_coord)

###### 1.1 Sulfate input ######
source.test = "Sulfate"

model_input_ini = read_fst(paste0(source.test, "_ML_input_only_PMF_sites_", cmaq_period, ".fst"))
# model_input_ini = read_fst(paste0(source.test, "_only_PMF_points_input_", cmaq_period, ".fst"))
# model_input_mainland_more = read_fst(paste0("Dust_all_CMAQ_points_input_", cmaq_period, ".fst"))
model_input_all_grid = read_fst(paste0(source.test, "_ML_input_mainlandUS_", cmaq_period, ".fst"))

# dim(model_input_mainland_more); dim(model_input_all_grid)

print(paste0("Study period: ", cmaq_period))
print(paste0("Modeled source: ", source.test))
print(paste0("Input data: ", source.test, "-no_Longitude_Latitude"))

print(paste0("Initial site number: ", length(unique(model_input_ini$SiteCode))))

print("Intial input file check, only PMF")
dim(model_input_ini); head(model_input_ini)
print("Intial input file check, all grids")
dim(model_input_all_grid); head(model_input_all_grid)

# head(data.frame(table(model_input_ini$Longitude)))
# head(data.frame(table(model_input_ini$Latitude)))

#### 2. Random Forest: Predict with Uncertainty #### 

model.method = "Random_Forest"

#### Use dataset WITHOUT Longitude or Latitude!!!!!
model_input_use$Longitude = model_input_use$Latitude = 
  model_input_all_grid_noPMF$Longitude = model_input_all_grid_noPMF$Latitude = NULL

###### 2.1 Random Forest with uncertainty, all data, cross validation ###### 

unique_sites <- unique(model_input_use$grid_ID)

# Set up 5-fold cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Fit the Random Forest model with and without grid_ID
rf_model_cv <- caret::train(
  PMF_conc ~ ., 
  data = dplyr::select(model_input_use, 
                       -Dataset, -Date, -Source_aftermanual, -year), 
  method = "rf",
  trControl = train_control,
  importance = TRUE
)

rf_model_noGrid_cv <- caret::train(
  PMF_conc ~ ., 
  data = dplyr::select(model_input_use, 
                       -Dataset, -Date, -Source_aftermanual, -year, -grid_ID), 
  method = "rf",
  trControl = train_control,
  importance = TRUE
)

# Get base predictions and residuals
model_input_use$rf_pred <- predict(rf_model_cv, model_input_use)
model_input_use$residuals <- model_input_use$PMF_conc - model_input_use$rf_pred

# Analyze residuals by grid
rf_grid_residual <- 
  model_input_use %>%
  group_by(grid_ID) %>%
  dplyr::summarize(
    mean_residual = mean(residuals, na.rm = TRUE),
    sd_residual = sd(residuals, na.rm = TRUE),
    n_obs = n()
  ) %>%
  ungroup()

## Get distribution of grid effects for simulation

# Grid-level variation, random effects specific to each location
rf_grid_residual_effect_mean_sd <- 
  sd(rf_grid_residual$mean_residual, na.rm = TRUE)

# Within-grid variation: remaining unexplained variation
rf_within_grid_residual_sd_mean <- 
  mean(rf_grid_residual$sd_residual, na.rm = TRUE)

rf_grid_residual_effect_mean_sd; rf_within_grid_residual_sd_mean

# Function to make predictions with uncertainty
rf_cv_predictions_with_uncertainty <- 
  function(new_data, n_sims = 1000) {
    # Get base RF predictions
    rf_predictions <- predict(rf_model_cv, new_data)
    
    # For each prediction, simulate possible grid effects
    rf_simul_predictions <- matrix(nrow = nrow(new_data), ncol = n_sims)
    
    for(i in 1:n_sims) {
      # Simulate grid effects - location-specific random effects
      grid_effects <- rnorm(nrow(new_data), 0, rf_grid_residual_effect_mean_sd)
      
      # Simulate residual variation - remaining uncertainty
      res_effects <- rnorm(nrow(new_data), 0, rf_within_grid_residual_sd_mean)
      
      # Final prediction for this simulation
      rf_simul_predictions[,i] <- rf_predictions + grid_effects + res_effects
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
    predictions <- cbind(new_data, 
                         as.data.frame(pred_summary))
    
    return(predictions)
  }

# Function to make predictions for new locations (without grid_ID) in parallel chunks
rf_cv_predictions_parallel <- 
  function(full_pred_data, chunk_size) {
    
    n_sims <- 1000 # number of simulated possible values
    
    # Get unique grid_IDs
    unique_grids <- unique(full_pred_data$grid_ID)
    
    # Split grid_IDs into chunks
    n_chunks <- ceiling(length(unique_grids) / chunk_size)
    grid_chunks <- split(unique_grids, 
                         cut(seq_along(unique_grids), n_chunks, labels = FALSE))
  
    print(n_chunks)
    # head(grid_chunks)
    
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
        dplyr::select(chunk_data, -Date, -PMF_conc, -grid_ID)
      
      # Get base RF predictions
      rf_predictions <- predict(rf_model_noGrid_cv, chunk_data_use)
      
      # Return results as data frame
      chunk_pred = 
        data.frame(
        Date = chunk_data$Date,
        grid_ID = chunk_data$grid_ID,
        PMF_conc = NA,
        rf_predictions = rf_predictions
      )
      head(chunk_pred); dim(chunk_pred)
      
      return(chunk_pred)
    }
    
    # Determine number of cores to use (leave one core free)
    n_cores <- detectCores() - 2
    
    # # Try processing just one chunk first to test:
    # test_result <- process_chunk(grid_chunks[[1]])
    
    # Process chunks in parallel
    chunk_predictions <- mclapply(
      grid_chunks, # grid_chunks[[1]], grid_chunks[1:3], grid_chunks,
      process_chunk,
      mc.cores = n_cores
    )
    
    # Combine all chunks
    all_predictions <- do.call(rbind, chunk_predictions)
    dim(all_predictions)
    length(unique(all_predictions$grid_ID))
    
    # Ensure predictions are ordered by Date and grid_ID
    all_predictions <- all_predictions %>%
      dplyr::arrange(Date, grid_ID)
    
    return(all_predictions)
  }


# Generate predictions, using data with andn without Longitude or Latitude
# names(rf_model_cv$trainingData)
# names(rf_model_noGrid_cv$trainingData)

rf_with_pmf_predictions <- 
  rf_cv_predictions_with_uncertainty(
    dplyr::select(model_input_use, -Dataset, -Date, -Source_aftermanual, -year))
    # dplyr::select(model_input_use, -Longitude, -Latitude, -Dataset, -Date, -Source_aftermanual, -year))


# unique_nopmf_sites <- unique(model_input_all_grid_noPMF$grid_ID)
# train_nopmf_sites <- sample(unique_nopmf_sites, size = floor(0.05 * length(unique_nopmf_sites)))
# grid_us_try <- model_input_all_grid_noPMF %>% filter(grid_ID %in% train_nopmf_sites)
# grid_us_try = subset(grid_us_try, month %in% c(1, 4, 7))
# dim(grid_us_try)
# 
# # ATTENTION!! Date is included in the input!!!
# rf_no_pmf_predictions <-
#   rf_cv_predictions_parallel(
#     dplyr::select(grid_us_try, -Dataset, -Source_aftermanual, -year), 200)
# head(grid_us_try); dim(grid_us_try)
# head(rf_no_pmf_predictions); dim(rf_no_pmf_predictions)

# ATTENTION!! Date is included in the input!!!
rf_no_pmf_predictions <- 
  rf_cv_predictions_parallel(
    dplyr::select(model_input_all_grid_noPMF, -Dataset, -Source_aftermanual, -year), 200)
    # dplyr::select(model_input_all_grid_noPMF, -Longitude, -Latitude, -Dataset, -Source_aftermanual, -year), 100)

cor(rf_with_pmf_predictions$PMF_conc, rf_with_pmf_predictions$mean)

# Add Date and rename columns
rf_with_pmf_predictions$Date = model_input_use$Date
rf_with_pmf_predictions = 
  plyr::rename(rf_with_pmf_predictions, 
               c("mean" = "rf_predictions"))

# Merge two predictions
rf_with_pmf_predictions_sharedCol =
  dplyr::select(rf_with_pmf_predictions, names(rf_no_pmf_predictions))
rf_all_predictions = 
  bind_rows(rf_with_pmf_predictions_sharedCol, rf_no_pmf_predictions)

# Get month, long and lat
rf_all_predictions$Month = month(rf_all_predictions$Date)

rf_all_predictions_coords =
  rf_all_predictions %>%
  dplyr::mutate(
    Longitude = as.numeric(str_extract(grid_ID, "-?\\d+\\.\\d+")),
    Latitude = as.numeric(str_extract(grid_ID, "\\d+\\.\\d+$"))
  )

head(rf_all_predictions_coords); dim(rf_all_predictions_coords)

# Output files
write_fst(rf_all_predictions_coords,
          paste0("RF_overall_No_coords_", source.test, "_", cmaq_period, ".fst"))
write_fst(rf_with_pmf_predictions,
          paste0("RF_SiteID_Uncertainty_No_coords_", source.test, "_", cmaq_period, ".fst"))

