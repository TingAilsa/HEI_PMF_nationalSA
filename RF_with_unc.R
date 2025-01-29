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
library(USAboundaries)

# Detect the number of cores available on the server
num_cores <- detectCores() - 1  # Use all but one core
num_cores

# Create a parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# setwd("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/MLresult_RF/")

# setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/machine_learning_source_input/")
getwd()
base_dir = getwd()

#### 0. Extract long & points with the continental US ####
us_states = USAboundaries::us_states()
us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]

# # Create sequences for longitude and latitude with 0.1 degree intervals
# lons <- seq(-125.05, -66.05, by = 0.1)
# lats <- seq(24.05, 50.05, by = 0.1)
# length(lons); length(lats)
# 
# # Create a grid of all combinations
# grid_points <- expand.grid(Longitude = lons, Latitude = lats)
# head(grid_points)
# dim(grid_points)
# 
# # Convert to sf object
# grid_sf <- st_as_sf(grid_points,
#                     coords = c("Longitude", "Latitude"),
#                     crs = 4326)
# dim(grid_sf)
# 
# 
# # Find points that intersect with continental US
# points_in_us <- st_intersects(grid_sf, us_states)
# dim(points_in_us)
# 
# # Keep only points that fall within US (points that have matches)
# us_point_coord <- grid_points[lengths(points_in_us) > 0, ]
# dim(grid_points); dim(us_point_coord)
# length(unique(us_point_coord$Longitude)); length(unique(us_point_coord$Latitude))
# 
# # Check the spatial distribution
# ggplot() +
#   geom_point(data = us_point_coord,
#              aes(x = Longitude, y = Latitude),
#              size = 0.1, color = "grey45") +
#   geom_point(data = grid_points,
#              aes(x = Longitude, y = Latitude),
#              size = 0.1, color = "dodgerblue", alpha = 0.2) +
#   geom_sf(data = us_states,
#           fill = NA, color = "grey75") +
#   coord_sf(xlim = c(-130, -65), ylim = c(25, 50)) +
#   theme_minimal(base_size = 16) +
#   labs(x = "Longitude",
#        y = "Latitude")
# 
# write_fst(us_point_coord, "Long_lat_Mainland_US_0.1_degree.fst")

#### 1. Modeling Input ####

cmaq_period = "2011-01_2011-12"
# cmaq_period = "2011-02_2011-12"

# All date related predictors 
date_use = read.fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Date_DOW_Holiday_2011-20.fst")
us_point_coord = read.fst("Long_lat_Mainland_US_0.1_degree.fst")
dim(us_point_coord)

# date_use = read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Date_DOW_Holiday_2011-20.fst")
# head(date_use)

###### 1.1 Sulfate input ######
model_input_ini = read_fst(paste0("Sulfate_only_PMF_points_input_", cmaq_period, ".fst"))
model_input_all_grid = read_fst(paste0("Sulfate_all_CMAQ_points_input_", cmaq_period, ".fst"))

# model_input_ini = read_fst(paste0("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/var_combined_rds/Sulfate_only_PMF_points_input_", cmaq_period, ".fst"))
# model_input_all_grid = read_fst(paste0("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/var_combined_rds/Sulfate_all_CMAQ_points_input_", cmaq_period, ".fst"))

source.test = "Secondary_Sulfate"
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

# # Check if the points are correctly extracted
# model_input_all_grid_coords <- 
#   dplyr::select(model_input_all_grid, Longitude, Latitude)
# model_input_all_grid_us_coords <- 
#   dplyr::select(model_input_all_grid_us, Longitude, Latitude)
# model_input_all_grid_coords = model_input_all_grid_coords[!duplicated(model_input_all_grid_coords), ]
# model_input_all_grid_us_coords = model_input_all_grid_us_coords[!duplicated(model_input_all_grid_us_coords), ]
# dim(model_input_all_grid_coords); dim(model_input_all_grid_us_coords)
# length(unique(model_input_all_grid_coords$Longitude)); length(unique(model_input_all_grid_coords$Latitude))
# length(unique(model_input_all_grid_us_coords$Longitude)); length(unique(model_input_all_grid_us_coords$Latitude))
# 
# ggplot() +
#   geom_point(data = model_input_all_grid_coords,
#              aes(x = Longitude, y = Latitude),
#              size = 0.15, color = "dodgerblue", alpha = 0.5) +
#   geom_sf(data = us_states,
#           fill = NA, color = "grey55") +
#   coord_sf(xlim = c(-130, -65), ylim = c(25, 50)) +
#   theme_minimal(base_size = 16) +
#   labs(x = "Longitude",
#        y = "Latitude")
# 
# ggplot() +
#   geom_point(data = model_input_all_grid_us_coords,
#              aes(x = Longitude, y = Latitude),
#              size = 0.15, color = "forestgreen", alpha = 0.5) +
#   geom_sf(data = us_states,
#           fill = NA, color = "grey55") +
#   coord_sf(xlim = c(-130, -65), ylim = c(25, 50)) +
#   theme_minimal(base_size = 16) +
#   labs(x = "Longitude",
#        y = "Latitude")

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

###### 1.2 Dust input ######
model_input_ini = read_fst(paste0("Dust_only_PMF_points_input_", cmaq_period, ".fst"))
model_input_all_grid = read_fst(paste0("Dust_all_CMAQ_points_input_", cmaq_period, ".fst"))
source.test = "Dust"
length(unique(model_input_ini$SiteCode))
head(model_input_ini)

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

###### 1.3 Traffic input ######
model_input_ini = read_fst(paste0("Traffic_only_PMF_points_input_", cmaq_period, ".fst"))
model_input_all_grid = read_fst(paste0("Traffic_all_CMAQ_points_input_", cmaq_period, ".fst"))
source.test = "Traffic"
length(unique(model_input_ini$SiteCode))

dim(model_input_ini); dim(model_input_all_grid)

# Remove the extreme values in 2011.06
model_input_ini =
  subset(model_input_ini, 
         PM25_TOT_NRD <= quantile(model_input_ini$PM25_TOT_NRD, 0.995))
summary(model_input_ini); dim(model_input_ini)

# Use the SUM of PM25_TOT_ONR and PM25_TOT_ONR
model_input_ini$PM25_TOT_Traf = 
  model_input_ini$PM25_TOT_NRD + model_input_ini$PM25_TOT_ONR
summary(model_input_ini); dim(model_input_ini)

# Remove extremes in data for prediction, and use the sum of PM25_TOT_ONR and PM25_TOT_ONR
dim(model_input_all_grid)
model_input_all_grid =
  subset(model_input_all_grid, 
         PM25_TOT_NRD <= max(model_input_ini$PM25_TOT_NRD))

model_input_all_grid$PM25_TOT_Traf = 
  model_input_all_grid$PM25_TOT_NRD + model_input_all_grid$PM25_TOT_ONR
summary(model_input_all_grid); dim(model_input_all_grid)

# Get PMF_conc & CMAQ_conc for use
model_input_ini <-
  plyr::rename(model_input_ini,
               c("Concentration" = "PMF_conc",
                 "PM25_TOT_Traf" = "CMAQ_conc"))

model_input_all_grid <-
  plyr::rename(model_input_all_grid,
               c("Concentration" = "PMF_conc",
                 "PM25_TOT_Traf" = "CMAQ_conc"))

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

#### 2. Random Forest: Predict with Uncertainty #### 

model.method = "Random Forest"

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

# # Function to make predictions for new locations (without grid_ID) in chunks
# rf_cv_predictions_with_uncertainty_chunks <- 
#   function(full_pred_data, chunk_size) { # chunk_size, number of grid_IDs per chunk
#     n_sims = 1000 # number of simulated possible values 
#     
#   # Get unique grid_IDs
#   unique_grids <- unique(full_pred_data$grid_ID)
#   
#   # Split grid_IDs into chunks
#   n_chunks <- ceiling(length(unique_grids) / chunk_size)
#   grid_chunks <- split(unique_grids, 
#                        cut(seq_along(unique_grids), n_chunks, labels = FALSE))
#   
#   # Initialize list to store results
#   chunk_predictions <- list()
#   
#   # Process each chunk
#   for(i in seq_along(grid_chunks)) {
#     # Select data for current chunk of grid_IDs
#     chunk_data <- full_pred_data %>%
#       filter(grid_ID %in% grid_chunks[[i]]) # PBMCL apply, multiple cores
#     
#     # Prepare data for prediction (remove columns not used in model)
#     chunk_data_use <- 
#       dplyr::select(chunk_data, -PMF_conc, -Dataset, -Date, -Source_aftermanual, -year, -grid_ID) # -Dataset, -Date, -Source_aftermanual, -year, -grid_ID
#     
#     # Get base RF predictions using model without grid_ID
#     rf_predictions <- predict(rf_model_noGrid_cv, chunk_data_use)
#     
#     # # Initialize matrix for simulations
#     # rf_simul_predictions <- matrix(nrow = nrow(chunk_data), ncol = n_sims)
#     # 
#     # # Run simulations
#     # for(j in 1:n_sims) {
#     #   # Use average uncertainty for new locations
#     #   grid_effects <- rnorm(nrow(chunk_data), 0, rf_grid_residual_effect_mean_sd)
#     #   res_effects <- rnorm(nrow(chunk_data), 0, rf_within_grid_residual_sd_mean)
#     #   
#     #   # Final prediction for this simulation
#     #   rf_simul_predictions[,j] <- rf_predictions + grid_effects + res_effects
#     # }
#     # 
#     # # Calculate prediction intervals
#     # pred_summary <- t(apply(rf_simul_predictions, 1, function(x) {
#     #   c(mean = mean(x),
#     #     median = median(x),
#     #     sd = sd(x),
#     #     q025 = quantile(x, 0.025),
#     #     q975 = quantile(x, 0.975))
#     # }))
#     
#     # Combine with original identifiers
#     chunk_predictions[[i]] <- data.frame(
#       Date = chunk_data$Date,
#       grid_ID = chunk_data$grid_ID,
#       PMF_conc = NA,  # NA for predictions
#       rf_predictions = rf_predictions
#       # as.data.frame(pred_summary)
#     )
#     
#     # Print progress
#     cat(sprintf("Processed chunk %d of %d\n", i, n_chunks))
#   }
#   
#   # Combine all chunks
#   all_predictions <- do.call(rbind, chunk_predictions)
#   
#   # Ensure predictions are ordered by Date and grid_ID
#   all_predictions <- all_predictions %>%
#     arrange(Date, grid_ID)
#   
#   return(all_predictions)
# }

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
  
  # Define the chunk processing function
  process_chunk <- function(chunk_grids) {
    # Select data for current chunk of grid_IDs
    chunk_data <- full_pred_data %>%
      filter(grid_ID %in% chunk_grids)
    
    # Prepare data for prediction
    chunk_data_use <- dplyr::select(chunk_data, 
                                    -PMF_conc, -Dataset, -Date,
                                    -Source_aftermanual, -year, -grid_ID)
    
    # Get base RF predictions
    rf_predictions <- predict(rf_model_noGrid_cv, chunk_data_use)
    
    # Return results as data frame
    data.frame(
      Date = chunk_data$Date,
      grid_ID = chunk_data$grid_ID,
      PMF_conc = NA,
      rf_predictions = rf_predictions
    )
  }
  
  # Determine number of cores to use (leave one core free)
  n_cores <- detectCores() - 1
  
  # Process chunks in parallel
  chunk_predictions <- mclapply(
    grid_chunks,
    process_chunk,
    mc.cores = n_cores
  )
  
  # Combine all chunks
  all_predictions <- do.call(rbind, chunk_predictions)
  
  # Ensure predictions are ordered by Date and grid_ID
  all_predictions <- all_predictions %>%
    arrange(Date, grid_ID)
  
  return(all_predictions)
}

# Generate predictions
rf_with_pmf_predictions <- 
  rf_cv_predictions_with_uncertainty(model_input_use)
# rf_no_pmf_predictions <- 
#   rf_cv_predictions_with_uncertainty_chunks(
#   model_input_all_grid_us, 100)
rf_no_pmf_predictions <- 
  rf_cv_predictions_parallel(
    model_input_all_grid_us, 100)

cor(rf_with_pmf_predictions$PMF_conc, rf_with_pmf_predictions$mean)

# # Merge two predictions
# ncol_no_pmf = ncol(rf_no_pmf_predictions)
# names(rf_no_pmf_predictions)[(ncol_no_pmf-1):ncol_no_pmf] = c("q025.2.5%", "q975.97.5%")
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
write_fst(rf_all_predictions_coords,
          paste0("RF_overall_prediction_SiteID_", source.test, "_", cmaq_period, ".fst"))




# Get monthly average and whold study period average
rf_predictions_coords_avg = 
  rf_all_predictions_coords %>%
  group_by(grid_ID, Longitude, Latitude) %>%
  dplyr::summarize(
    mean_predction = mean(mean, na.rm = TRUE),
    PMF_conc = mean(mean, na.rm = TRUE),
    .groups = "drop"
  )
head(rf_predictions_coords_avg); dim(rf_predictions_coords_avg)

rf_predictions_coords_month = 
  rf_all_predictions_coords %>%
  group_by(grid_ID, Longitude, Latitude, Month) %>%
  dplyr::summarize(
    mean_predction = mean(mean, na.rm = TRUE),
    PMF_conc = mean(mean, na.rm = TRUE),
    .groups = "drop"
  )
head(rf_predictions_coords_month); dim(rf_predictions_coords_month)

# Plot spatial distribution

ggplot() +
  geom_point(data = rf_predictions_coords_avg, 
             aes(x = Longitude, y = Latitude, color = mean_predction),
             size = 0.25) +
  geom_sf(data = us_states, 
          fill = NA, color = "grey45") + 
  scale_color_viridis_c(name = "Average\nConcentration") +
  coord_sf(xlim = c(-130, -65), ylim = c(25, 50)) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude", 
       y = "Latitude")

ggplot() +
  geom_point(data = rf_predictions_coords_avg, 
             aes(x = Longitude, y = Latitude, color = mean_predction),
             size = 0.35, alpha = 0.8) +
  geom_sf(data = us_states, 
          fill = NA, color = "grey70", size = 0.3) + 
  scale_color_viridis_c(name = "Avg. Concentration", option = "plasma") +
  coord_sf(xlim = c(-130, -65), ylim = c(25, 50), expand = FALSE) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude", 
       y = "Latitude", 
       title = "Average Predicted Concentration Across the US: Dust",
       subtitle = "Spatial distribution of predictions by grid") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 15),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 15)
  )


ggplot() +
  geom_point(data = rf_predictions_coords_month, 
             aes(x = Longitude, y = Latitude, color = mean_predction),
             size = 0.25) +
  geom_sf(data = us_states, 
          fill = NA, color = "grey45") + 
  facet_grid(Month~.) +
  scale_color_viridis_c(name = "Average\nConcentration") +
  coord_sf(xlim = c(-130, -65), ylim = c(25, 50)) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude", 
       y = "Latitude")




# Diagnostic information
rf_diagnostics <- 
  list(
  rf_performance = rf_model_cv$results,
  grid_effects = rf_grid_residual,
  variance_components = list(
    grid_effect_variance = rf_grid_residual_effect_mean_sd^2,
    within_grid_variance = rf_within_grid_residual_sd_mean^2
  ),
  grid_effect_proportion = 
    rf_grid_residual_effect_mean_sd^2 / (rf_grid_residual_effect_mean_sd^2 + rf_within_grid_residual_sd_mean^2)
)

# Extract sites with high grid effect
rf_grid_effects = rf_diagnostics$grid_effects
rf_grid_effects$mean_res_abs = abs(rf_grid_effects$mean_residual)
rf_res_threshold = quantile(rf_grid_effects$mean_res_abs, 0.975)

rf_high_grid_effect <- 
  subset(rf_grid_effects, mean_res_abs > rf_res_threshold)
rf_normal_grid_effect = 
  subset(rf_grid_effects, mean_res_abs <= rf_res_threshold)
head(rf_high_grid_effect); head(rf_normal_grid_effect)
dim(rf_high_grid_effect); dim(rf_normal_grid_effect)

# Plot 1: Distribution of grid effects
grid_effect_distribution <- 
  ggplot(rf_grid_effects, 
             aes(x = mean_residual)) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  labs(title = "Distribution of Grid Effects",
       x = "Mean Residual by Grid",
       y = "Count")

# Plot 2: Uncertainty vs Number of Observations
uncertainty_vs_observation_count <- 
  ggplot(rf_grid_effects, 
             aes(x = n_obs, y = sd_residual)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Grid Uncertainty vs Sample Size",
       x = "Number of Observations",
       y = "Standard Deviation of Residuals")


no_pmf_pred_cor = cor(rf_no_pmf_predictions$PMF_conc, rf_no_pmf_predictions$mean)
with_pmf_pred_cor = cor(rf_with_pmf_predictions$PMF_conc, rf_with_pmf_predictions$mean)

###### 2.2 Random Forest with uncertainty, train & test, cross validation ###### 

# Randomly select 80% of sites for training
unique_sites <- unique(model_input_use$grid_ID)
train_sites <- sample(unique_sites, size = floor(0.8 * length(unique_sites)))

# Create training and test datasets
train_data <- model_input_use %>% filter(grid_ID %in% train_sites)
test_data <- model_input_use %>% filter(!grid_ID %in% train_sites)

model_input_use = test_data
model_input_all_grid_us = train_data

# Set up 5-fold cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Fit the Random Forest model with and without grid_ID
rf_model_cv <- caret::train(
  PMF_conc ~ ., #  + year 
  data = dplyr::select(model_input_use, -Dataset, -Date, -Source_aftermanual, -year), # dplyr::select(model_input, -Dataset, -Date, -SiteCode), # model_input_train, 
  method = "rf",
  trControl = train_control,
  importance = TRUE
)

rf_model_noGrid_cv <- caret::train(
  PMF_conc ~ ., #  + year 
  data = dplyr::select(model_input_use, -Dataset, -Date, -Source_aftermanual, -year, -grid_ID), # dplyr::select(model_input, -Dataset, -Date, -SiteCode), # model_input_train, 
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
  summarize(
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

# Generate predictions
rf_with_pmf_predictions <- rf_cv_predictions_with_uncertainty(model_input_use)
rf_no_pmf_predictions <- rf_cv_predictions_with_uncertainty(model_input_all_grid_us)

cor(rf_no_pmf_predictions$PMF_conc, rf_no_pmf_predictions$mean)
cor(rf_with_pmf_predictions$PMF_conc, rf_with_pmf_predictions$mean)

# Diagnostic information
rf_diagnostics <- 
  list(
    rf_performance = rf_model_cv$results,
    grid_effects = rf_grid_residual,
    variance_components = list(
      grid_effect_variance = rf_grid_residual_effect_mean_sd^2,
      within_grid_variance = rf_within_grid_residual_sd_mean^2
    ),
    grid_effect_proportion = 
      rf_grid_residual_effect_mean_sd^2 / (rf_grid_residual_effect_mean_sd^2 + rf_within_grid_residual_sd_mean^2)
  )

# Extract sites with high grid effect
rf_grid_effects = rf_diagnostics$grid_effects
rf_grid_effects$mean_res_abs = abs(rf_grid_effects$mean_residual)
rf_res_threshold = quantile(rf_grid_effects$mean_res_abs, 0.975)

rf_high_grid_effect <- 
  subset(rf_grid_effects, mean_res_abs > rf_res_threshold)
rf_normal_grid_effect = 
  subset(rf_grid_effects, mean_res_abs <= rf_res_threshold)
head(rf_high_grid_effect); head(rf_normal_grid_effect)
dim(rf_high_grid_effect); dim(rf_normal_grid_effect)

# Plot 1: Distribution of grid effects
grid_effect_distribution <- 
  ggplot(rf_grid_effects, 
         aes(x = mean_residual)) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  labs(title = "Distribution of Grid Effects",
       x = "Mean Residual by Grid",
       y = "Count")

# Plot 2: Uncertainty vs Number of Observations
uncertainty_vs_observation_count <- 
  ggplot(rf_grid_effects, 
         aes(x = n_obs, y = sd_residual)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Grid Uncertainty vs Sample Size",
       x = "Number of Observations",
       y = "Standard Deviation of Residuals")


pmf_pred_cor = cor(rf_no_pmf_predictions$PMF_conc, rf_no_pmf_predictions$mean)
pmf_pred_cor = cor(rf_with_pmf_predictions$PMF_conc, rf_with_pmf_predictions$mean)

###### 2.3 Random Forest with uncertainty - Holdout ######

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
# prediction_noPMF_data = subset(model_input_all_grid_us_holdout, month <= 3)
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
    
    # Add date
    predictions_train$Date = train_data$Date
    predictions_test$Date = test_data$Date
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
# model_input_all_grid_us_holdout <- 
#   model_input_all_grid_us %>% dplyr::select(-all_of(columns_to_remove))

# Perform iterations
set.seed(123)
n_iterations <- 3
holdout_results <- list()

for(i in 1:n_iterations) {
  message(sprintf("Processing iteration %d of %d", i, n_iterations))
  holdout_results[[i]] <-
    rf_holdout_predictions_with_uncertainty(
      model_input_use_holdout, i) # , model_input_all_grid_us_holdou
}

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

#### 5. Random Forest modeling #### 

model.method = "Random Forest"

# original data
conc_plot_data <- data.frame(
  Date = model_input$Date,
  PMF_conc = model_input$PMF_conc,
  CMAQ_conc = model_input$CMAQ_conc
) 

model_input$SiteCode = as.factor(model_input$SiteCode)
summary(is.na(model_input))

###### 5.1 Random Forest, cross validation ###### 

# Set up 5-fold cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Fit the Random Forest model
rf_model_cv <- caret::train(
  PMF_conc ~ ., #  + year 
  data = dplyr::select(model_input, -Dataset, -Date, -Source_aftermanual, -year), # dplyr::select(model_input, -Dataset, -Date, -SiteCode), # model_input_train, 
  method = "rf",
  trControl = train_control,
  importance = TRUE
)

# summary(rf_model_cv)
print(rf_model_cv)

# Check the hyperparameter setting
caret.rf.mtry = rf_model_cv$bestTune$mtry
caret.rf.ntree = rf_model_cv$finalModel$ntree
summary(rf_model_cv$finalModel)
# rf_model_cv$bestTune
# rf_model_cv$modelInfo$library
# rf_model_cv$modelInfo$type
# rf_model_cv$modelInfo$fit
# rf_model_cv$modelInfo$prob
# rf_model_cv$modelInfo$predict
# rf_model_cv$modelInfo$predictors
# rf_model_cv$modelInfo$levels
# rf_model_cv$modelInfo$tags
# rf_model_cv$modelInfo$oob
# rf_model_cv$modelType
# rf_model_cv$pred
# rf_model_cv$call
# rf_model_cv$method

print(caret.rf.mtry); print(caret.rf.ntree)

# Predict on the training data
rf_cv_predictions <- predict(rf_model_cv, model_input)

# Evaluate the model performance
rf_cv_performance <- 
  postResample(rf_cv_predictions, model_input$PMF_conc)
print(rf_cv_performance)

# Extract performance metrics
rmse_rf <- round(rf_cv_performance["RMSE"], 3)
rsq_rf <- round(rf_cv_performance["Rsquared"], 2)
mae_rf <- round(rf_cv_performance["MAE"], 3)
rmse_rf; rsq_rf; mae_rf

###### Plot the relative variable importance of each predictor
# Extract variable importance
rf_cv_var_imp <- varImp(rf_model_cv, scale = TRUE)

# Convert variable importance to a data frame
rf_cv_var_imp_df <- rf_cv_var_imp$importance

# Add Variable names as a column
rf_cv_var_imp_df$Variable <- rownames(rf_cv_var_imp_df)
rownames(rf_cv_var_imp_df) <- NULL
head(rf_cv_var_imp_df)
names(rf_cv_var_imp_df) = c("Relative_Influence", "Variable")

# Backup of original variables
rf_cv_var_imp_df$Variable_sum = rf_cv_var_imp_df$Variable
unique(rf_cv_var_imp_df$Variable)

# Group all SiteCode into one
rf_cv_var_imp_df$Variable[grepl("Station", rf_cv_var_imp_df$Variable, fixed = T) |
                            grepl("SiteCode", rf_cv_var_imp_df$Variable, fixed = T)] = "SiteCode"
unique(rf_cv_var_imp_df$Variable)

rf_cv_imp_site <-
  ggplot(subset(rf_cv_var_imp_df, Variable == "SiteCode"),
         aes(x = Variable, y = Relative_Influence)) +
  geom_boxplot(outlier.shape = NA, fill = NA,
               linewidth = 0.6, width = 0.5) +
  geom_jitter(width = 0.18, alpha = 0.65) +
  theme_minimal(base_size = 22)

write_fst(rf_cv_var_imp_df, 
          paste0("RF_cross-validation_no_siteID_", source.test, 
                 "_Variable_Imp_Site#", Site.No, "_", dataset, "_", cmaq_period, ".fst"))
ggsave(rf_cv_imp_site, 
       paste0("RF_cross-validation_no_siteID_", source.test, 
              "_Variable_Imp_Site#", Site.No, "_", dataset, "_", cmaq_period, ".pdf"))

# Calculate the mean and standard deviation of variable importance across iterations
rf_cv_var_imp_sum <- 
  rf_cv_var_imp_df %>%
  group_by(Variable) %>%
  summarize(Relative_Influence = median(Relative_Influence))

### Predictor performance plot
rf_var_influence_p <-
  ggplot(rf_cv_var_imp_df, 
         aes(x = reorder(Variable, 
                         Relative_Influence), 
             y = Relative_Influence)) +
  geom_bar(stat = "identity",
           fill = "steelblue",
           width = 0.5) +
  coord_flip() +  # Flip coordinates to make a horizontal bar plot
  labs(title = "Variable Performance",
       x = "Predictor",
       y = "Relative Influence (%)") +
  theme_minimal(base_size = 16)

ggplot(subset(rf_cv_var_imp_df, Variable != "SiteCode"), 
       aes(x = reorder(Variable, 
                       Relative_Influence), 
           y = Relative_Influence)) +
  geom_bar(stat = "identity",
           fill = "steelblue",
           width = 0.5) +
  coord_flip() +  # Flip coordinates to make a horizontal bar plot
  labs(title = "Variable Performance",
       x = "Predictor",
       y = "Relative Influence (%)") +
  theme_minimal(base_size = 16)

# theme(plot.title = element_text(hjust = 2.5))

### Prediction vs. measurement
conc_plot_rf_cv = conc_plot_data
conc_plot_rf_cv$RF_Prediction = rf_cv_predictions

# calculate the limits based on the range of the data
max_rf <- 
  max(c(conc_plot_data$PMF_conc, 
        conc_plot_data$CMAQ_conc, 
        conc_plot_rf_cv$RF_Prediction), 
      na.rm = TRUE)
min_rf <-
  min(c(conc_plot_data$PMF_conc, 
        conc_plot_data$CMAQ_conc, 
        conc_plot_rf_cv$RF_Prediction), 
      na.rm = TRUE)

pmf_vs_cmaq_rf <-
  ggplot(conc_plot_data, aes(x = PMF_conc, y = CMAQ_conc)) +
  geom_point(color = "darkgreen", alpha = 0.15) +
  coord_equal() + # enforces an equal scaling between the axes
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = paste0("PMF vs. CMAQ"),
       x = "PMF conc µg/m3",
       y = "CMAQ conc µg/m3") +
  theme_minimal(base_size = 16) +
  xlim(min_rf, max_rf) +
  ylim(min_rf, max_rf)

pmf_vs_predicted_rf <-
  ggplot(conc_plot_rf_cv, aes(x = PMF_conc, y = RF_Prediction)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  coord_equal() + # enforces an equal scaling between the axes
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = paste0("PMF vs. ML Predictions"),
       x = "PMF conc µg/m3",
       y = "ML Predictions µg/m3") +
  theme_minimal(base_size = 16) +
  xlim(min_rf, max_rf) +
  ylim(min_rf, max_rf)


##### time series

# Combine data reshaping and summarization using pivot_longer
rf_cv_daily <- 
  conc_plot_rf_cv %>%
  dplyr::select(Date, PMF_conc, RF_Prediction) %>%
  pivot_longer(
    cols = c(PMF_conc, RF_Prediction),
    names_to = "Variable",
    values_to = "Concentration"
  ) %>%
  group_by(Date, Variable) %>%
  dplyr::summarize(
    median_Conc = round(median(Concentration, na.rm = TRUE), 7),
    lower_CI = round(quantile(Concentration, probs = 0.025, na.rm = TRUE), 7),
    upper_CI = round(quantile(Concentration, probs = 0.775, na.rm = TRUE), 7), 
    .groups = "drop"
  )

summary(rf_cv_daily$lower_CI <= rf_cv_daily$median_Conc)
summary(rf_cv_daily$median_Conc <= rf_cv_daily$upper_CI)

pmf_rf_daily_plot <-
  ggplot(rf_cv_daily, # , Date > as.Date("2011-04-01")
         aes(x = Date, y = median_Conc, 
             color = Variable, fill = Variable)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), 
              alpha = 0.2, color = NA) +
  labs(
    title =  paste0("Daily Median with 95% CI"),
    x = "Date",
    y = "Concentration"
  ) +
  theme_minimal(base_size = 16) +
  theme(legend.position = c(0.15, 0.9), 
        legend.background = element_blank(),
        legend.title = element_text(size = 0))

# Combine the figure
title_text <- paste0(model.method, " for ", 
                     source.test, " ", dataset,
                     ": RMSE, ", rmse_rf,
                     "; MAE, ", mae_rf,
                     "; R-squared, ", rsq_rf)

combined_rf_cv_plot <- 
  rf_var_influence_p + ((pmf_vs_cmaq_rf + pmf_vs_predicted_rf) / pmf_rf_daily_plot) +
  plot_layout(widths = c(2, 5)) +
  plot_annotation(title = title_text,
                  theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))
combined_rf_cv_plot

# Output the figure
# ggsave(paste0("RF_cross-validation_", source.test, "_", dataset, "_", cmaq_period, ".pdf"), 
#        plot = combined_rf_cv_plot, 
#        width = 14.5, height = 10)

ggsave(paste0("RF_cross-validation_no_siteID_", "_Site#", Site.No, source.test, "_", dataset, "_", cmaq_period, ".pdf"), 
       plot = combined_rf_cv_plot, 
       width = 14.5, height = 10)

###### 5.2 Random Forest, Holdout Analyses ###### 

# Exclude variables not to use
model_input_site = 
  dplyr::select(model_input, -Dataset, -Source_aftermanual, -year) # model_input_site

head(model_input_site); dim(model_input_site)
length(unique(model_input_site$Longitude, model_input_site$Latitude))
# length(unique(model_input_site$Date))

# Create an empty list to store results from 50 iterations
all_rf_predictions <- list()
rf_rmse_list <- c()
rf_rsq_list <- c()
rf_mae_list <- c()

# Lists to store variable importance from each iteration
rf_ct_var_imp_list <- list()
shapley_results_list <- list()

# Number of iterations
n_iterations <- 25 # 50 30 3

# Loop for 50 iterations
for (i in 1:n_iterations) {
  
  # Split data into 80% train and 20% test based on unique SiteCode
  set.seed(i)  # for reproducibility
  unique_sites <- unique(model_input_site$SiteCode)
  
  # Randomly select 80% of sites for training
  train_sites <- sample(unique_sites, size = floor(0.8 * length(unique_sites)))
  # length(unique_sites); length(train_sites)
  
  # Create training and test datasets
  train_data <- model_input_site %>% filter(SiteCode %in% train_sites)
  test_data <- model_input_site %>% filter(!SiteCode %in% train_sites)
  
  # Set up training input for modeling
  model_input_train <- dplyr::select(train_data, -Date) # -SiteCode, 
  summary(model_input_train)
  # cor.test(model_input_train$PMF_conc, model_input_train$CMAQ_conc)
  # cor.test(model_input_train$PMF_conc, model_input_train$PM25_TOT_ONR)
  # cor.test(model_input_train$PMF_conc, model_input_train$sRoadiness.1.)
  
  # Set up test input for prediction
  model_input_test <- dplyr::select(test_data, -Date) # -SiteCode, 
  
  # Fit the Random Forest model on training data
  # Disable resampling
  train_control <- trainControl(method = "none")
  # train_control <- trainControl(method = "cv", number = 5)
  
  rf_model_ct <- caret::train(
    PMF_conc ~ ., 
    data = model_input_train, 
    method = "rf",
    trControl = train_control,  
    importance = TRUE,
    ntree = 500
  )
  
  # summary(rf_model_ct)
  # print(rf_model_ct)
  # rf_model_ct$finalModel
  # rf_model_ct$bestTune
  # rf_model_ct$pred
  # rf_model_ct$coefnames
  
  rf_test_predictions <- predict(rf_model_ct, model_input_test)
  postResample(rf_test_predictions, model_input_test$PMF_conc)
  # rf_model_ct$bestTune$mtry; rf_model_ct$finalModel$ntree
  
  ##### partial importance plot  
  
  
  # Store variable importance for this iteration
  rf_ct_var_imp <- 
    data.frame(
      Importance = rf_model_ct$finalModel$importance[, "IncNodePurity"],
      Variable = rownames(rf_model_ct$finalModel$importance))
  
  rf_ct_var_imp$Iteration <- i
  rf_ct_var_imp_list[[i]] <- rf_ct_var_imp
  
  # Predict on the test data
  rf_test_predictions <- predict(rf_model_ct, model_input_test)
  
  # Store the predictions and actual values
  all_rf_predictions[[i]] <- 
    data.frame(SiteCode = test_data$SiteCode,
               Date = test_data$Date,
               PMF_conc = test_data$PMF_conc,
               RF_Prediction = rf_test_predictions)
  
  # Evaluate performance metrics for this iteration
  rf_performance <- postResample(rf_test_predictions, model_input_test$PMF_conc)
  rf_rmse_list[i] <- rf_performance["RMSE"]
  rf_rsq_list[i] <- rf_performance["Rsquared"]
  rf_mae_list[i] <- rf_performance["MAE"]
}

# Combine all predictions from 50 iterations
combined_rf_predictions <- bind_rows(all_rf_predictions)

# Calculate the average predictions per site
final_rf_predictions <- 
  combined_rf_predictions %>%
  group_by(SiteCode, Date) %>%
  summarize(
    Mean_RF_Prediction = mean(RF_Prediction, na.rm = TRUE),
    Actual_PMF = mean(PMF_conc, na.rm = TRUE))

# Calculate overall RMSE, MAE, R-squared using the averaged predictions
final_rf_performance <- 
  postResample(final_rf_predictions$Mean_RF_Prediction, 
               final_rf_predictions$Actual_PMF)

cor(final_rf_predictions$Mean_RF_Prediction, final_rf_predictions$Actual_PMF)

# Output performance metrics
final_rf_rmse <- round(final_rf_performance["RMSE"], 3)
final_rf_rsq <- round(final_rf_performance["Rsquared"], 2)
final_rf_mae <- round(final_rf_performance["MAE"], 3)
final_rf_rmse; final_rf_rsq; final_rf_mae

# Plotting the averaged results

# Plot actual vs predicted for final predictions
rf_predictions_vs_pmf <- 
  ggplot(final_rf_predictions, aes(x = Actual_PMF, y = Mean_RF_Prediction)) +
  geom_point(color = "blue", alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "PMF vs. Averaged RF Predictions",
       x = "Actual PMF Concentration (µg/m3)",
       y = "Mean Predicted PMF Concentration (µg/m3)") +
  theme_minimal(base_size = 16)


##### Relative variable importance
# Combine all variable importance results into one dataframe
combined_var_imp <- bind_rows(rf_ct_var_imp_list)

# Get one variable to represent Station_ID, if using it as predictor
combined_var_imp$Variable_sum = combined_var_imp$Variable
combined_var_imp$Variable[grepl("Station", combined_var_imp$Variable, fixed = T) |
                            grepl("SiteCode", combined_var_imp$Variable, fixed = T)] = "SiteCode"

# Calculate the mean and standard deviation of variable importance across iterations
final_var_imp_summary <- combined_var_imp %>%
  group_by(Variable) %>%
  summarize(Mean_Importance = mean(Importance),
            SD_Importance = sd(Importance))


relative_var_imp_rf <-
  ggplot(final_var_imp_summary, 
         aes(x = reorder(Variable, Mean_Importance), 
             y = Mean_Importance)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  geom_errorbar(aes(ymin = Mean_Importance - SD_Importance, 
                    ymax = Mean_Importance + SD_Importance), 
                width = 0.2, color = "gray") +
  coord_flip() +  # Flip coordinates to make a horizontal bar plot
  labs(title = "Variable Importance",
       x = "Predictor",
       y = "Mean Importance (IncNodePurity)") +
  theme_minimal(base_size = 16)


# The combined figure
overall_title =
  title = paste0(source.test, ": PMF vs. Averaged RF Predictions (50 Holdout Iterations)",
                 "\nRMSE:", final_rf_rmse, 
                 " , MAE:", final_rf_mae, 
                 " , R-squared:", final_rf_rsq)
combined_plot_rf_ct =
  rf_predictions_vs_pmf + relative_var_imp_rf + # shapley_summary_rf_ct_imp +
  plot_annotation(title = overall_title,
                  theme = theme(plot.title = element_text(size = 20, hjust = 0.5)))
combined_plot_rf_ct

# Output the figure
ggsave(paste0("RF_holdout_Normalized_SiteID_", source.test, "Site#", Site.No, "_", dataset,
              "_", cmaq_period, ".pdf"),
       # "90train__", cmaq_period, ".pdf"),
       plot = combined_plot_rf_ct,
       width = 14.5, height = 10)

# ggsave(paste0("RF_holdout_caret_no_SiteID_", source.test, "_", dataset,
#               "_", cmaq_period, ".pdf"),
#        # "90train__", cmaq_period, ".pdf"),
#        plot = combined_plot_rf_ct,
#        width = 14.5, height = 10)


# Overall performance
rf_perfomance_overall = 
  data.frame(conc_RMSE = rf_rmse_list, RSquare = rf_rsq_list, conc_MAE = rf_mae_list)

rf_perfomance_overall =
  rf_perfomance_overall %>%
  pivot_longer(
    cols = c("conc_RMSE", "RSquare", "conc_MAE"),
    names_to = "Model_Validations",
    values_to = "Values"
  )

rf_perfomance_overall$interaction = rep(1:50, each = 3)

rf_perfomance_weak = base::subset(rf_perfomance_overall, 
                                  Model_Validations == "RSquare" &
                                    Values < 0.1)
rf_perfomance_strong = base::subset(rf_perfomance_overall, 
                                    Model_Validations == "RSquare" &
                                      Values > 0.5)
rf_hold_perfomance_overall <- 
  ggplot(rf_perfomance_overall,
         aes(x = Model_Validations, y = Values)) +
  geom_boxplot(linewidth = 0.6, width = 0.5) +
  geom_jitter(aes(color = Model_Validations), 
              width = 0.18, alpha = 0.5, size = 1.5)+
  theme_minimal(base_size = 16)

# Output the figure
ggsave(
  filename = paste0("RF_holdout_Normalized_RMSE_R_MAE_SiteID_", source.test, "Site#", Site.No, "_", dataset,
                    "_", cmaq_period, ".pdf"),
  # "_90train_", cmaq_period, ".pdf"),
  plot = rf_hold_perfomance_overall,
  width = 14.5,
  height = 8
)

# ggsave(
#   filename = paste0("RF_holdout_RMSE_R_MAE_no_SiteID_", source.test, "_", dataset,
#                     "_", cmaq_period, ".pdf"),
#   # "_90train_", cmaq_period, ".pdf"),
#   plot = rf_hold_perfomance_overall,
#   width = 14.5,
#   height = 8
# )

# Output files for later analyses
write_fst(combined_rf_predictions, 
          paste0("RF_holdout_Normalized_rf_predictions_SiteID_", source.test, "Site#", Site.No, "_", dataset,
                 "_", cmaq_period, ".fst"))
write_fst(combined_var_imp, 
          paste0("RF_holdout_Normalized_var_imp_SiteID_", source.test, "Site#", Site.No, "_", dataset,
                 "_", cmaq_period, ".fst"))
write_fst(rf_perfomance_overall, 
          paste0("RF_holdout_Normalized_perfomance_overall_SiteID_", source.test, "Site#", Site.No, "_", dataset,
                 "_", cmaq_period, ".fst"))

# write_fst(combined_rf_predictions, 
#           paste0("RF_holdout_rf_predictions_SiteID_", source.test, "Site#", Site.No, "_", dataset,
#                  "_", cmaq_period, ".fst"))
# write_fst(combined_var_imp, 
#           paste0("RF_holdout_var_imp_SiteID_", source.test, "Site#", Site.No, "_", dataset,
#                  "_", cmaq_period, ".fst"))
# write_fst(rf_perfomance_overall, 
#           paste0("RF_holdout_perfomance_overall_SiteID_", source.test, "Site#", Site.No, "_", dataset,
#                  "_", cmaq_period, ".fst"))






