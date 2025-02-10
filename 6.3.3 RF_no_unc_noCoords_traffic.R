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

cmaq_period = "2011-01_2011-12"; cmaq_year = 2011
#cmaq_period = "2017-01_2017-11"; cmaq_year = 2017
print(paste0("Study period: ", cmaq_period, " & year ", cmaq_year))

# All date related predictors 
#date_use = read.fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Date_DOW_Holiday_2011-20.fst")
#us_point_coord = read.fst("Long_lat_Mainland_US_0.1_degree.fst")
#dim(us_point_coord)

date_use = read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Date_DOW_Holiday_2011-20.fst")
us_point_coord = read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Long_lat_Mainland_US_0.1_degree.fst")

# date_use = read.fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Date_DOW_Holiday_2011-20.fst")
# us_point_coord = read.fst("Long_lat_Mainland_US_0.1_degree.fst")

# head(date_use)

###### 1.3 Traffic input ######
model_input_ini = read_fst(paste0("Traffic_only_PMF_points_input_", cmaq_period, ".fst"))
model_input_all_grid = read_fst(paste0("Traffic_all_CMAQ_points_input_", cmaq_period, ".fst"))

# model_input_ini = read_fst(
#   paste0("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/var_combined_rds/Traffic_only_PMF_points_input_", cmaq_period, ".fst"))
# model_input_all_grid = read_fst(
#   paste0("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/var_combined_rds/Traffic_all_CMAQ_points_input_", cmaq_period, ".fst"))

# ### Replace census and roadiness for all grids
# # Read updated census and roadiness info and combine them
# commute_census_year =
#   read_fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/US_census/ACS/US_census_tract_grid_01_Commute_2011.fst")
# roadiness_grid_mean =
#   read_fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/Roadiness/Roadiness_in_US_grid_01.fst")
# 
# road_censusCommute_grid =
#   merge(dplyr::select(commute_census_year, -year), 
#         roadiness_grid_mean)
# 
# # Remove previous census and roadiness info
# model_input_ini <- model_input_ini %>% 
#   dplyr::select(Longitude:land_type)
# model_input_all_grid <- model_input_all_grid %>% 
#   dplyr::select(Longitude:land_type)
# 
# # Get coords column
# road_censusCommute_grid$coords = paste(road_censusCommute_grid$Longitude, road_censusCommute_grid$Latitude)
# model_input_all_grid$coords = paste(model_input_all_grid$Longitude, model_input_all_grid$Latitude)
# 
# # Update census and roadiness info
# model_input_ini_new <-
#   merge(model_input_ini, road_censusCommute_grid)
# model_input_all_grid_new <-
#   merge(model_input_all_grid, road_censusCommute_grid, by = "coords")
# 
# model_input_ini_new$coords = model_input_all_grid_new$coords = NULL
# summary(model_input_ini_new); summary(model_input_all_grid_new)
# head(model_input_ini_new); head(model_input_all_grid_new)
# 
# # Output updated file
# write_fst(model_input_ini_new,
#           paste0("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/var_combined_rds/Traffic_only_PMF_points_input_", cmaq_period, ".fst"))
# write_fst(model_input_all_grid_new,
#           paste0("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/CMAQ_Sumaiya/var_combined_rds/Traffic_all_CMAQ_points_input_", cmaq_period, ".fst"))


source.test = "Traffic"

print(paste0("Study period: ", cmaq_period))
print(paste0("Modeled source: ", source.test))
print(paste0("Input data: ", source.test, "-no_Longitude_Latitude"))

print(paste0("Initial site number: ", length(unique(model_input_ini$SiteCode))))

print("Intial input file check")
dim(model_input_ini); dim(model_input_all_grid)
head(model_input_ini); head(model_input_all_grid)

# head(data.frame(table(model_input_ini$Longitude)))
# head(data.frame(table(model_input_ini$Latitude)))

# Filter points within mainland US
model_input_all_grid_us <- 
  merge(model_input_all_grid, us_point_coord, all.y = TRUE)
dim(model_input_all_grid); dim(model_input_all_grid_us)
length(unique(model_input_all_grid_us$Longitude)); length(unique(model_input_all_grid_us$Latitude))
length(unique(model_input_all_grid$Longitude)); length(unique(model_input_all_grid$Latitude))
summary(model_input_all_grid_us)

model_input_all_grid_us = 
  subset(model_input_all_grid_us, is.na(Concentration))
# summary(model_input_all_grid_us)

# Remove the extreme values in 2011.06
# quantile(model_input_ini$PM25_TOT_NRD, 0.9985)

model_input_ini =
  subset(model_input_ini, 
         PM25_TOT_NRD <= quantile(model_input_ini$PM25_TOT_NRD, 0.9985))

# Use the SUM of PM25_TOT_ONR and PM25_TOT_ONR
model_input_ini$PM25_TOT_Traf = 
  model_input_ini$PM25_TOT_NRD + model_input_ini$PM25_TOT_ONR
# summary(model_input_ini); dim(model_input_ini)

# Remove days with extremes in model_input_ini for prediction in model_input_all_grid_us
dim(model_input_all_grid_us)
model_input_all_grid_us =
  subset(model_input_all_grid_us, 
         # Date %in% model_input_ini$Date)
         PM25_TOT_NRD <= max(model_input_ini$PM25_TOT_NRD))

print("-----Check if there are extremes after removing XX quantile data-----")
summary(model_input_ini); dim(model_input_ini)
summary(model_input_all_grid_us); dim(model_input_all_grid_us) # 8972316      25

# Use the SUM of PM25_TOT_ONR and PM25_TOT_ONR
model_input_all_grid_us$PM25_TOT_Traf = 
  model_input_all_grid_us$PM25_TOT_NRD + model_input_all_grid_us$PM25_TOT_ONR
summary(model_input_all_grid_us); dim(model_input_all_grid_us)

# Get PMF_conc & CMAQ_conc for use
model_input_ini <-
  plyr::rename(model_input_ini,
               c("Concentration" = "PMF_conc",
                 "PM25_TOT_Traf" = "CMAQ_conc"))

model_input_all_grid_us <-
  plyr::rename(model_input_all_grid_us,
               c("Concentration" = "PMF_conc",
                 "PM25_TOT_Traf" = "CMAQ_conc"))

# Remove original PM25_TOT_ variables
model_input_ini$PM25_TOT_NRD = model_input_ini$PM25_TOT_ONR = NULL
model_input_all_grid_us$PM25_TOT_NRD = model_input_all_grid_us$PM25_TOT_ONR = NULL

# Get grid_ID
model_input_ini$grid_ID = 
  paste0("grid_", model_input_ini$Longitude, "_", model_input_ini$Latitude)
model_input_ini$grid_ID = as.factor(model_input_ini$grid_ID)

model_input_all_grid_us$grid_ID = 
  paste0("grid_", model_input_all_grid_us$Longitude, "_", model_input_all_grid_us$Latitude)
model_input_all_grid_us$grid_ID = as.factor(model_input_all_grid_us$grid_ID)
print("-----Check how the grid_ID look like, if correct-----")
model_input_all_grid_us$grid_ID[1]; model_input_ini$grid_ID[1]

print("-----Check if there are NAs, and remove if there are-----")
# Check if there is potential NAs, and remove
summary(model_input_all_grid_us); dim(model_input_all_grid_us)
model_input_all_grid_us = subset(model_input_all_grid_us, !is.na(land_type))
dim(model_input_all_grid_us)

print("-----If column names are the same in two input datasets-----")
summary(names(model_input_all_grid_us) %in% names(model_input_ini))
summary(names(model_input_ini) %in% names(model_input_all_grid_us))

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

# Remove extremes in model_input_all_grid_us, applied earlier, just double check
print("-----Last check if there are extreme values in CMAQ_conc-----")
summary(model_input_all_grid_us$CMAQ_conc)
summary(model_input_use$CMAQ_conc)

# model_input_all_grid_us = 
#   subset(model_input_all_grid_us, 
#          CMAQ_conc <= max(model_input_use$CMAQ_conc))
# dim(model_input_all_grid_us)

# Remove column not to use & potential NAs
model_input_use$SiteCode = model_input_all_grid_us$SiteCode = NULL
model_input_use = na.omit(model_input_use)  
model_input_all_grid_us = # still some NAs (1%) in meteorology data
  subset(model_input_all_grid_us, !is.na(th))

# summary(model_input_use); dim(model_input_use)
# summary(model_input_all_grid_us); dim(model_input_all_grid_us)

# Get column order from model_input_use
col_order <- names(model_input_use)

# Reorder model_input_all_grid_us columns
model_input_all_grid_us <- 
  model_input_all_grid_us %>%
  dplyr::select(all_of(col_order))
all(names(model_input_use) == names(model_input_all_grid_us))

# Get the number of unique site input
site_input_count = length(unique(model_input_use$grid_ID))
print(paste0("Number of unique input sites, ", site_input_count))

dim(model_input_use); dim(model_input_all_grid_us)

model_input_use$long = model_input_use$lat = 
  model_input_all_grid_us$long = model_input_all_grid_us$lat = NULL

##### Interpolation for census commute
print("Use mice to interpolate the missing census commute cells/grid")
# library(missForest)
library(mice)

model_input_all_commute_NA = 
  dplyr::select(model_input_all_grid_us,  
                Longitude, Latitude, 
                land_type:commute_time)

# Input should be factor or numeric in missForest
sapply(model_input_all_commute_NA, class)
# model_input_all_commute_NA$Date = as.factor(model_input_all_commute_NA$Date)
model_input_all_commute_NA = 
  model_input_all_commute_NA %>%
  group_by(Longitude, Latitude) %>%
  summarise(
    land_type = first(land_type),
    across(where(is.numeric), mean, na.rm = TRUE)
  ) %>%
  ungroup()
summary(model_input_all_commute_NA); dim(model_input_all_commute_NA)
sapply(model_input_all_commute_NA, class)

# Conduct interpolation
#set.seed(123)
#model_input_all_commute_mf = 
#  missForest(model_input_all_commute_NA, 
#  variablewise = T,
#  mtry = 1, ntree = 50, maxiter = 20, verbose = TRUE)

# Define mice algorithm
mice_meth <- 
  sapply(model_input_all_commute_NA, function(x) {
    if (is.factor(x)) "polyreg" else "pmm"
  })

# mice interpolation
mice_imputed_data <- mice(
  model_input_all_commute_NA,
  m = 5,        
  method = mice_meth,
  maxit = 10,
  printFlag = TRUE
)

# Use the first dataset
model_input_all_commute_mice <-
  complete(mice_imputed_data, 1)
print("Check if there is still NA in census commute data or not")
summary(model_input_all_commute_mice)
dim(model_input_all_commute_mice); dim(model_input_all_commute_NA)

# Extract data that only contains commute variables
model_input_commute_mice <-
  model_input_all_commute_mice %>%
  dplyr::select(Longitude, Latitude, car_truck_van:commute_time)
 
# Replace by the new values
model_input_all_grid_us_1 = model_input_all_grid_us
# model_input_all_grid_us = model_input_all_grid_us_1
common_cols <- setdiff(names(model_input_commute_mice), c("Longitude", "Latitude"))

model_input_all_grid_us_imputed <- 
  model_input_all_grid_us %>%
  left_join(model_input_commute_mice, 
            by = c("Longitude", "Latitude"), 
            suffix = c("", "_imputed")) %>%
  mutate(across(all_of(common_cols), 
                ~ ifelse(is.na(.x), 
                         get(paste0(cur_column(), "_imputed")), 
                         .x))) %>%
  dplyr::select(-ends_with("_imputed"))

print("Check if the unmatched commute_time in model_input_all_grid_us is the same as No. of NAs in model_input_all_grid_us_1")
summary(model_input_all_grid_us_1$commute_time == model_input_all_grid_us_imputed$commute_time)
summary(is.na(model_input_all_grid_us_1$commute_time))

# Output the imputed file
write_fst(model_input_all_grid_us_imputed,
          paste0("Traffic_all_CMAQ_points_input_", cmaq_period, "_commute_mice_interpolated.fst"))
write_fst(model_input_use,
          paste0("Traffic_model_input_use_withPMF_", cmaq_period, ".fst"))

# Assign the imputed one to model_input_all_grid_us
model_input_all_grid_us = model_input_all_grid_us_imputed

print("-----Last check if there are NAs, or unused columns in model inputs-----")
print("-----model_input_use----")
summary(model_input_use)
print("-----model_input_all_grid_us----")
summary(model_input_all_grid_us)

#### 2. Random Forest: Predict without Uncertainty #### 

model.method = "Random_Forest"
print(paste0("Applied statistical model: ", model.method))

#### Use dataset WITHOUT Longitude or Latitude!!!!!
print("-----Use dataset WITHOUT Longitude or Latitude-----")
model_input_use$Longitude = model_input_use$Latitude = 
  model_input_all_grid_us$Longitude = model_input_all_grid_us$Latitude = NULL

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
    # chunk_predictions <- lapply(
    #   grid_chunks,  # Process all chunks
    #   process_chunk
    # )
    
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


# unique_nopmf_sites <- unique(model_input_all_grid_us$grid_ID)
# train_nopmf_sites <- sample(unique_nopmf_sites, size = floor(0.05 * length(unique_nopmf_sites)))
# grid_us_try <- model_input_all_grid_us %>% filter(grid_ID %in% train_nopmf_sites)
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
    dplyr::select(model_input_all_grid_us, -Dataset, -Source_aftermanual, -year), 200)
    # dplyr::select(model_input_all_grid_us, -Longitude, -Latitude, -Dataset, -Source_aftermanual, -year), 100)

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

# grid_id_example = rf_all_predictions$grid_ID[1]
# as.numeric(str_extract(grid_id_example, "-?\\d+\\.\\d+"))
# as.numeric(str_extract(grid_id_example, "\\d+\\.\\d+$"))
rf_all_predictions_coords =
  rf_all_predictions %>%
  dplyr::mutate(
    Longitude = as.numeric(str_extract(grid_ID, "-?\\d+\\.\\d+")),
    Latitude = as.numeric(str_extract(grid_ID, "\\d+\\.\\d+$"))
  )

head(rf_all_predictions_coords); dim(rf_all_predictions_coords)

# Output files
write_fst(rf_all_predictions_coords,
          paste0("RF_overall_No_coords_", source.test, "_", cmaq_period, "_after_interpolation.fst"))
write_fst(rf_with_pmf_predictions,
          paste0("RF_SiteID_Uncertainty_No_coords_", source.test, "_", cmaq_period, "_after_interpolation.fst"))

