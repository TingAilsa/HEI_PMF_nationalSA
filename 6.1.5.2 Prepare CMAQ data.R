# module load gnu10 openmpi r/4.3.1-gnu-openblas gdal/3.4.1-27 udunits geos/3.7.2-gj proj/7.1.0-3w
# module load netcdf-c netcdf-fortran

library(ncdf4)
library(ggrepel)
library(base)
library(dplyr)
library(tidyr)
library(data.table)
library(doParallel)
library(sf)
library(raster)
library(terra)
library(fst)
library(stARS) # raster to stARS, then to sf

# Detect the number of cores available on the server
num_cores <- detectCores() - 1  # Use all but one core
num_cores

# Create a parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Working path
setwd("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds")
getwd()

dir_path = getwd()

####  Function to calculate geometric mean #### 
geom_mean <- function(x) {
  exp(mean(log(x), na.rm = TRUE))
}

#### Function to get geometric mean of 2 values before and 2 after #### 
replace_na_with_geom_mean <- function(dt, col_name) {
  # Make a copy of the original values
  values <- dt[[col_name]]
  n <- length(values)
  
  # Find NA positions
  na_positions <- which(is.na(values))
  
  for (pos in na_positions) {
    # Get indices for 2 values before and 2 after
    before_idx <- max(1, pos-2):max(1, pos-1)
    after_idx <- min(pos+1, n):min(pos+2, n)
    
    # Get the values
    surrounding_values <- values[c(before_idx, after_idx)]
    
    # Calculate geometric mean and replace NA
    if (length(surrounding_values[!is.na(surrounding_values)]) > 0) {
      values[pos] <- geom_mean(surrounding_values)
    }
  }
  
  # Update the data.table
  dt[, (col_name) := values]
}

# Function replacing NAs with overall geometric mean
replace_na_comprehensive <- function(dt, column_name, n = 2) {
  # Work on a copy throughout
  dt_work <- copy(dt)
  
  # First pass: local geometric mean (modify the working copy)
  replace_na_with_geom_mean(dt_work, column_name)
  
  # Second pass: use overall geometric mean for remaining NAs
  remaining_nas <- sum(is.na(dt_work[[column_name]]))
  if (remaining_nas > 0) {
    overall_geo_mean <- geom_mean(dt_work[[column_name]])
    dt_work[is.na(get(column_name)), (column_name) := overall_geo_mean]
    cat("Replaced", remaining_nas, "remaining NAs with overall geometric mean:", overall_geo_mean, "\n")
  }
  
  return(dt_work)  # Return the modified copy
}

# Function replacing NAs only with overall geometric mean
# add a small value to 0
replace_na_with_geomean <- function(x, small_value = 1e-6) {
  # Calculate geometric mean of non-NA values
  non_na_values <- x[!is.na(x)]
  
  # Check if there are any non-NA values
  if(length(non_na_values) == 0) {
    warning("All values are NA, returning original vector")
    return(x)
  }
  
  if(any(non_na_values <= 0)) {
    # Add small value to zeros and negative values
    non_na_values[non_na_values <= 0] <- small_value
    warning("Non-positive values found. Replaced with small positive value for calculation.")
  }
  
  # Calculate geometric mean
  geom_mean <- exp(mean(log(non_na_values)))
  
  # Replace NAs with geometric mean
  x[is.na(x)] <- geom_mean
  
  return(x)
}



# Function to fill NA values with average of 2 days before and 2 days after
fill_na_with_adjacent_days <- function(df, variable_name, na_date) {
  # Convert to data.table for faster processing
  dt <- as.data.table(df)
  
  # Convert Date to proper Date format 
  na_date = as.Date(na_date)
  
  # Create a vector of the dates we want to use for averaging
  date_vector <- c(
    na_date - 2,
    na_date - 1,
    na_date + 1, 
    na_date + 2
  )
  
  # Subset the data to only include the dates we need
  reference_data <- dt[Date %in% date_vector]
  
  # Group by location and calculate average for each location
  variable_sym <- sym(variable_name)
  avg_values <- reference_data %>%
    dplyr::group_by(Longitude, Latitude) %>%
    dplyr::summarize(avg_value = mean(!!variable_sym, na.rm = TRUE),
                     .group = "drop")
  
  # Create a template of NA rows that need to be filled
  na_rows <- dt[Date == na_date]
  na_rows[, eval(variable_name) := NA]
  
  # Join the average values to the NA rows
  na_rows <- merge(na_rows, avg_values, by = c("Longitude", "Latitude"), all.x = TRUE)
  
  # Apply the calculated average value to the original variable
  na_rows[, eval(variable_name) := avg_value]
  na_rows[, avg_value := NULL]
  
  # Remove the NA rows from the original data
  clean_dt <- dt[Date != na_date]
  
  # Combine the datasets
  result_dt <- rbind(clean_dt, na_rows)
  
  # Sort by Date, Longitude, Latitude to restore original order
  setorder(result_dt, Date, Longitude, Latitude)
  
  return(result_dt)
}


# Mainland US coordinates of 0.1 * 0.1 degree
us_point_coord = 
  read.fst("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/pmf_ncld_meteo_census/Long_lat_Mainland_US_0.1_degree.fst")
# us_point_coord = 
#   read.fst("/Users/TingZhang/Dropbox/HEI_PMF_files_Ting/Nation_SA_data/MLresult_RF/Long_lat_Mainland_US_0.1_degree.fst")
# plot(us_point_coord$Longitude, us_point_coord$Latitude)

#### Create grid-like data ####
# Define the approximate bounding box for mainland U.S.
us_bbox <- c(xmin = -125, xmax = -66, ymin = 24, ymax = 50)
crs_proj <- "+proj=longlat +datum=WGS84 +no_defs"

us_grid_raster_01 = raster(file.path("base_raster_grid_sf/us_grid_raster_01.tif"))


#### CMAQ periods & year, for sources ####

# # Set file path and period to process
CMAQ_path = "/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_annual"

# All included years
cmaq_years = 2011:2020

for (cmaq_year in cmaq_years) { # cmaq_year = cmaq_years[1]
  # Get the cmaq_period
  cmaq_period = 
    paste0(cmaq_year, "-01_", cmaq_year, "-12")
  cat("CMAQ_Period", cmaq_period, "& CMAQ_Year", cmaq_year)
  
  ###### Get rds files for each source######
  # List file of the name patterns (study period)
  rds_period =  list.files(CMAQ_path,
                           pattern = paste0(".*", cmaq_period, "\\.rds$"),
                           full.names = TRUE)
  print("CMAQ files to be combined:"); rds_period
  
  
  # NH3
  cmaq_NH3_rds = readRDS(rds_period[grepl("NH3", rds_period, fixed = T)])
  class(cmaq_NH3_rds); head(cmaq_NH3_rds)
  # summary(cmaq_NH3_rds)
  
  # SO2
  cmaq_SO2_rds <- readRDS(rds_period[grepl("SO2", rds_period, fixed = T)])
  # summary(cmaq_SO2_rds)
  
  # NO2
  cmaq_NO2_rds <- readRDS(rds_period[grepl("NO2", rds_period, fixed = T)])
  # summary(cmaq_NO2_rds)
  
  # O3
  cmaq_O3_rds <- readRDS(rds_period[grepl("O3", rds_period, fixed = T)])
  # summary(cmaq_O3_rds)
  
  # EGU, sulfate & industry, Electricity generating units (coal, gas, oil)
  cmaq_EGU_rds <- readRDS(rds_period[grepl("TOT_EGU", rds_period, fixed = T)])
  # summary(cmaq_EGU_rds)
  
  # NRD, traffic, On-road and nonroad diesel
  cmaq_NRD_rds <- readRDS(rds_period[grepl("TOT_NRD", rds_period, fixed = T)])
  # summary(cmaq_NRD_rds)
  
  # ONR, traffic, On-road and nonroad gasoline
  cmaq_ONR_rds <- readRDS(rds_period[grepl("TOT_ONR", rds_period, fixed = T)])
  # summary(cmaq_ONR_rds)
  
  # OTA, sulfate & industry, Non-point emission, NonIPM
  cmaq_OTA_rds <- readRDS(rds_period[grepl("TOT_OTA", rds_period, fixed = T)])
  # summary(cmaq_OTA_rds)
  
  # ACM, traffic, Onroad Mexico, airports, rail, commercial Marine vessels
  cmaq_ACM_rds <- readRDS(rds_period[grepl("TOT_ACM", rds_period, fixed = T)])
  # summary(cmaq_ACM_rds)
  
  # ASEA, salt, Seaspray emissions 
  cmaq_ASEA_rds <- readRDS(rds_period[grepl("TOT_ASEA", rds_period, fixed = T)])
  # summary(cmaq_ASEA_rds)
  
  # ARS, dust, Area fugitive dust, windblown dust, agriculture
  cmaq_ARS_rds <- readRDS(rds_period[grepl("TOT_ARS", rds_period, fixed = T)])
  # summary(cmaq_ARS_rds)
  
  # BIOG, biomass, Biogenic emissions 
  cmaq_BIOG_rds <- readRDS(rds_period[grepl("TOT_BIOG", rds_period, fixed = T)])
  # summary(cmaq_BIOG_rds)
  
  # AFI, biomass, Wildfires, agricultural fires, fire grass, other fires, residential wood combustion
  cmaq_AFI_rds <- readRDS(rds_period[grepl("TOT_AFI", rds_period, fixed = T)])
  # summary(cmaq_AFI_rds)
  
  ###### Handle extremes in rds files ######
  
  ### Set extremes to NA
  ## Remove the extremes, 100 for positive values, -10 for negative values
  cmaq_NH3_rds$NH3[cmaq_NH3_rds$NH3 > 100 | cmaq_NH3_rds$NH3 < -10] = NA
  cmaq_NO2_rds$NO2[cmaq_NO2_rds$NO2 > 100 | cmaq_NO2_rds$NO2 < -10] = NA
  cmaq_SO2_rds$SO2[cmaq_SO2_rds$SO2 > 100 | cmaq_SO2_rds$SO2 < -10] = NA
  cmaq_O3_rds$O3[cmaq_O3_rds$O3 > 100 | cmaq_O3_rds$O3 < -10] = NA

  cmaq_EGU_rds$PM25_TOT_EGU[cmaq_EGU_rds$PM25_TOT_EGU > 100 | cmaq_EGU_rds$PM25_TOT_EGU < -10] = NA
  cmaq_OTA_rds$PM25_TOT_OTA[cmaq_OTA_rds$PM25_TOT_OTA > 100 | cmaq_OTA_rds$PM25_TOT_OTA < -10] = NA
  cmaq_ONR_rds$PM25_TOT_ONR[cmaq_ONR_rds$PM25_TOT_ONR > 100 | cmaq_ONR_rds$PM25_TOT_ONR < -10] = NA
  cmaq_NRD_rds$PM25_TOT_NRD[cmaq_NRD_rds$PM25_TOT_NRD > 100 | cmaq_NRD_rds$PM25_TOT_NRD < -10] = NA
  cmaq_ACM_rds$PM25_TOT_ACM[cmaq_ACM_rds$PM25_TOT_ACM > 100 | cmaq_ACM_rds$PM25_TOT_ACM < -10] = NA
  cmaq_ASEA_rds$PM25_TOT_ASEA[cmaq_ASEA_rds$PM25_TOT_ASEA > 100 | cmaq_ASEA_rds$PM25_TOT_ASEA < -10] = NA
  cmaq_ARS_rds$PM25_TOT_ARS[cmaq_ARS_rds$PM25_TOT_ARS > 100 | cmaq_ARS_rds$PM25_TOT_ARS < -10] = NA
  cmaq_BIOG_rds$PM25_TOT_BIOG[cmaq_BIOG_rds$PM25_TOT_BIOG > 100 | cmaq_BIOG_rds$PM25_TOT_BIOG < -10] = NA
  cmaq_AFI_rds$PM25_TOT_AFI[cmaq_AFI_rds$PM25_TOT_AFI > 100 | cmaq_AFI_rds$PM25_TOT_AFI < -10] = NA
  
  # summary(cmaq_EGU_rds)
  
  # cmaq_EGU_rds$EGU = NULL
  # cmaq_OTA_rds$OTA = NULL
  # cmaq_ONR_rds$ONR = NULL
  # cmaq_NRD_rds$NRD = NULL
  # cmaq_ACM_rds$ACM = NULL
  # cmaq_ASEA_rds$ASEA = NULL
  # cmaq_ARS_rds$ARS = NULL
  # cmaq_BIOG_rds$BIOG = NULL
  # cmaq_AFI_rds$AFI = NULL
  
  ### Create copies and apply geometric mean replacement
  # Interpolate, geometric mean of 2 values before and after, use "copy" to for data.table assignment!!!!!
  # !!! for data.table, if using "=" with assignment like dt2 = dt1, dt2 point to the same data as dt1.
  # When modifying dt2, dt1 also change because they are the same object!!!
  
  # Define the datasets and their corresponding column names
  cmaq_datasets <- list(
    cmaq_NH3_rds = "NH3",
    cmaq_NO2_rds = "NO2", 
    cmaq_SO2_rds = "SO2",
    cmaq_O3_rds = "O3",
    cmaq_EGU_rds = "PM25_TOT_EGU",
    cmaq_OTA_rds = "PM25_TOT_OTA",
    cmaq_ONR_rds = "PM25_TOT_ONR",
    cmaq_NRD_rds = "PM25_TOT_NRD",
    cmaq_ACM_rds = "PM25_TOT_ACM",
    cmaq_ASEA_rds = "PM25_TOT_ASEA",
    cmaq_ARS_rds = "PM25_TOT_ARS",
    cmaq_BIOG_rds = "PM25_TOT_BIOG",
    cmaq_AFI_rds = "PM25_TOT_AFI"
  )
  
  # Apply to each dataset, make a copy for new dt
  for (cmaq_var in names(cmaq_datasets)) {
    column_name <- cmaq_datasets[[cmaq_var]]
    new_name <- paste0(cmaq_var, "_noExe")
    
    # Create copy and apply function
    assign(new_name, replace_na_comprehensive(get(cmaq_var), column_name))
  }
  
  summary(cmaq_NH3_rds_noExe); summary(cmaq_EGU_rds_noExe)
  
  ###### Merge rds for each source & add crs ######
  # Sort each dataset
  cmaq_NH3_rds_noExe <- cmaq_NH3_rds_noExe[order(Date, x, y)]
  cmaq_NO2_rds_noExe <- cmaq_NO2_rds_noExe[order(Date, x, y)]
  cmaq_SO2_rds_noExe <- cmaq_SO2_rds_noExe[order(Date, x, y)]
  cmaq_O3_rds_noExe <- cmaq_O3_rds_noExe[order(Date, x, y)]
  
  cmaq_EGU_rds_noExe <- cmaq_EGU_rds_noExe[order(Date, x, y)]
  cmaq_OTA_rds_noExe <- cmaq_OTA_rds_noExe[order(Date, x, y)]
  cmaq_ONR_rds_noExe <- cmaq_ONR_rds_noExe[order(Date, x, y)]
  cmaq_NRD_rds_noExe <- cmaq_NRD_rds_noExe[order(Date, x, y)]
  cmaq_ACM_rds_noExe <- cmaq_ACM_rds_noExe[order(Date, x, y)]
  
  cmaq_ASEA_rds_noExe <- cmaq_ASEA_rds_noExe[order(Date, x, y)]
  cmaq_ARS_rds_noExe <- cmaq_ARS_rds_noExe[order(Date, x, y)]

  cmaq_BIOG_rds_noExe <- cmaq_BIOG_rds_noExe[order(Date, x, y)]
  cmaq_AFI_rds_noExe <- cmaq_AFI_rds_noExe[order(Date, x, y)]
  
  # Verify they're aligned (optional but recommended)
  sort_cols <- c("Date", "x", "y")  # or whatever your coordinate columns are named
  
  all.equal(cmaq_NH3_rds_noExe[, ..sort_cols], cmaq_EGU_rds_noExe[, ..sort_cols])
  all.equal(cmaq_BIOG_rds_noExe[, ..sort_cols], cmaq_ONR_rds_noExe[, ..sort_cols])
  
  # Use cbind to get source specific dataset
  ## Sulfate
  cmaq_sulfate_rds = cbind(
    cmaq_EGU_rds_noExe,
    cmaq_OTA_rds_noExe[, .(PM25_TOT_OTA)],
    cmaq_NH3_rds_noExe[, .(NH3)],
    cmaq_SO2_rds_noExe[, .(SO2)],
    cmaq_O3_rds_noExe[, .(O3)]
  )
  head(cmaq_sulfate_rds); summary(cmaq_sulfate_rds)
  
  ## Traffic
  cmaq_traffic_rds = cbind(
    cmaq_NRD_rds_noExe,
    cmaq_ONR_rds_noExe[, .(PM25_TOT_ONR)],
    cmaq_ACM_rds_noExe[, .(PM25_TOT_ACM)],
    cmaq_NO2_rds_noExe[, .(NO2)],
    cmaq_O3_rds_noExe[, .(O3)]
  )
  head(cmaq_traffic_rds); summary(cmaq_traffic_rds)
  
  ## Dust
  cmaq_dust_rds = cmaq_ARS_rds_noExe
  head(cmaq_dust_rds); summary(cmaq_dust_rds)
  
  ## Biomass
  cmaq_biom_rds = cbind(
    cmaq_AFI_rds_noExe,
    cmaq_BIOG_rds_noExe[, .(PM25_TOT_BIOG)]
  )
  head(cmaq_biom_rds); summary(cmaq_biom_rds)
  
  ## Nitrate
  cmaq_nitrate_rds = cbind(
    # related to sulfate 
    cmaq_sulfate_rds,
    
    # related to traffic
    cmaq_NRD_rds_noExe[, .(PM25_TOT_NRD)],
    cmaq_ONR_rds_noExe[, .(PM25_TOT_ONR)],
    cmaq_ACM_rds_noExe[, .(PM25_TOT_ACM)],
    cmaq_NO2_rds_noExe[, .(NO2)],
    
    # related to biomass
    cmaq_AFI_rds_noExe[, .(PM25_TOT_AFI)],
    cmaq_BIOG_rds_noExe[, .(PM25_TOT_BIOG)]
  )
  head(cmaq_nitrate_rds); summary(cmaq_nitrate_rds)
  
  # Rename the columns
  names(cmaq_sulfate_rds)[1:2] = c("Longitude", "Latitude")
  names(cmaq_traffic_rds)[1:2] = c("Longitude", "Latitude")
  names(cmaq_dust_rds)[1:2] = c("Longitude", "Latitude")
  names(cmaq_biom_rds)[1:2] = c("Longitude", "Latitude")
  names(cmaq_nitrate_rds)[1:2] = c("Longitude", "Latitude")
  
 # Output CMAQ files for use
  write_fst(cmaq_sulfate_rds,
            file.path(paste0("base_raster_grid_sf/CMAQ_Sulfate_", cmaq_period, ".fst")))
 
  write_fst(cmaq_nitrate_rds,
            file.path(paste0("base_raster_grid_sf/CMAQ_Nitrate_", cmaq_period, ".fst")))
  
  write_fst(cmaq_dust_rds,
            file.path(paste0("base_raster_grid_sf/CMAQ_Dust_", cmaq_period, ".fst")))
  
  write_fst(cmaq_traffic_rds,
            file.path(paste0("base_raster_grid_sf/CMAQ_Traffic_", cmaq_period, ".fst")))
 
  write_fst(cmaq_biom_rds,
            file.path(paste0("base_raster_grid_sf/CMAQ_Biomass_", cmaq_period, ".fst")))
  
  ###### CMAQ variable combine into annual ######
  
  # List CMAQ rds files for each sector
  rds_list =
    c("cmaq_EGU_rds_noExe", "cmaq_OTA_rds_noExe",
      "cmaq_ONR_rds_noExe", "cmaq_NRD_rds_noExe",  "cmaq_ACM_rds_noExe",
      "cmaq_ASEA_rds_noExe", "cmaq_ARS_rds_noExe",
      "cmaq_BIOG_rds_noExe", "cmaq_AFI_rds_noExe",
      "cmaq_O3_rds_noExe", "cmaq_NH3_rds_noExe", "cmaq_SO2_rds_noExe", "cmaq_NO2_rds_noExe")
  
  # Corresponding variable names
  variables <- c(
    "EGU", "OTA",
    "ONR", "NRD", "ACM",
    "ASEA", "ARS", 
    "BIOG", "AFI",
    "O3", "NH3", "SO2", "NO2")
  
  # Evaluate the character names to actual data.tables
  rds_tables <- lapply(rds_list, function(name) get(name))
  
  # Process each data.table
  for (i in seq_along(rds_tables)) {
    unique_col <- setdiff(names(rds_tables[[i]]), c("x", "y", "Date"))
    setnames(rds_tables[[i]], unique_col, "cmaq_value")  # Rename the unique column
    rds_tables[[i]][, cmaq_variable := variables[i]]    # Add cmaq_variable column
  }
  
  # Combine all data.tables into one
  cmaq_rds_noExe_all <- data.table::rbindlist(rds_tables, use.names = TRUE, fill = TRUE)
  
  names(cmaq_rds_noExe_all)[1:2]
  dim(cmaq_rds_noExe_all)
  names(cmaq_rds_noExe_all)[1:2] = c("Longitude", "Latitude")
  
  # Write the combined file
  write_fst(cmaq_rds_noExe_all,
            file.path("/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_annual",
                      paste0("CMAQ_all_sectors_", cmaq_period, ".fst")))
  
  rm(rds_tables)
  rm(cmaq_rds_noExe_all)
  gc()
  
  
  ###### CMAQ variable plotting ######
  library(USAboundaries)
  us_states = USAboundaries::us_states()
  us_states <- us_states[!(us_states$state_abbr %in% c( 'HI', 'AK', "AS", "GU", "MP", "PR", "VI")),]
  
  #### NH3
  quantile(cmaq_NH3_rds_noExe$cmaq_value, 0.995)
  
  cmaq_NH3_rds_noExe_use =
    subset(cmaq_NH3_rds_noExe,
           cmaq_value < quantile(cmaq_NH3_rds_noExe$cmaq_value, 0.995) &
             cmaq_value > quantile(cmaq_NH3_rds_noExe$cmaq_value, 0.005)) %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarise(cmaq_mean = mean(cmaq_value),
                     cmaq_median = median(cmaq_value))
  head(cmaq_NH3_rds_noExe_use); dim(cmaq_NH3_rds_noExe_use)
  
  cmaq_NH3_rds_noExe_plot <-
    ggplot() +
    geom_point(data = cmaq_NH3_rds_noExe_use,
               aes(x = x, y = y, color = cmaq_median),
               size = 0.35, alpha = 0.8) +
    geom_sf(data = us_states,
            fill = NA, color = "grey70", size = 0.3) +
    scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
    theme_minimal(base_size = 16) +
    labs(x = "x",
         y = "y",
         title = paste0("Median CMAQ contribution in ", cmaq_year)) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 19),
      plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
      # plot.subtitle = element_text(size = 22),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 19)
    )
  # cmaq_NH3_rds_noExe_plot
  
  NH3_name = paste0("CMAQ_source_map_NH3_", cmaq_year, ".pdf")
  ggsave(
    file.path("machine_learning_source_input/ML_plot", NH3_name),
    plot = cmaq_NH3_rds_noExe_plot, width = 14.5, height = 8.5)
  
  #### SO2
  quantile(cmaq_SO2_rds_noExe$cmaq_value, 0.995)
  
  cmaq_SO2_rds_noExe_use =
    subset(cmaq_SO2_rds_noExe,
           cmaq_value < quantile(cmaq_SO2_rds_noExe$cmaq_value, 0.995) &
             cmaq_value > quantile(cmaq_SO2_rds_noExe$cmaq_value, 0.005)) %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarise(cmaq_mean = mean(cmaq_value),
                     cmaq_median = median(cmaq_value))
  head(cmaq_SO2_rds_noExe_use); dim(cmaq_SO2_rds_noExe_use)
  
  cmaq_SO2_rds_noExe_plot <-
    ggplot() +
    geom_point(data = cmaq_SO2_rds_noExe_use,
               aes(x = x, y = y, color = cmaq_median),
               size = 0.35, alpha = 0.8) +
    geom_sf(data = us_states,
            fill = NA, color = "grey70", size = 0.3) +
    scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
    theme_minimal(base_size = 16) +
    labs(x = "x",
         y = "y",
         title = paste0("Median CMAQ contribution in ", cmaq_year)) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 19),
      plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
      # plot.subtitle = element_text(size = 22),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 19)
    )
  # cmaq_SO2_rds_noExe_plot
  
  SO2_name = paste0("CMAQ_source_map_SO2_", cmaq_year, ".pdf")
  ggsave(
    file.path("machine_learning_source_input/ML_plot", SO2_name),
    plot = cmaq_SO2_rds_noExe_plot, width = 14.5, height = 8.5)
  
  
  #### NO2
  quantile(cmaq_NO2_rds_noExe$cmaq_value, 0.995)
  
  cmaq_NO2_rds_noExe_use =
    subset(cmaq_NO2_rds_noExe,
           cmaq_value < quantile(cmaq_NO2_rds_noExe$cmaq_value, 0.995) &
             cmaq_value > quantile(cmaq_NO2_rds_noExe$cmaq_value, 0.005)) %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarise(cmaq_mean = mean(cmaq_value),
                     cmaq_median = median(cmaq_value))
  head(cmaq_NO2_rds_noExe_use); dim(cmaq_NO2_rds_noExe_use)
  
  cmaq_NO2_rds_noExe_plot <-
    ggplot() +
    geom_point(data = cmaq_NO2_rds_noExe_use,
               aes(x = x, y = y, color = cmaq_median),
               size = 0.35, alpha = 0.8) +
    geom_sf(data = us_states,
            fill = NA, color = "grey70", size = 0.3) +
    scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
    theme_minimal(base_size = 16) +
    labs(x = "x",
         y = "y",
         title = paste0("Median CMAQ contribution in ", cmaq_year)) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 19),
      plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
      # plot.subtitle = element_text(size = 22),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 19)
    )
  # cmaq_NO2_rds_noExe_plot
  
  NO2_name = paste0("CMAQ_source_map_NO2_", cmaq_year, ".pdf")
  ggsave(
    file.path("machine_learning_source_input/ML_plot", NO2_name),
    plot = cmaq_NO2_rds_noExe_plot, width = 14.5, height = 8.5)
  
  
  #### O3
  quantile(cmaq_O3_rds_noExe$cmaq_value, 0.995)
  
  cmaq_O3_rds_noExe_use =
    subset(cmaq_O3_rds_noExe,
           cmaq_value < quantile(cmaq_O3_rds_noExe$cmaq_value, 0.995) &
             cmaq_value > quantile(cmaq_O3_rds_noExe$cmaq_value, 0.005)) %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarise(cmaq_mean = mean(cmaq_value),
                     cmaq_median = median(cmaq_value))
  head(cmaq_O3_rds_noExe_use); dim(cmaq_O3_rds_noExe_use)
  
  cmaq_O3_rds_noExe_plot <-
    ggplot() +
    geom_point(data = cmaq_O3_rds_noExe_use,
               aes(x = x, y = y, color = cmaq_median),
               size = 0.35, alpha = 0.8) +
    geom_sf(data = us_states,
            fill = NA, color = "grey70", size = 0.3) +
    scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
    theme_minimal(base_size = 16) +
    labs(x = "x",
         y = "y",
         title = paste0("Median CMAQ contribution in ", cmaq_year)) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 19),
      plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
      # plot.subtitle = element_text(size = 22),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 19)
    )
  # cmaq_O3_rds_noExe_plot
  
  O3_name = paste0("CMAQ_source_map_O3_", cmaq_year, ".pdf")
  ggsave(
    file.path("machine_learning_source_input/ML_plot", O3_name),
    plot = cmaq_O3_rds_noExe_plot, width = 14.5, height = 8.5)
  
  
  #### DUST, ARS
  quantile(cmaq_ARS_rds_noExe$cmaq_value, 0.995)
  
  cmaq_ARS_rds_noExe_use =
    subset(cmaq_ARS_rds_noExe,
           cmaq_value < quantile(cmaq_ARS_rds_noExe$cmaq_value, 0.995) &
             cmaq_value > quantile(cmaq_ARS_rds_noExe$cmaq_value, 0.005)) %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarise(cmaq_mean = mean(cmaq_value),
                     cmaq_median = median(cmaq_value))
  head(cmaq_ARS_rds_noExe_use); dim(cmaq_ARS_rds_noExe_use)
  
  cmaq_ARS_rds_noExe_plot <-
    ggplot() +
    geom_point(data = cmaq_ARS_rds_noExe_use,
               aes(x = x, y = y, color = cmaq_median),
               size = 0.35, alpha = 0.8) +
    geom_sf(data = us_states,
            fill = NA, color = "grey70", size = 0.3) +
    scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
    theme_minimal(base_size = 16) +
    labs(x = "x",
         y = "y",
         title = paste0("Median CMAQ contribution in ", cmaq_year)) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 19),
      plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
      # plot.subtitle = element_text(size = 22),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 19)
    )
  # cmaq_ARS_rds_noExe_plot
  
  ARS_name = paste0("CMAQ_source_map_ARS_", cmaq_year, ".pdf")
  ggsave(
    file.path("machine_learning_source_input/ML_plot", ARS_name),
    plot = cmaq_ARS_rds_noExe_plot, width = 14.5, height = 8.5)
  
  
  #### EGU
  quantile(cmaq_EGU_rds_noExe$cmaq_value, 0.995)
  
  cmaq_EGU_rds_noExe_use =
    subset(cmaq_EGU_rds_noExe,
           cmaq_value < quantile(cmaq_EGU_rds_noExe$cmaq_value, 0.995) &
             cmaq_value > quantile(cmaq_EGU_rds_noExe$cmaq_value, 0.005)) %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarise(cmaq_mean = mean(cmaq_value),
                     cmaq_median = median(cmaq_value))
  head(cmaq_EGU_rds_noExe_use); dim(cmaq_EGU_rds_noExe_use)
  
  cmaq_EGU_rds_noExe_plot <-
    ggplot() +
    geom_point(data = cmaq_EGU_rds_noExe_use,
               aes(x = x, y = y, color = cmaq_median),
               size = 0.35, alpha = 0.8) +
    geom_sf(data = us_states,
            fill = NA, color = "grey70", size = 0.3) +
    scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
    theme_minimal(base_size = 16) +
    labs(x = "x",
         y = "y",
         title = paste0("Median CMAQ contribution in ", cmaq_year)) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 19),
      plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
      # plot.subtitle = element_text(size = 22),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 19)
    )
  # cmaq_EGU_rds_noExe_plot
  
  EGU_name = paste0("CMAQ_source_map_EGU_", cmaq_year, ".pdf")
  ggsave(
    file.path("machine_learning_source_input/ML_plot", EGU_name),
    plot = cmaq_EGU_rds_noExe_plot, width = 14.5, height = 8.5)
  
  
  #### NRD
  quantile(cmaq_NRD_rds_noExe$cmaq_value, 0.995)
  
  cmaq_NRD_rds_noExe_use =
    subset(cmaq_NRD_rds_noExe,
           cmaq_value < quantile(cmaq_NRD_rds_noExe$cmaq_value, 0.995) &
             cmaq_value > quantile(cmaq_NRD_rds_noExe$cmaq_value, 0.005)) %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarise(cmaq_mean = mean(cmaq_value),
                     cmaq_median = median(cmaq_value))
  head(cmaq_NRD_rds_noExe_use); dim(cmaq_NRD_rds_noExe_use)
  
  cmaq_NRD_rds_noExe_plot <-
    ggplot() +
    geom_point(data = cmaq_NRD_rds_noExe_use,
               aes(x = x, y = y, color = cmaq_median),
               size = 0.35, alpha = 0.8) +
    geom_sf(data = us_states,
            fill = NA, color = "grey70", size = 0.3) +
    scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
    theme_minimal(base_size = 16) +
    labs(x = "x",
         y = "y",
         title = paste0("Median CMAQ contribution in ", cmaq_year)) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 19),
      plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
      # plot.subtitle = element_text(size = 22),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 19)
    )
  # cmaq_NRD_rds_noExe_plot
  
  NRD_name = paste0("CMAQ_source_map_NRD_", cmaq_year, ".pdf")
  ggsave(
    file.path("machine_learning_source_input/ML_plot", NRD_name),
    plot = cmaq_NRD_rds_noExe_plot, width = 14.5, height = 8.5)
  
  
  #### ONR
  quantile(cmaq_ONR_rds_noExe$cmaq_value, 0.995)
  
  cmaq_ONR_rds_noExe_use =
    subset(cmaq_ONR_rds_noExe,
           cmaq_value < quantile(cmaq_ONR_rds_noExe$cmaq_value, 0.995) &
             cmaq_value > quantile(cmaq_ONR_rds_noExe$cmaq_value, 0.005)) %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarise(cmaq_mean = mean(cmaq_value),
                     cmaq_median = median(cmaq_value))
  head(cmaq_ONR_rds_noExe_use); dim(cmaq_ONR_rds_noExe_use)
  
  cmaq_ONR_rds_noExe_plot <-
    ggplot() +
    geom_point(data = cmaq_ONR_rds_noExe_use,
               aes(x = x, y = y, color = cmaq_median),
               size = 0.35, alpha = 0.8) +
    geom_sf(data = us_states,
            fill = NA, color = "grey70", size = 0.3) +
    scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
    theme_minimal(base_size = 16) +
    labs(x = "x",
         y = "y",
         title = paste0("Median CMAQ contribution in ", cmaq_year)) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 19),
      plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
      # plot.subtitle = element_text(size = 22),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 19)
    )
  # cmaq_ONR_rds_noExe_plot
  
  ONR_name = paste0("CMAQ_source_map_ONR_", cmaq_year, ".pdf")
  ggsave(
    file.path("machine_learning_source_input/ML_plot", ONR_name),
    plot = cmaq_ONR_rds_noExe_plot, width = 14.5, height = 8.5)
  
  
  #### OTA
  quantile(cmaq_OTA_rds_noExe$cmaq_value, 0.995)
  
  cmaq_OTA_rds_noExe_use =
    subset(cmaq_OTA_rds_noExe,
           cmaq_value < quantile(cmaq_OTA_rds_noExe$cmaq_value, 0.995) &
             cmaq_value > quantile(cmaq_OTA_rds_noExe$cmaq_value, 0.005)) %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarise(cmaq_mean = mean(cmaq_value),
                     cmaq_median = median(cmaq_value))
  head(cmaq_OTA_rds_noExe_use); dim(cmaq_OTA_rds_noExe_use)
  
  cmaq_OTA_rds_noExe_plot <-
    ggplot() +
    geom_point(data = cmaq_OTA_rds_noExe_use,
               aes(x = x, y = y, color = cmaq_median),
               size = 0.35, alpha = 0.8) +
    geom_sf(data = us_states,
            fill = NA, color = "grey70", size = 0.3) +
    scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
    theme_minimal(base_size = 16) +
    labs(x = "x",
         y = "y",
         title = paste0("Median CMAQ contribution in ", cmaq_year)) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 19),
      plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
      # plot.subtitle = element_text(size = 22),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 19)
    )
  # cmaq_OTA_rds_noExe_plot
  
  OTA_name = paste0("CMAQ_source_map_OTA_", cmaq_year, ".pdf")
  ggsave(
    file.path("machine_learning_source_input/ML_plot", OTA_name),
    plot = cmaq_OTA_rds_noExe_plot, width = 14.5, height = 8.5)
  
  
  #### ACM
  quantile(cmaq_ACM_rds_noExe$cmaq_value, 0.995)
  
  cmaq_ACM_rds_noExe_use =
    subset(cmaq_ACM_rds_noExe,
           cmaq_value < quantile(cmaq_ACM_rds_noExe$cmaq_value, 0.995) &
             cmaq_value > quantile(cmaq_ACM_rds_noExe$cmaq_value, 0.005)) %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarise(cmaq_mean = mean(cmaq_value),
                     cmaq_median = median(cmaq_value))
  head(cmaq_ACM_rds_noExe_use); dim(cmaq_ACM_rds_noExe_use)
  
  cmaq_ACM_rds_noExe_plot <-
    ggplot() +
    geom_point(data = cmaq_ACM_rds_noExe_use,
               aes(x = x, y = y, color = cmaq_median),
               size = 0.35, alpha = 0.8) +
    geom_sf(data = us_states,
            fill = NA, color = "grey70", size = 0.3) +
    scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
    theme_minimal(base_size = 16) +
    labs(x = "x",
         y = "y",
         title = paste0("Median CMAQ contribution in ", cmaq_year)) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 19),
      plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
      # plot.subtitle = element_text(size = 22),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 19)
    )
  # cmaq_ACM_rds_noExe_plot
  
  ACM_name = paste0("CMAQ_source_map_ACM_", cmaq_year, ".pdf")
  ggsave(
    file.path("machine_learning_source_input/ML_plot", ACM_name),
    plot = cmaq_ACM_rds_noExe_plot, width = 14.5, height = 8.5)
  
  
  #### ASEA
  quantile(cmaq_ASEA_rds_noExe$cmaq_value, 0.995)
  
  cmaq_ASEA_rds_noExe_use =
    subset(cmaq_ASEA_rds_noExe,
           cmaq_value < quantile(cmaq_ASEA_rds_noExe$cmaq_value, 0.995) &
             cmaq_value > quantile(cmaq_ASEA_rds_noExe$cmaq_value, 0.005)) %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarise(cmaq_mean = mean(cmaq_value),
                     cmaq_median = median(cmaq_value))
  head(cmaq_ASEA_rds_noExe_use); dim(cmaq_ASEA_rds_noExe_use)
  
  cmaq_ASEA_rds_noExe_plot <-
    ggplot() +
    geom_point(data = cmaq_ASEA_rds_noExe_use,
               aes(x = x, y = y, color = cmaq_median),
               size = 0.35, alpha = 0.8) +
    geom_sf(data = us_states,
            fill = NA, color = "grey70", size = 0.3) +
    scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
    theme_minimal(base_size = 16) +
    labs(x = "x",
         y = "y",
         title = paste0("Median CMAQ contribution in ", cmaq_year)) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 19),
      plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
      # plot.subtitle = element_text(size = 22),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 19)
    )
  # cmaq_ASEA_rds_noExe_plot
  
  ASEA_name = paste0("CMAQ_source_map_ASEA_", cmaq_year, ".pdf")
  ggsave(
    file.path("machine_learning_source_input/ML_plot", ASEA_name),
    plot = cmaq_ASEA_rds_noExe_plot, width = 14.5, height = 8.5)
  
  
  #### BIOG
  quantile(cmaq_BIOG_rds_noExe$cmaq_value, 0.995)
  
  cmaq_BIOG_rds_noExe_use =
    subset(cmaq_BIOG_rds_noExe,
           cmaq_value < quantile(cmaq_BIOG_rds_noExe$cmaq_value, 0.995) &
             cmaq_value > quantile(cmaq_BIOG_rds_noExe$cmaq_value, 0.005)) %>%
    dplyr::group_by(x, y) %>%
    dplyr::summarise(cmaq_mean = mean(cmaq_value),
                     cmaq_median = median(cmaq_value))
  head(cmaq_BIOG_rds_noExe_use); dim(cmaq_BIOG_rds_noExe_use)
  
  cmaq_BIOG_rds_noExe_plot <-
    ggplot() +
    geom_point(data = cmaq_BIOG_rds_noExe_use,
               aes(x = x, y = y, color = cmaq_median),
               size = 0.35, alpha = 0.8) +
    geom_sf(data = us_states,
            fill = NA, color = "grey70", size = 0.3) +
    scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
    theme_minimal(base_size = 16) +
    labs(x = "x",
         y = "y",
         title = paste0("Median CMAQ contribution in ", cmaq_year)) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 19),
      plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
      # plot.subtitle = element_text(size = 22),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 19)
    )
  # cmaq_BIOG_rds_noExe_plot
  
  BIOG_name = paste0("CMAQ_source_map_BIOG_", cmaq_year, ".pdf")
  ggsave(
    file.path("machine_learning_source_input/ML_plot", BIOG_name),
    plot = cmaq_BIOG_rds_noExe_plot, width = 14.5, height = 8.5)
  
  
  #### AFI
  quantile(cmaq_AFI_rds_noExe$cmaq_value, 0.995)
  
  cmaq_AFI_rds_noExe_use =
    subset(cmaq_AFI_rds_noExe,
           cmaq_value < quantile(cmaq_AFI_rds_noExe$cmaq_value, 0.995) &
             cmaq_value > quantile(cmaq_AFI_rds_noExe$cmaq_value, 0.005)) %>%
    group_by(x, y) %>%
    dplyr::summarise(cmaq_mean = mean(cmaq_value),
                     cmaq_median = median(cmaq_value))
  head(cmaq_AFI_rds_noExe_use); dim(cmaq_AFI_rds_noExe_use)
  
  cmaq_AFI_rds_noExe_plot <-
    ggplot() +
    geom_point(data = cmaq_AFI_rds_noExe_use,
               aes(x = x, y = y, color = cmaq_median),
               size = 0.35, alpha = 0.8) +
    geom_sf(data = us_states,
            fill = NA, color = "grey70", size = 0.3) +
    scale_color_viridis_c(name = "Concentration µg/m^3", option = "plasma") +
    coord_sf(xlim = c(-130, -65), ylim = c(24, 50), expand = FALSE) +
    theme_minimal(base_size = 16) +
    labs(x = "x",
         y = "y",
         title = paste0("Median CMAQ contribution in ", cmaq_year)) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 19),
      plot.title = element_text(size = 22, face = "bold", vjust = 1.2),
      # plot.subtitle = element_text(size = 22),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 19)
    )
  # cmaq_AFI_rds_noExe_plot
  
  AFI_name = paste0("CMAQ_source_map_AFI_", cmaq_year, ".pdf")
  ggsave(
    file.path("machine_learning_source_input/ML_plot", AFI_name),
    plot = cmaq_AFI_rds_noExe_plot, width = 14.5, height = 8.5)
  
}

#### CMAQ periods & year, PM2.5 ####

# # Set file path and period to process
CMAQ_path = "/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_annual"

# All included years
cmaq_years = 2012:2020

for (cmaq_year in cmaq_years) { # cmaq_year = cmaq_years[1]
  # Get the cmaq_period
  cmaq_period = 
    paste0(cmaq_year, "-01_", cmaq_year, "-12")
  cat("CMAQ_Period", cmaq_period, "& CMAQ_Year", cmaq_year, "\n")
  
  ###### Get rds files for each source ######
  # List file of the name patterns (study period)
  rds_pm_file =  list.files(CMAQ_path,
                           pattern = paste0("PM_TOT_cmaq_", cmaq_period, "\\.rds$"),
                           full.names = TRUE)
  print("CMAQ PM2.5 files to be combined:"); rds_pm_file
  
  # Total PM2.5 
  cmaq_PM25_rds <- readRDS(rds_pm_file)
  # summary(cmaq_PM25_rds)

  ## Remove the extremes, 200 for positive values, -10 for negative values
  cmaq_PM25_rds$PM_TOT[cmaq_PM25_rds$PM_TOT > 200 | cmaq_PM25_rds$PM_TOT < -10] = NA

  ### Create copies and apply geometric mean replacement
  # Interpolate, geometric mean of 2 values before and after, use "copy" to for data.table assignment!!!!!
  # !!! for data.table, if using "=" with assignment like dt2 = dt1, dt2 point to the same data as dt1.
  # When modifying dt2, dt1 also change because they are the same object!!!
  # Define the datasets and their corresponding column names
  cmaq_datasets <- list(
    cmaq_PM25_rds = "PM_TOT"
  )
  
  # Apply to each dataset, make a copy for new dt
  for (cmaq_var in names(cmaq_datasets)) {
    column_name <- cmaq_datasets[[cmaq_var]]
    new_name <- paste0(cmaq_var, "_noExe")
    
    # Create copy and apply function
    assign(new_name, replace_na_comprehensive(get(cmaq_var), column_name))
  }
  
  # Sort each dataset
  cmaq_PM25_rds_noExe <- cmaq_PM25_rds_noExe[order(Date, x, y)]
  summary(cmaq_PM25_rds_noExe)
  
  # Change colname
  cmaq_PM25_rds_noExe =
    plyr::rename(
      cmaq_PM25_rds_noExe,
      c("x" = "Longitude",
        "y" = "Latitude")
    )
  head(cmaq_PM25_rds_noExe)
  
  ###### Merge rds for OTHER sources ######
  # The combined file for all other CMAQ variables
  cmaq_rds_noExe_all = 
    read_fst(
      file.path(
        "/scratch/tzhang23/cmaq_sumaiya/var_combined_rds/cmaq_combined_annual",
        paste0("CMAQ_all_sectors_", cmaq_period, ".fst")))
  
  # pivot_wider for combine
  cmaq_rds_noExe_wide =
    cmaq_rds_noExe_all %>%
    pivot_wider(
      names_from = cmaq_variable,
      values_from = cmaq_value
    )
  
  # Check if data points match
  # summary(cmaq_rds_noExe_wide$Date == cmaq_PM25_rds_noExe$Date)
  # summary(cmaq_rds_noExe_wide$Longitude == cmaq_PM25_rds_noExe$Longitude)
  # summary(cmaq_rds_noExe_wide$Latitude == cmaq_PM25_rds_noExe$Latitude)
  
  cmaq_PM25_rds_noExe_final =
    cbind(cmaq_PM25_rds_noExe, 
          dplyr::select(cmaq_rds_noExe_wide, 
                        -Date, -Longitude, -Latitude))
  # head(cmaq_PM25_rds_noExe_final)
  # summary(cmaq_PM25_rds_noExe_final)
  
  write_fst(cmaq_PM25_rds_noExe_final,
            file.path(paste0("base_raster_grid_sf/CMAQ_PM25_", cmaq_period, ".fst")))
}

cmaq_year = 2017
cmaq_period = 
  paste0(cmaq_year, "-01_", cmaq_year, "-12")
cmaq_PM25_rds_noExe_final = 
  read_fst(file.path(paste0("base_raster_grid_sf/CMAQ_PM25_", cmaq_period, ".fst")))
summary(cmaq_PM25_rds_noExe_final)
